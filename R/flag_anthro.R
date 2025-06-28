#' @title Flag Implausible Anthropometric Values (with LMS z-score or limits depending on age)
#'
#' @description
#' Computes LMS-based z-scores and flags biologically implausible anthropometric values (weight, height, or BMI)
#' for individuals across the full age spectrum, using CDC growth charts for those ≤20 years.
#'
#' @param data A data frame with sex, age (in years), and anthropometric variable(s).
#' @param variable Variable to evaluate (must contain 'weight', 'height', or 'bmi').
#' @param patient_id_col Column name for patient ID.
#' @param sex_col Column name for sex.
#' @param age_col Column name for age in years.
#'
#' @return Data frame with z-score and converted percentile where possible and implausibility flag columns for the specified variable.
#' For individuals aged 20 years or younger, values are flagged if the absolute z-score exceeds 4 or -4.
#' For those over 20 years old, BMI is flagged if it is less than 14 or greater than 50,
#' weight is flagged if it is less than 30 kg or greater than 180 kg,
#' and height is flagged if it is less than 120 cm or greater than 210 cm.
#'
#' @export
#'

flag_anthro <- function(data, variable, patient_id_col, sex_col, age_col) {
  load_extdata <- function(filename) {
    path <- system.file("extdata", filename, package = "healthimpute")
    if (path == "") {
      path <- here::here("inst", "extdata", filename)
    }
    return(path)
  }
  if (!stringr::str_detect(variable, "(?i)weight|height|bmi")) {
    stop("The `variable` argument must be pointing to variables that contain the substrings: 'weight', 'height', or 'bmi'.")
  }

  if (!(variable %in% names(data))) {
    stop("The `variable` you supplied does not exist in the data.")
  }

  id_sym <- rlang::sym(patient_id_col)
  sex_sym <- rlang::sym(sex_col)
  var_sym <- rlang::sym(variable)
  age_sym<-rlang::sym(age_col)
  z_sym <- rlang::sym(paste0("z_", variable))
  data[[variable]] <- as.numeric(data[[variable]])
  data[[age_col]] <- as.numeric(data[[age_col]])

  # Load LMS reference data
  wtage <- rio::import(load_extdata("wtage.csv")) %>% dplyr::mutate(variable = "weight")
  statage <- rio::import(load_extdata("statage.csv")) %>% dplyr::mutate(variable = "height")
  bmiagerev <- rio::import(load_extdata("bmiagerev.csv")) %>%
    dplyr::filter(Sex != "Sex") %>%
    dplyr::mutate(across(Sex, as.integer), across(-Sex, as.numeric), variable = "bmi")
  wtageinf <- rio::import(load_extdata("wtageinf.csv")) %>% dplyr::mutate(variable = "weight")
  lenageinf <- rio::import(load_extdata("lenageinf.csv")) %>%
    dplyr::filter(Sex != "Sex") %>%
    dplyr::mutate(across(Sex, as.integer), across(-Sex, as.numeric), variable = "height")

  lms_combined <- dplyr::bind_rows(wtage, statage, bmiagerev) %>% janitor::clean_names() %>% dplyr::mutate(agemos = as.numeric(agemos))
  lms_combined_inf <- dplyr::bind_rows(wtageinf, lenageinf) %>% janitor::clean_names() %>% dplyr::mutate(agemos = as.numeric(agemos)) %>%
    filter(agemos<=24.5) # the adults lms goes from 24 months so not sure why the infants lms goes up to 36 - use the adults lms from >=24 months

  variable_lms<-ifelse(grepl("weight",variable,ignore.case = T),"weight",
                       ifelse(grepl("height",variable,ignore.case = T),"height",
                              ifelse(grepl("bmi",variable,ignore.case = T),"bmi",NA)))

  # Internal LMS interpolation function
  interpolate_LMS <- function(sex_val, agemos_val, variable, ref_lms_data) {
    subset_lms <- dplyr::filter(ref_lms_data, sex == sex_val & variable == variable_lms) %>%
      dplyr::arrange(agemos)

    subset_lms <- dplyr::filter(lms_combined, sex == 1 & variable == "weight") %>%
      dplyr::arrange(agemos)

    if (nrow(subset_lms) == 0) return(rep(NA, 3))
    lower_idx <- max(which(subset_lms$agemos <= agemos_val))
    upper_idx <- min(which(subset_lms$agemos >= agemos_val))
    if (is.na(lower_idx) || is.na(upper_idx)) return(rep(NA, 3))
    lower_row <- subset_lms[lower_idx, ]
    upper_row <- subset_lms[upper_idx, ]
    if (lower_idx == upper_idx) {
      return(c(lower_row$l, lower_row$m, lower_row$s))
    } else {
      frac <- (agemos_val - lower_row$agemos) / (upper_row$agemos - lower_row$agemos)
      return(c(
        lower_row$l + frac * (upper_row$l - lower_row$l),
        lower_row$m + frac * (upper_row$m - lower_row$m),
        lower_row$s + frac * (upper_row$s - lower_row$s)
      ))
    }
  }

  zscore_LMS <- function(X, L, M, S) {
    dplyr::if_else(
      is.na(X) | is.na(L) | is.na(M) | is.na(S),
      NA_real_,
      dplyr::if_else(L != 0, (((X / M)^L) - 1) / (L * S), log(X / M) / S)
    )
  }

  prepare_data <- function(df, min_age, max_age) {
    dplyr::mutate(df,
                  sex_lms = dplyr::case_when(
                    !!sex_sym %in% c("M", "male", "Male", 1) ~ 1L,
                    !!sex_sym %in% c("F", "female", "Female", 2) ~ 2L,
                    TRUE ~ as.integer(!!sex_sym)
                  ),
                  agemos = !!age_sym * 12
    ) %>%
      dplyr::filter(!is.na(sex_lms) & !is.na(agemos) & agemos > min_age & agemos < max_age)
  }

  data_adults <- prepare_data(data, 24, 240)
  data_inf <- if (!grepl("bmi",variable)) prepare_data(data, 0, 24) else NULL

  # Issue warning if no individuals under 20 years are present
  if ((nrow(data_adults) == 0) & (is.null(data_inf) || nrow(data_inf) == 0)) {
    stop("No individuals under 20 years old found in the dataset. This function is designed for pediatric and adolescent data.")
  }

  if (!is.null(data_adults) && nrow(data_adults) > 0) {
    lms_vals <- purrr::pmap_dfr(
      list(data_adults$sex_lms, data_adults$agemos),
      ~ {
        lms <- interpolate_LMS(ref_lms_data = lms_combined,sex_val = ..1, agemos_val = ..2, variable = variable_lms)
        tibble::tibble(L_interp = lms[1], M_interp = lms[2], S_interp = lms[3])
      }
    )
    data_adults <- dplyr::bind_cols(data_adults, lms_vals)
  }

  if (!is.null(data_inf) && nrow(data_inf) > 0) {
    lms_vals <- purrr::pmap_dfr(
      list(data_inf$sex_lms, data_inf$agemos),
      ~ {
        lms <- interpolate_LMS(ref_lms_data = lms_combined_inf,sex_val = ..1, agemos_val = ..2, variable = variable_lms)
        tibble::tibble(L_interp = lms[1], M_interp = lms[2], S_interp = lms[3])
      }
    )
    data_inf <- dplyr::bind_cols(data_inf, lms_vals)
  }



  # Precompute variable type booleans once (outside mutate)
  is_bmi <- grepl("bmi", variable,ignore.case = T)
  is_weight <- grepl("weight", variable,ignore.case = T)
  is_height <- grepl("height", variable,ignore.case = T)

  data_flag <- dplyr::bind_rows(data_adults, data_inf)

  data_final_flag<- left_join(data,data_flag) %>%
    dplyr::mutate(
      !!paste0("z_", variable) := zscore_LMS(!!var_sym, L_interp, M_interp, S_interp)
    ) %>%
    dplyr::mutate(
      !!paste0("percentile_", variable) := pmin(stats::pnorm(!!z_sym) * 100, 99.999),
      !!paste0("implausible_", variable) := dplyr::case_when(
        age_at_visit <= 20 & abs(!!z_sym) > 4 | abs(!!z_sym) < -4 ~ TRUE,
        age_at_visit > 20 & is_bmi & (!!var_sym < 14 | !!var_sym > 50) ~ TRUE,
        age_at_visit > 20 & is_weight & (!!var_sym < 30 | !!var_sym > 180) ~ TRUE,
        age_at_visit > 20 & is_height & (!!var_sym < 120 | !!var_sym > 210) ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    select(-c(L_interp,M_interp,S_interp))


  return(data_final_flag)
}
