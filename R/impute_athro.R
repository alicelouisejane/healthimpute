#' @title Impute Missing Anthropometric Values Using CDC Growth Charts
#'
#' @description
#' This function imputes missing anthropometric values (weight, height, or BMI) in a longitudinal dataset of pediatric and adolescent patients using the LMS method from CDC growth charts. It interpolates LMS values, calculates z-scores and percentiles, flags outliers, and imputes missing percentiles using neighboring timepoints. Measurements are back-calculated from imputed percentiles using the LMS formula.
#'
#' The function supports individuals aged 0 to 20 years using the CDC infant (0–36 months) and child/adolescent (36–240 months) growth standards.
#'
#' @param data A data frame containing anthropometric measurements and related metadata.
#' @param variable A character string specifying the name of the variable to impute. The name must contain `"weight"`, `"height"`, or `"bmi"`.
#' @param patient_id_col A character string specifying the name of the column containing the patient ID.
#' @param age_col A character string specifying the name of the column containing age in years.
#' @param sex_col A character string specifying the name of the column indicating sex. Values should be coded as `"M"`/`1` for male and `"F"`/`2` for female.
#'
#' @return A data frame that includes:
#' - the original data,
#' - an imputed value column (e.g., `imputed_value_weight`), and
#' - a flag column indicating if the value was imputed (e.g., `weight_carried`).
#'
#' @details
#' - The input variable name must contain `"weight"`, `"height"`, or `"bmi"`.
#' - Sex values are standardized to `1 = male` and `2 = female`.
#' - Percentiles <1 or >100 are flagged as implausible and excluded from imputation.
#' - Imputed values are estimated using the average of the nearest available percentiles in time.
#' - Ensure you assign the function in order to store the outputted dataframe in your environment.
#'
#' @import dplyr
#' @import here
#' @import tidyr
#' @importFrom purrr map_dfr
#' @importFrom rlang sym
#' @importFrom zoo na.locf
#' @importFrom stats qnorm pnorm
#' @importFrom janitor clean_names
#'
#' @author Alice Carr
#'
#' @examples
#' example_data <- data.frame(
#'   patient_id = c(1, 1, 1, 2, 2),
#'   sex = c("M", "M", "M", "F", "F"),
#'   age_at_visit = c(5, 6, 7, 5, 7),
#'   weightkg = c(NA, 22, NA, 18, NA)
#' )
#' impute_anthro(
#'   data = example_data,
#'   variable = "weightkg",
#'   patient_id_col = "patient_id",
#'   age_col = "age_at_visit",
#'   sex_col = "sex"
#' )
#'
#' @export

 impute_anthro <- function(data, variable,patient_id_col,age_col,sex_col) {
   load_extdata <- function(filename) {
     path <- system.file("extdata", filename, package = "healthimpute")
     if (path == "") {
       path <- here::here("inst", "extdata", filename)
     }
     return(path)
   }
   if (!stringr::str_detect(variable, "weight|height|bmi")) {
     stop("The `variable` argument must be pointing to variables that contain the substrings: 'weight', 'height', or 'bmi'.")
   }

   if (!(variable %in% names(data))) {
     stop("The `variable` you supplied does not exist in the data.")
   }

   id_sym <- rlang::sym(patient_id_col)
   sex_sym <- rlang::sym(sex_col)
   var_sym <- rlang::sym(variable)
   age_sym<-rlang::sym(age_col)
   data[[variable]] <- as.numeric(data[[variable]])

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
     filter(agemos<24) # the adults lms goes from 24 months so not sure why the infants lms goes up to 36 - use the adults lms from >=24 months

   variable_lms<-ifelse(grepl("weight",variable),"weight",
                        ifelse(grepl("height",variable),"height",
                               ifelse(grepl("bmi",variable),"bmi",NA)))

   # Internal LMS interpolation function
   interpolate_LMS <- function(sex_val, agemos_val, variable, ref_lms_data) {
     subset_lms <- dplyr::filter(ref_lms_data, sex == sex_val & variable == variable) %>% dplyr::arrange(agemos)
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
       dplyr::filter(!is.na(sex_lms) & !is.na(agemos) & agemos >= min_age & agemos < max_age)
   }

   data_adults <- prepare_data(data, 24, 241)
   data_inf <- if (!grepl("bmi",variable)) prepare_data(data, 0, 24) else NULL

   # Issue warning if no individuals under 20 years are present
   if ((nrow(data_adults) == 0) & (is.null(data_inf) || nrow(data_inf) == 0)) {
     stop("No individuals under 20 years old found in the dataset. This function is designed for pediatric and adolescent data.")
   }

   if (!is.null(data_adults) && nrow(data_adults) > 0) {
     lms_vals <- purrr::pmap_dfr(
       list(data_adults$sex_lms, data_adults$agemos),
       ~ {
         lms <- interpolate_LMS(..1, ..2, variable_lms, lms_combined)
         tibble::tibble(L_interp = lms[1], M_interp = lms[2], S_interp = lms[3])
       }
     )
     data_adults <- dplyr::bind_cols(data_adults, lms_vals)
   }

   if (!is.null(data_inf) && nrow(data_inf) > 0) {
     lms_vals <- purrr::pmap_dfr(
       list(data_inf$sex_lms, data_inf$agemos),
       ~ {
         lms <- interpolate_LMS(..1, ..2, variable_lms, lms_combined_inf)
         tibble::tibble(L_interp = lms[1], M_interp = lms[2], S_interp = lms[3])
       }
     )
     data_inf <- dplyr::bind_cols(data_inf, lms_vals)
   }

   data_new <- dplyr::bind_rows(data_adults, data_inf) %>%
     dplyr::mutate(
       observed = !is.na(!!var_sym),
       z = ifelse(observed, zscore_LMS(!!var_sym, L_interp, M_interp, S_interp), NA_real_),
       percentile = stats::pnorm(z) * 100,
       flag = ifelse(percentile < 1 | percentile > 100, 1, NA_integer_),
       percentile_new = ifelse(!is.na(flag), NA_real_, percentile)
     ) %>%
     dplyr::group_by(!!id_sym) %>%
     dplyr::arrange(!!age_sym, .by_group = TRUE) %>%
     dplyr::mutate(
       last_percentile = zoo::na.locf(percentile_new, na.rm = FALSE),
       next_percentile = zoo::na.locf(percentile_new, na.rm = FALSE, fromLast = TRUE),
       impute_percentile = dplyr::case_when(
         !is.na(last_percentile) & !is.na(next_percentile) ~ (last_percentile + next_percentile) / 2,
         !is.na(last_percentile) ~ last_percentile,
         !is.na(next_percentile) ~ next_percentile,
         TRUE ~ NA_real_
       )
     ) %>%
     dplyr::ungroup()

   imputed_vals <- ifelse(
     !is.na(data_new$impute_percentile),
     data_new$M_interp * (1 + data_new$L_interp * data_new$S_interp * stats::qnorm(data_new$impute_percentile / 100))^(1 / data_new$L_interp),
     NA_real_
   )
   data_new[[paste0("imputed_value_", variable)]] <- imputed_vals

   data_new<-data_new %>%
     select(-sex_lms)

   data_final <- left_join(data,data_new) %>%
     dplyr::mutate(
       !!variable := dplyr::if_else(is.na(!!var_sym), .data[[paste0("imputed_value_", variable)]], !!var_sym),
       !!paste0(variable, "_carried") := dplyr::if_else(observed == FALSE & !is.na(.data[[variable]]), TRUE, FALSE)
     ) %>%
     dplyr::select(dplyr::all_of(names(data)), dplyr::contains(c("imputed_value", "carried")))

   return(data_final)
 }
