utils::globalVariables(c(
  "Sex", "agemos", "sex", "sex_lms",
  "observed", "L_interp", "M_interp", "S_interp", "z",
  "percentile", "flag", "percentile_new",
  "nearest_result", "Number_Of_Days", "nearest_value",
  "imputed_value", "obs_time", "obs_value", "row_id"
))

#' @title Impute Missing Values Within a Time Window
#'
#' @description
#' This function imputes missing values in a longitudinal dataset by carrying forward or backward
#' the nearest observed non-missing value within a defined number of days (default is 30). It is
#' suitable for clinical measures such as HbA1c, insulin, or any time-varying biomarker. It also
#' can be used to impute antropometric data in adults over the age of 20.
#'
#' The function searches within each patient for the closest available value in time and imputes it
#' only if it falls within the defined time window. This is useful for preserving temporal integrity
#' while minimizing missingness in sparse clinical data.
#'
#' @param data A data frame containing repeated measures data with a time variable.
#' @param variable A character string specifying the variable to impute (e.g., `"hba1c"`).
#' @param patient_id_col A character string indicating the name of the patient ID column (e.g., `"id"`).
#' @param dy_col A character string indicating the name of the time column (e.g., `"dy"` for days from baseline).
#' @param window_days Numeric. The maximum absolute time difference (in days) allowed for imputation. Default is 30. Using a window_days value greater than 30 is not recommended for imputation of biological variables that can change overtime such as HbA1c, insulin dose or anthropometric values.
#' @param is_date Logical. If TRUE, the `dy_col` is a `Date` object and will be converted to numeric days. If FALSE, assumes `dy_col` is already numeric. Default is FALSE.
#'
#' @return A cleaned and augmented data frame containing:
#' - The original variable
#' - A new column `imputed_value_variable` containing the imputed values
#' - A logical flag `variable_carried` indicating whether imputation was applied
#'
#' @details
#' - Only missing values with a non-missing neighbor within the specified window are imputed.
#' - The nearest value (forward or backward in time) is used.
#' - Imputation does not occur if no observed value is found within the window.
#' - The function is flexible and can be used for both lab and clinical measurements.
#' - Ensure you assign the function in order to store the outputted dataframe in your environment.
#'
#' @import dplyr
#' @import tidyr
#' @importFrom purrr map_dfr
#' @importFrom rlang sym
#' @import anytime
#' @import lubridate
#' @import tibble
#' @import utils
#'
#' @author Alice Carr
#'
#' @examples
#' example_data <- data.frame(
#'   id = c(1, 1, 1, 2, 2),
#'   dy = c(0, 15, 45, 0, 20),
#'   hba1c = c(7.2, NA, 7.4, NA, 6.9)
#' )
#' impute_timewindow(
#'   example_data,
#'   variable = "hba1c",
#'   patient_id_col = "id",
#'   dy_col = "dy",
#'   is_date = FALSE
#' )
#' @export

impute_timewindow <- function(data,
                              variable,
                              patient_id_col,
                              dy_col,
                              window_days = 30,
                              is_date = FALSE) {
  if (!(variable %in% names(data))) {
    stop("The `variable` you supplied does not exist in the data.")
  }

  if (window_days > 30) {
    warning(
      "Using a window_days value greater than 30 is not recommended for impuation of biological variables that can change overtime such as HbA1c, insulin dose or anthropometric values"
    )
  }

  # Check validity of is_date argument against dy_col type
  if (!is_date && !is.numeric(data[[dy_col]])) {
    stop(
      "The column specified in `dy_col` must be numeric when `is_date = FALSE` (e.g., days since enrollment)."
    )
  }

  if (is_date && !lubridate::is.timepoint(data[[dy_col]])) {
    stop(
      "The column specified in `dy_col` must be a date or datetime (e.g., Date, POSIXct, or POSIXlt) when `is_date = TRUE`."
    )
  }

  var_sym <- rlang::sym(variable)
  dy_sym <- rlang::sym(dy_col)
  id_sym <- rlang::sym(patient_id_col)
  data[[variable]] <- as.numeric(data[[variable]])


  # If dy_col is a Date, convert to numeric days from first date per patient
  if (is_date) {
    data <- data %>%
      group_by(!!id_sym) %>%
      mutate(.dy_numeric = as.numeric(difftime(
        anytime::anydate(!!dy_sym),
        min(anytime::anydate(!!dy_sym), na.rm = TRUE),
        units = "days"
      ))) %>%
      ungroup()
    dy_working <- ".dy_numeric"
  } else {
    dy_working <- dy_col
  }

  data <- data %>%
    dplyr::mutate(row_id = dplyr::row_number()) %>%
    dplyr::group_by(!!id_sym) %>%
    dplyr::mutate(
      obs_time = ifelse(!is.na(!!var_sym), .data[[dy_working]], NA_real_),
      obs_value = ifelse(!is.na(!!var_sym), as.numeric(!!var_sym), NA_real_)
    ) %>%
    dplyr::mutate(nearest_result = purrr::map_dfr(row_number(), function(i) {
      this_time <- .data[[dy_working]][i]
      avail_times <- .data$obs_time[!is.na(.data$obs_time)]
      avail_values <- .data$obs_value[!is.na(.data$obs_value)]

      if (length(avail_times) == 0) {
        return(tibble::tibble(nearest_value = NA_real_, Number_Of_Days = NA_real_))
      }

      time_diff <- abs(avail_times - this_time)
      min_idx <- which.min(time_diff)
      tibble::tibble(nearest_value = avail_values[min_idx], Number_Of_Days = time_diff[min_idx])
    })) %>%
    tidyr::unnest_wider(nearest_result) %>%
    dplyr::ungroup()

  # Impute if within window
  data <- data %>%
    dplyr::mutate(
      imputed_value = ifelse(
        is.na(!!var_sym) & Number_Of_Days <= window_days,
        nearest_value,
        NA_real_
      ),
      !!paste0("imputed_value_", variable) := ifelse(is.na(!!var_sym), imputed_value, !!var_sym),
      !!paste0(variable, "_carried") := dplyr::case_when(
        is.na(!!var_sym) & !is.na(imputed_value) ~ TRUE,
        is.na(!!var_sym) & is.na(imputed_value) ~ NA,
        TRUE ~ FALSE
      )
    ) %>%
    dplyr::select(-c(
      obs_time,
      obs_value,
      nearest_value,
      Number_Of_Days,
      imputed_value,
      row_id
    ))

  if (is_date) {
    data <- dplyr::select(data, -".dy_numeric")
  }

  return(data)
}
