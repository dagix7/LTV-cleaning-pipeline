################################################################################
# LTV DATA CHECKING
################################################################################

rm(list = ls())

################################################################################
# 1. Load packages
################################################################################

required_packages <- c(
  "dplyr",
  "tidyr",
  "purrr",
  "stringr",
  "readxl",
  "openxlsx",
  "lubridate",
  "readr",
  "here",
  "rlang",
  "tibble",
  "cleaningtools"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {
  stop(
    "Missing packages: ",
    paste(missing_packages, collapse = ", "),
    ". Run source('00_setup.R') first."
  )
}

invisible(
  lapply(
    required_packages,
    library,
    character.only = TRUE
  )
)

options(
  scipen = 999,
  dplyr.summarise.inform = FALSE
)

################################################################################
# 2. Define paths
################################################################################

raw_file <- here::here(
  "data",
  "raw",
  "LTV_Household_V2_IMPACT_training_diversified.xlsx"
)

logical_checks_file <- here::here(
  "resources",
  "logical_checks.xlsx"
)

cleaning_log_file <- here::here(
  "outputs",
  "cleaning_log.xlsx"
)

combined_checks_file <- here::here(
  "outputs",
  "combined_checks.rds"
)

dir.create(
  here::here("outputs"),
  recursive = TRUE,
  showWarnings = FALSE
)

################################################################################
# 3. Confirm files exist
################################################################################

if (!file.exists(raw_file)) {
  stop(
    "Raw workbook not found. Put ",
    "LTV_Household_V2_IMPACT_training_diversified.xlsx ",
    "inside data/raw/."
  )
}

if (!file.exists(logical_checks_file)) {
  stop(
    "resources/logical_checks.xlsx was not found."
  )
}

################################################################################
# 4. Import raw data
################################################################################

raw_data <- readxl::read_excel(
  raw_file,
  sheet = "LTV Household V2 Form (Round1)",
  na = c("", "NA")
)

required_variables <- c(
  "_id",
  "_uuid",
  "start",
  "end",
  "hh_size",
  "hh_children_u18",
  "hh_children_u5",
  "resp_age"
)

missing_variables <- setdiff(
  required_variables,
  names(raw_data)
)

if (length(missing_variables) > 0) {
  stop(
    "Missing variables: ",
    paste(
      missing_variables,
      collapse = ", "
    )
  )
}

################################################################################
# 5. Create working key
################################################################################

# The diversified training data intentionally contain a missing and duplicated
# _uuid. Kobo _id remains unique, so _id is used as the working key.
#
# submission_uuid preserves the original Kobo UUID for checking.

raw_data <- raw_data |>
  dplyr::mutate(
    uuid = as.character(.data[["_id"]]),
    submission_uuid =
      as.character(.data[["_uuid"]])
  )

################################################################################
# 6. Convert dates
################################################################################

convert_datetime <- function(x) {
  
  if (inherits(x, "POSIXt")) {
    return(
      as.POSIXct(
        x,
        tz = "UTC"
      )
    )
  }
  
  if (inherits(x, "Date")) {
    return(
      as.POSIXct(
        x,
        tz = "UTC"
      )
    )
  }
  
  if (is.numeric(x)) {
    return(
      as.POSIXct(
        x * 86400,
        origin = "1899-12-30",
        tz = "UTC"
      )
    )
  }
  
  parsed <- suppressWarnings(
    lubridate::parse_date_time(
      as.character(x),
      orders = c(
        "Ymd HMS",
        "Ymd HM",
        "Y-m-dTH:M:S",
        "Y-m-dTH:M:OS"
      ),
      tz = "UTC"
    )
  )
  
  as.POSIXct(
    parsed,
    tz = "UTC"
  )
}

raw_data <- raw_data |>
  dplyr::mutate(
    start = convert_datetime(.data[["start"]]),
    end = convert_datetime(.data[["end"]])
  )

################################################################################
# 7. Convert selected variables to numeric
################################################################################

numeric_variables <- intersect(
  c(
    "resp_age",
    "hoh_age",
    "hh_size",
    "hh_children_u18",
    "hh_children_u5",
    "disp_count",
    "_hh_location_latitude",
    "_hh_location_longitude",
    "_hh_location_altitude",
    "_hh_location_precision",
    "amount_recieved"
  ),
  names(raw_data)
)

raw_data[numeric_variables] <- lapply(
  raw_data[numeric_variables],
  function(x) {
    readr::parse_number(
      as.character(x)
    )
  }
)

################################################################################
# 8. Calculate interview duration
################################################################################

raw_data <- raw_data |>
  dplyr::mutate(
    duration_minutes = as.numeric(
      difftime(
        .data[["end"]],
        .data[["start"]],
        units = "mins"
      )
    )
  )

message(
  "Records imported: ",
  nrow(raw_data)
)

message(
  "Variables imported: ",
  ncol(raw_data)
)

################################################################################
# 9. Read logical checks
################################################################################

logical_checks <- readxl::read_excel(
  logical_checks_file,
  sheet = "checks"
)

required_check_columns <- c(
  "check_id",
  "check_to_perform",
  "columns_to_clean",
  "description"
)

missing_check_columns <- setdiff(
  required_check_columns,
  names(logical_checks)
)

if (length(missing_check_columns) > 0) {
  stop(
    "logical_checks.xlsx is missing: ",
    paste(
      missing_check_columns,
      collapse = ", "
    )
  )
}

################################################################################
# 10. Duplicate Kobo UUID check
################################################################################

checks <- raw_data |>
  cleaningtools::check_duplicate(
    uuid_column = "uuid",
    columns_to_check = "submission_uuid",
    log_name = "duplicate_submission_uuid_log"
  )

# check_duplicate() returns checked_dataset and a duplicate log. 【1-016509】

################################################################################
# 11. Duration check
################################################################################

checks <- checks |>
  cleaningtools::check_duration(
    column_to_check = "duration_minutes",
    uuid_column = "uuid",
    log_name = "duration_log",
    lower_bound = 10,
    higher_bound = 180
  )

# The limits are training assumptions.
# check_duration() flags values strictly outside the range. 【2-e68cf6】

################################################################################
# 12. Logical checks
################################################################################

checks <- checks |>
  cleaningtools::check_logical_with_list(
    uuid_column = "uuid",
    information_to_add = NULL,
    list_of_check = logical_checks,
    check_id_column = "check_id",
    check_to_perform_column =
      "check_to_perform",
    columns_to_clean_column =
      "columns_to_clean",
    description_column =
      "description",
    bind_checks = TRUE
  )

# The Excel workbook supplies the condition, affected columns and explanation.
# 【3-dfe608】

################################################################################
# 13. Outlier checks
################################################################################

outlier_variables <- intersect(
  c(
    "resp_age",
    "hoh_age",
    "hh_size",
    "hh_children_u18",
    "hh_children_u5",
    "disp_count",
    "amount_recieved"
  ),
  names(checks$checked_dataset)
)

outlier_data <- checks$checked_dataset |>
  dplyr::select(
    uuid,
    dplyr::all_of(
      outlier_variables
    )
  )

outlier_checks <-
  cleaningtools::check_outliers(
    dataset = outlier_data,
    uuid_column = "uuid",
    strongness_factor = 3,
    minimum_unique_value_of_variable = 5,
    remove_choice_multiple = TRUE
  )

# Outliers are review flags, not automatic errors. 【4-56a58e】

################################################################################
# 14. Combine check logs
################################################################################

outlier_logs <- outlier_checks[
  names(outlier_checks) !=
    "checked_dataset"
]

all_check_logs <- c(
  checks,
  outlier_logs
)

combined_checks <-
  cleaningtools::create_combined_log(
    list_of_log = all_check_logs,
    dataset_name = "checked_dataset"
  )

# create_combined_log() returns checked_dataset and cleaning_log.
# 【5-563d85】

################################################################################
# 15. Add useful context
################################################################################

record_context <- raw_data |>
  dplyr::select(
    uuid,
    submission_uuid,
    dplyr::any_of(
      c(
        "admin1Pcod",
        "admin2Pcod",
        "enum_id"
      )
    )
  ) |>
  dplyr::distinct()

combined_checks$cleaning_log <-
  combined_checks$cleaning_log |>
  dplyr::left_join(
    record_context,
    by = "uuid"
  ) |>
  dplyr::mutate(
    feedback = NA_character_
  ) |>
  dplyr::relocate(
    uuid,
    submission_uuid,
    dplyr::any_of(
      c(
        "admin1Pcod",
        "admin2Pcod",
        "enum_id"
      )
    )
  )

################################################################################
# 16. Save combined checking object
################################################################################

saveRDS(
  combined_checks,
  combined_checks_file
)

################################################################################
# 17. Export Excel cleaning log
################################################################################

cleaningtools::create_xlsx_cleaning_log(
  write_list = combined_checks,
  cleaning_log_name = "cleaning_log",
  change_type_col = "change_type",
  column_for_color = "check_binding",
  use_dropdown = FALSE,
  output_path = cleaning_log_file
)

# XLSForm-based dropdowns are disabled because the matching survey and choices
# sheets are not available. 【6-b7f516】

################################################################################
# 18. Final message
################################################################################

message(
  "Checking completed."
)

message(
  "Open: ",
  cleaning_log_file
)

message(
  "Review every row and save the copy as:"
)

message(
  here::here(
    "outputs",
    "cleaning_log_reviewed.xlsx"
  )
)