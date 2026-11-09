rm(list = ls())

source(here::here("utils", "helpers.R"))

packages <- c("dplyr", "tidyr", "purrr", "stringr", "readxl", "openxlsx", 
              "lubridate", "readr", "here", "rlang", "tibble", "cleaningtools")
load_packages(packages)

options(scipen = 999, dplyr.summarise.inform = FALSE)

raw_file <- here("data", "raw", "LTV_Household_V2_IMPACT_training_diversified.xlsx")
logical_checks_file <- here("resources", "logical_checks.xlsx")
cleaning_log_file <- here("outputs", "cleaning_log.xlsx")
combined_checks_file <- here("outputs", "combined_checks.rds")

dir.create(here("outputs"), recursive = TRUE, showWarnings = FALSE)

if (!file.exists(raw_file)) stop("Raw data not found: ", raw_file)

raw_data <- readxl::read_excel(raw_file, sheet = "LTV Household V2 Form (Round1)", na = c("", "NA"))

raw_data <- raw_data |>
  dplyr::mutate(
    uuid = as.character(`_id`),
    submission_uuid = as.character(`_uuid`),
    start = convert_datetime(start),
    end = convert_datetime(end)
  )

numeric_vars <- intersect(
  c("resp_age", "hoh_age", "hh_size", "hh_children_u18", "hh_children_u5", 
    "disp_count", "_hh_location_latitude", "_hh_location_longitude", 
    "_hh_location_altitude", "_hh_location_precision", "amount_recieved"),
  names(raw_data)
)

raw_data[numeric_vars] <- lapply(raw_data[numeric_vars], function(x) {
  readr::parse_number(as.character(x))
})

raw_data <- raw_data |>
  dplyr::mutate(duration_minutes = as.numeric(difftime(end, start, units = "mins")))

message("Imported ", nrow(raw_data), " records with ", ncol(raw_data), " variables")

logical_checks <- readxl::read_excel(logical_checks_file, sheet = "checks")

checks <- raw_data |>
  cleaningtools::check_duplicate(
    uuid_column = "uuid",
    columns_to_check = "submission_uuid",
    log_name = "duplicate_submission_uuid_log"
  ) |>
  cleaningtools::check_duration(
    column_to_check = "duration_minutes",
    uuid_column = "uuid",
    log_name = "duration_log",
    lower_bound = 10,
    higher_bound = 180
  ) |>
  cleaningtools::check_logical_with_list(
    uuid_column = "uuid",
    information_to_add = NULL,
    list_of_check = logical_checks,
    check_id_column = "check_id",
    check_to_perform_column = "check_to_perform",
    columns_to_clean_column = "columns_to_clean",
    description_column = "description",
    bind_checks = TRUE
  )

outlier_vars <- intersect(
  c("resp_age", "hoh_age", "hh_size", "hh_children_u18", "hh_children_u5", 
    "disp_count", "amount_recieved"),
  names(checks$checked_dataset)
)

outlier_data <- checks$checked_dataset |>
  dplyr::select(uuid, all_of(outlier_vars))

outlier_checks <- cleaningtools::check_outliers(
  dataset = outlier_data,
  uuid_column = "uuid",
  strongness_factor = 3,
  minimum_unique_value_of_variable = 5,
  remove_choice_multiple = TRUE
)

all_check_logs <- c(checks, outlier_checks[names(outlier_checks) != "checked_dataset"])

combined_checks <- cleaningtools::create_combined_log(
  list_of_log = all_check_logs,
  dataset_name = "checked_dataset"
)

context_cols <- raw_data |>
  dplyr::select(uuid, submission_uuid, any_of(c("admin1Pcod", "admin2Pcod", "enum_id"))) |>
  dplyr::distinct()

combined_checks$cleaning_log <- combined_checks$cleaning_log |>
  dplyr::left_join(context_cols, by = "uuid") |>
  dplyr::mutate(feedback = NA_character_) |>
  dplyr::relocate(uuid, submission_uuid, any_of(c("admin1Pcod", "admin2Pcod", "enum_id")))

saveRDS(combined_checks, combined_checks_file)

cleaningtools::create_xlsx_cleaning_log(
  write_list = combined_checks,
  cleaning_log_name = "cleaning_log",
  change_type_col = "change_type",
  column_for_color = "check_binding",
  use_dropdown = FALSE,
  output_path = cleaning_log_file
)

message("Checking complete. Review: ", cleaning_log_file)
message("Save reviewed version as: outputs/cleaning_log_reviewed.xlsx")