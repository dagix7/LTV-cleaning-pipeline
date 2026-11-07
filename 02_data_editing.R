################################################################################
# LTV DATA EDITING
#
# Purpose:
#   1. Re-import the untouched raw dataset
#   2. Recreate variables used during data checking
#   3. Read the reviewed cleaning log
#   4. Validate all cleaning decisions
#   5. Apply approved corrections
#   6. Review whether corrections were applied correctly
#   7. Export the cleaned dataset and review outputs
################################################################################

rm(list = ls())

################################################################################
# 1. LOAD PACKAGES
################################################################################

required_packages <- c(
  "dplyr",
  "tidyr",
  "stringr",
  "readxl",
  "openxlsx",
  "readr",
  "here",
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
# 2. DEFINE PATHS
################################################################################

raw_file <- here::here(
  "data",
  "raw",
  "LTV_Household_V2_IMPACT_training_diversified.xlsx"
)

reviewed_log_file <- here::here(
  "outputs",
  "cleaning_log_reviewed.xlsx"
)

clean_rds_file <- here::here(
  "data",
  "clean",
  "LTV_clean_data.rds"
)

clean_xlsx_file <- here::here(
  "data",
  "clean",
  "LTV_clean_data.xlsx"
)

cleaning_review_rds_file <- here::here(
  "outputs",
  "cleaning_review.rds"
)

cleaning_review_xlsx_file <- here::here(
  "outputs",
  "cleaning_review.xlsx"
)

deletion_log_file <- here::here(
  "outputs",
  "deletion_log.xlsx"
)

parent_correction_log_file <- here::here(
  "outputs",
  "select_multiple_parent_corrections.xlsx"
)

unparsed_datetime_file <- here::here(
  "outputs",
  "unparsed_datetime_values.xlsx"
)

invalid_duration_file <- here::here(
  "outputs",
  "invalid_negative_durations.xlsx"
)

dir.create(
  here::here("data", "clean"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  here::here("outputs"),
  recursive = TRUE,
  showWarnings = FALSE
)

################################################################################
# 3. CONFIRM INPUT FILES EXIST
################################################################################

if (!file.exists(raw_file)) {
  stop(
    "Raw workbook not found. Expected file: ",
    raw_file
  )
}

if (!file.exists(reviewed_log_file)) {
  stop(
    "Reviewed cleaning log not found. ",
    "Review outputs/cleaning_log.xlsx and save it as ",
    "outputs/cleaning_log_reviewed.xlsx."
  )
}

################################################################################
# 4. DATETIME CONVERSION FUNCTION
################################################################################

convert_datetime <- function(x, timezone = "UTC") {
  
  number_of_values <- length(x)
  
  if (number_of_values == 0) {
    return(
      as.POSIXct(
        character(0),
        tz = timezone
      )
    )
  }
  
  if (inherits(x, "POSIXt")) {
    return(
      as.POSIXct(
        x,
        tz = timezone
      )
    )
  }
  
  if (inherits(x, "Date")) {
    return(
      as.POSIXct(
        x,
        tz = timezone
      )
    )
  }
  
  x_character <- trimws(
    as.character(x)
  )
  
  missing_positions <- (
    is.na(x_character) |
      x_character == "" |
      toupper(x_character) %in% c(
        "NA",
        "N/A",
        "NULL",
        "NAN"
      )
  )
  
  x_character[missing_positions] <- NA_character_
  
  result_numeric <- rep(
    NA_real_,
    number_of_values
  )
  
  numeric_values <- suppressWarnings(
    as.numeric(x_character)
  )
  
  excel_positions <- which(
    !is.na(numeric_values) &
      numeric_values >= 1 &
      numeric_values <= 100000
  )
  
  if (length(excel_positions) > 0) {
    
    excel_datetimes <- as.POSIXct(
      numeric_values[excel_positions] * 86400,
      origin = "1899-12-30",
      tz = timezone
    )
    
    result_numeric[excel_positions] <- as.numeric(
      excel_datetimes
    )
  }
  
  datetime_formats <- c(
    "%Y-%m-%d %H:%M:%OS",
    "%Y-%m-%d %H:%M:%S",
    "%Y-%m-%d %H:%M",
    "%Y/%m/%d %H:%M:%OS",
    "%Y/%m/%d %H:%M:%S",
    "%Y/%m/%d %H:%M",
    "%Y-%m-%dT%H:%M:%OS",
    "%Y-%m-%dT%H:%M:%S",
    "%Y-%m-%dT%H:%M",
    "%Y-%m-%dT%H:%M:%OSZ",
    "%Y-%m-%dT%H:%M:%SZ",
    "%Y-%m-%dT%H:%M:%OS%z",
    "%Y-%m-%dT%H:%M:%S%z",
    "%d/%m/%Y %H:%M:%OS",
    "%d/%m/%Y %H:%M:%S",
    "%d/%m/%Y %H:%M",
    "%d-%m-%Y %H:%M:%OS",
    "%d-%m-%Y %H:%M:%S",
    "%d-%m-%Y %H:%M",
    "%m/%d/%Y %H:%M:%OS",
    "%m/%d/%Y %H:%M:%S",
    "%m/%d/%Y %H:%M",
    "%Y-%m-%d %I:%M:%S %p",
    "%Y-%m-%d %I:%M %p",
    "%Y/%m/%d %I:%M:%S %p",
    "%Y/%m/%d %I:%M %p",
    "%d/%m/%Y %I:%M:%S %p",
    "%d/%m/%Y %I:%M %p",
    "%m/%d/%Y %I:%M:%S %p",
    "%m/%d/%Y %I:%M %p"
  )
  
  for (datetime_format in datetime_formats) {
    
    remaining_positions <- which(
      is.na(result_numeric) &
        !is.na(x_character)
    )
    
    if (length(remaining_positions) == 0) {
      break
    }
    
    parsed_values <- suppressWarnings(
      as.POSIXct(
        strptime(
          x_character[remaining_positions],
          format = datetime_format,
          tz = timezone
        )
      )
    )
    
    successful_positions <- which(
      !is.na(parsed_values)
    )
    
    if (length(successful_positions) > 0) {
      
      original_positions <- remaining_positions[
        successful_positions
      ]
      
      result_numeric[original_positions] <- as.numeric(
        parsed_values[successful_positions]
      )
    }
  }
  
  as.POSIXct(
    result_numeric,
    origin = "1970-01-01",
    tz = timezone
  )
}

################################################################################
# 5. RE-IMPORT THE UNTOUCHED RAW DATA
################################################################################

raw_sheet <- "LTV Household V2 Form (Round1)"

available_raw_sheets <- readxl::excel_sheets(
  raw_file
)

if (!raw_sheet %in% available_raw_sheets) {
  stop(
    "The expected raw-data sheet was not found: ",
    raw_sheet,
    ". Available sheets are: ",
    paste(
      available_raw_sheets,
      collapse = ", "
    )
  )
}

raw_data <- readxl::read_excel(
  raw_file,
  sheet = raw_sheet,
  col_types = "text",
  na = c("", "NA")
)

names(raw_data) <- trimws(
  names(raw_data)
)

required_raw_columns <- c(
  "_id",
  "_uuid",
  "start",
  "end",
  "hh_size",
  "hh_children_u18",
  "hh_children_u5",
  "resp_age"
)

missing_raw_columns <- setdiff(
  required_raw_columns,
  names(raw_data)
)

if (length(missing_raw_columns) > 0) {
  stop(
    "Raw data are missing these variables: ",
    paste(
      missing_raw_columns,
      collapse = ", "
    )
  )
}

if (nrow(raw_data) == 0) {
  stop(
    "The raw-data sheet contains no records."
  )
}

################################################################################
# 6. RECREATE CHECKING VARIABLES
################################################################################

raw_start_original <- raw_data[["start"]]
raw_end_original <- raw_data[["end"]]

raw_data <- raw_data |>
  dplyr::mutate(
    uuid = trimws(
      as.character(.data[["_id"]])
    ),
    submission_uuid = trimws(
      as.character(.data[["_uuid"]])
    ),
    start = convert_datetime(
      .data[["start"]],
      timezone = "UTC"
    ),
    end = convert_datetime(
      .data[["end"]],
      timezone = "UTC"
    )
  )

missing_working_ids <- raw_data |>
  dplyr::filter(
    is.na(.data[["uuid"]]) |
      .data[["uuid"]] == ""
  )

if (nrow(missing_working_ids) > 0) {
  stop(
    nrow(missing_working_ids),
    " raw-data records have a missing _id. ",
    "The editing pipeline requires _id as the unique working identifier."
  )
}

duplicated_working_ids <- raw_data |>
  dplyr::count(
    .data[["uuid"]],
    name = "number_of_records"
  ) |>
  dplyr::filter(
    .data[["number_of_records"]] > 1
  )

if (nrow(duplicated_working_ids) > 0) {
  
  openxlsx::write.xlsx(
    duplicated_working_ids,
    here::here(
      "outputs",
      "duplicated_working_ids.xlsx"
    ),
    overwrite = TRUE
  )
  
  stop(
    "Duplicated _id values were found. ",
    "Open outputs/duplicated_working_ids.xlsx."
  )
}

failed_start_positions <- which(
  !is.na(raw_start_original) &
    trimws(as.character(raw_start_original)) != "" &
    is.na(raw_data$start)
)

failed_end_positions <- which(
  !is.na(raw_end_original) &
    trimws(as.character(raw_end_original)) != "" &
    is.na(raw_data$end)
)

datetime_failures <- dplyr::bind_rows(
  tibble::tibble(
    uuid = raw_data$uuid[failed_start_positions],
    variable = "start",
    unparsed_value = as.character(
      raw_start_original[failed_start_positions]
    )
  ),
  tibble::tibble(
    uuid = raw_data$uuid[failed_end_positions],
    variable = "end",
    unparsed_value = as.character(
      raw_end_original[failed_end_positions]
    )
  )
)

if (nrow(datetime_failures) > 0) {
  
  openxlsx::write.xlsx(
    datetime_failures,
    unparsed_datetime_file,
    overwrite = TRUE
  )
  
  warning(
    nrow(datetime_failures),
    " datetime values could not be converted. ",
    "Inspect outputs/unparsed_datetime_values.xlsx."
  )
}

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

if (length(numeric_variables) > 0) {
  
  raw_data[numeric_variables] <- lapply(
    raw_data[numeric_variables],
    function(x) {
      readr::parse_number(
        as.character(x),
        na = c(
          "",
          "NA",
          "N/A",
          "NULL"
        )
      )
    }
  )
}

raw_data <- raw_data |>
  dplyr::mutate(
    duration_minutes = dplyr::if_else(
      !is.na(.data[["start"]]) &
        !is.na(.data[["end"]]),
      as.numeric(
        difftime(
          .data[["end"]],
          .data[["start"]],
          units = "mins"
        )
      ),
      NA_real_
    )
  )

invalid_durations <- raw_data |>
  dplyr::filter(
    !is.na(.data[["duration_minutes"]]),
    .data[["duration_minutes"]] < 0
  ) |>
  dplyr::select(
    .data[["uuid"]],
    .data[["start"]],
    .data[["end"]],
    .data[["duration_minutes"]]
  )

if (nrow(invalid_durations) > 0) {
  
  openxlsx::write.xlsx(
    invalid_durations,
    invalid_duration_file,
    overwrite = TRUE
  )
  
  warning(
    nrow(invalid_durations),
    " records have an end time earlier than their start time. ",
    "Inspect outputs/invalid_negative_durations.xlsx."
  )
}

message(
  "Raw data imported: ",
  nrow(raw_data),
  " records and ",
  ncol(raw_data),
  " variables."
)

################################################################################
# 7. READ THE REVIEWED CLEANING LOG
################################################################################

available_log_sheets <- readxl::excel_sheets(
  reviewed_log_file
)

if (length(available_log_sheets) == 0) {
  stop(
    "The reviewed cleaning-log workbook contains no worksheets."
  )
}

log_sheet <- if (
  "cleaning_log" %in% available_log_sheets
) {
  "cleaning_log"
} else {
  available_log_sheets[[1]]
}

reviewed_log <- readxl::read_excel(
  reviewed_log_file,
  sheet = log_sheet,
  col_types = "text",
  na = c("", "NA")
)

names(reviewed_log) <- trimws(
  names(reviewed_log)
)

required_log_columns <- c(
  "uuid",
  "question",
  "old_value",
  "change_type",
  "new_value"
)

missing_log_columns <- setdiff(
  required_log_columns,
  names(reviewed_log)
)

if (length(missing_log_columns) > 0) {
  stop(
    "Reviewed cleaning log is missing these columns: ",
    paste(
      missing_log_columns,
      collapse = ", "
    )
  )
}

if (nrow(reviewed_log) == 0) {
  stop(
    "The reviewed cleaning log contains no rows."
  )
}

################################################################################
# 8. STANDARDIZE REVIEW VALUES
################################################################################

reviewed_log <- reviewed_log |>
  dplyr::mutate(
    uuid = trimws(
      as.character(.data[["uuid"]])
    ),
    question = trimws(
      as.character(.data[["question"]])
    ),
    old_value = ifelse(
      is.na(.data[["old_value"]]),
      NA_character_,
      trimws(
        as.character(.data[["old_value"]])
      )
    ),
    change_type = tolower(
      trimws(
        as.character(.data[["change_type"]])
      )
    ),
    new_value = ifelse(
      is.na(.data[["new_value"]]),
      NA_character_,
      trimws(
        as.character(.data[["new_value"]])
      )
    )
  )

reviewed_log$uuid[
  reviewed_log$uuid == ""
] <- NA_character_

reviewed_log$question[
  reviewed_log$question == ""
] <- NA_character_

reviewed_log$change_type[
  reviewed_log$change_type == ""
] <- NA_character_

################################################################################
# 9. CONFIRM EVERY ROW WAS REVIEWED
################################################################################

unreviewed_rows <- reviewed_log |>
  dplyr::filter(
    is.na(.data[["change_type"]])
  )

if (nrow(unreviewed_rows) > 0) {
  
  openxlsx::write.xlsx(
    unreviewed_rows,
    here::here(
      "outputs",
      "unreviewed_cleaning_log_rows.xlsx"
    ),
    overwrite = TRUE
  )
  
  stop(
    nrow(unreviewed_rows),
    " cleaning-log rows have no change_type. ",
    "Complete the review and update ",
    "outputs/cleaning_log_reviewed.xlsx. ",
    "A copy of the incomplete rows was saved as ",
    "outputs/unreviewed_cleaning_log_rows.xlsx."
  )
}

missing_log_identifiers <- reviewed_log |>
  dplyr::filter(
    is.na(.data[["uuid"]]) |
      is.na(.data[["question"]])
  )

if (nrow(missing_log_identifiers) > 0) {
  
  openxlsx::write.xlsx(
    missing_log_identifiers,
    here::here(
      "outputs",
      "missing_log_identifiers.xlsx"
    ),
    overwrite = TRUE
  )
  
  stop(
    nrow(missing_log_identifiers),
    " cleaning-log rows have a missing uuid or question. ",
    "Open outputs/missing_log_identifiers.xlsx."
  )
}

################################################################################
# 10. VALIDATE CHANGE TYPES
################################################################################

allowed_change_types <- c(
  "no_action",
  "change_response",
  "blank_response",
  "remove_survey"
)

invalid_change_types <- setdiff(
  unique(
    stats::na.omit(
      reviewed_log$change_type
    )
  ),
  allowed_change_types
)

if (length(invalid_change_types) > 0) {
  stop(
    "Invalid change_type values: ",
    paste(
      invalid_change_types,
      collapse = ", "
    ),
    ". Allowed values are: ",
    paste(
      allowed_change_types,
      collapse = ", "
    )
  )
}

################################################################################
# 11. VALIDATE NEW VALUES
################################################################################

missing_new_values <- reviewed_log |>
  dplyr::filter(
    .data[["change_type"]] == "change_response",
    is.na(.data[["new_value"]]) |
      .data[["new_value"]] == ""
  )

if (nrow(missing_new_values) > 0) {
  
  openxlsx::write.xlsx(
    missing_new_values,
    here::here(
      "outputs",
      "missing_new_values.xlsx"
    ),
    overwrite = TRUE
  )
  
  stop(
    nrow(missing_new_values),
    " change_response rows have no new_value. ",
    "Open outputs/missing_new_values.xlsx."
  )
}

################################################################################
# 12. CHECK WHETHER LOG QUESTIONS EXIST IN THE DATASET
################################################################################

unknown_questions <- reviewed_log |>
  dplyr::filter(
    !.data[["question"]] %in% names(raw_data)
  ) |>
  dplyr::distinct(
    .data[["question"]]
  )

if (nrow(unknown_questions) > 0) {
  
  openxlsx::write.xlsx(
    unknown_questions,
    here::here(
      "outputs",
      "unknown_cleaning_log_questions.xlsx"
    ),
    overwrite = TRUE
  )
  
  stop(
    "Some cleaning-log questions do not exist in the editing dataset. ",
    "Open outputs/unknown_cleaning_log_questions.xlsx."
  )
}

################################################################################
# 13. CHECK WHETHER LOG UUIDS EXIST IN THE DATASET
################################################################################

unknown_uuids <- reviewed_log |>
  dplyr::filter(
    !.data[["uuid"]] %in% raw_data$uuid
  ) |>
  dplyr::distinct(
    .data[["uuid"]]
  )

if (nrow(unknown_uuids) > 0) {
  
  openxlsx::write.xlsx(
    unknown_uuids,
    here::here(
      "outputs",
      "unknown_cleaning_log_uuids.xlsx"
    ),
    overwrite = TRUE
  )
  
  stop(
    "Some cleaning-log UUIDs do not exist in the raw dataset. ",
    "Open outputs/unknown_cleaning_log_uuids.xlsx."
  )
}

################################################################################
# 14. CHECK FOR CONFLICTING DECISIONS
################################################################################

conflicting_decisions <- reviewed_log |>
  dplyr::mutate(
    decision_signature = paste(
      .data[["change_type"]],
      dplyr::coalesce(
        .data[["new_value"]],
        ""
      ),
      sep = " | "
    )
  ) |>
  dplyr::group_by(
    .data[["uuid"]],
    .data[["question"]]
  ) |>
  dplyr::summarise(
    number_of_decisions = dplyr::n_distinct(
      .data[["decision_signature"]]
    ),
    .groups = "drop"
  ) |>
  dplyr::filter(
    .data[["number_of_decisions"]] > 1
  )

if (nrow(conflicting_decisions) > 0) {
  
  conflicting_decision_details <- reviewed_log |>
    dplyr::inner_join(
      conflicting_decisions,
      by = c(
        "uuid",
        "question"
      )
    )
  
  openxlsx::write.xlsx(
    list(
      conflict_summary = conflicting_decisions,
      conflict_details = conflicting_decision_details
    ),
    here::here(
      "outputs",
      "conflicting_decisions.xlsx"
    ),
    overwrite = TRUE
  )
  
  stop(
    "Conflicting decisions were found for the same uuid and question. ",
    "Open outputs/conflicting_decisions.xlsx."
  )
}

################################################################################
# 15. REMOVE IDENTICAL DUPLICATE EDITING INSTRUCTIONS
################################################################################

reviewed_log_for_editing <- reviewed_log |>
  dplyr::distinct(
    .data[["uuid"]],
    .data[["question"]],
    .data[["change_type"]],
    .data[["new_value"]],
    .keep_all = TRUE
  )

message(
  "Reviewed cleaning-log rows: ",
  nrow(reviewed_log)
)

message(
  "Unique editing instructions: ",
  nrow(reviewed_log_for_editing)
)

################################################################################
# 16. APPLY REVIEWED CHANGES
################################################################################

clean_data <- cleaningtools::create_clean_data(
  raw_dataset = raw_data,
  raw_data_uuid_column = "uuid",
  cleaning_log = reviewed_log_for_editing,
  cleaning_log_uuid_column = "uuid",
  cleaning_log_question_column = "question",
  cleaning_log_new_value_column = "new_value",
  cleaning_log_change_type_column = "change_type",
  change_response_value = "change_response",
  NA_response_value = "blank_response",
  no_change_value = "no_action",
  remove_survey_value = "remove_survey"
)

if (!is.data.frame(clean_data)) {
  stop(
    "create_clean_data() did not return a data frame."
  )
}

if (!"uuid" %in% names(clean_data)) {
  stop(
    "The cleaned dataset does not contain the uuid column."
  )
}

message(
  "Reviewed changes were applied."
)

message(
  "Records before cleaning: ",
  nrow(raw_data)
)

message(
  "Records after cleaning: ",
  nrow(clean_data)
)

################################################################################
# 17. RECREATE SELECT-MULTIPLE PARENT FIELDS WHEN REQUIRED
################################################################################

select_multiple_edits <- reviewed_log_for_editing |>
  dplyr::filter(
    .data[["change_type"]] %in% c(
      "change_response",
      "blank_response"
    ),
    stringr::str_detect(
      .data[["question"]],
      stringr::fixed("/")
    )
  )

parent_correction_log <- tibble::tibble()

if (nrow(select_multiple_edits) > 0) {
  
  parent_result <- cleaningtools::recreate_parent_column(
    dataset = clean_data,
    uuid_column = "uuid",
    sm_separator = "/"
  )
  
  if (
    !is.list(parent_result) ||
    !"data_with_fix_concat" %in% names(parent_result)
  ) {
    stop(
      "recreate_parent_column() did not return ",
      "the expected data_with_fix_concat object."
    )
  }
  
  clean_data <- parent_result$data_with_fix_concat
  
  if (
    "correction_parent_sm_log" %in% names(parent_result) &&
    is.data.frame(
      parent_result$correction_parent_sm_log
    )
  ) {
    parent_correction_log <-
      parent_result$correction_parent_sm_log
  }
  
  message(
    "Select-multiple parent fields were rebuilt."
  )
  
} else {
  
  message(
    "No select-multiple child-field edits required parent reconstruction."
  )
}

################################################################################
# 18. SEPARATE DELETION DECISIONS
################################################################################

deletion_log <- reviewed_log_for_editing |>
  dplyr::filter(
    .data[["change_type"]] == "remove_survey"
  ) |>
  dplyr::distinct(
    .data[["uuid"]],
    .keep_all = TRUE
  )

cleaning_log_only <- reviewed_log_for_editing |>
  dplyr::filter(
    .data[["change_type"]] != "remove_survey"
  )

################################################################################
# 19. REVIEW WHETHER CLEANING WAS APPLIED CORRECTLY
################################################################################

if (nrow(deletion_log) > 0) {
  
  cleaning_review <- cleaningtools::review_cleaning(
    raw_dataset = raw_data,
    raw_dataset_uuid_column = "uuid",
    clean_dataset = clean_data,
    clean_dataset_uuid_column = "uuid",
    cleaning_log = cleaning_log_only,
    cleaning_log_uuid_column = "uuid",
    cleaning_log_change_type_column = "change_type",
    cleaning_log_question_column = "question",
    cleaning_log_new_value_column = "new_value",
    cleaning_log_old_value_column = "old_value",
    cleaning_log_no_change_value = c(
      "no_action",
      "no_change"
    ),
    deletion_log = deletion_log,
    deletion_log_uuid_column = "uuid",
    check_for_deletion_log = TRUE
  )
  
} else {
  
  cleaning_review <- cleaningtools::review_cleaning(
    raw_dataset = raw_data,
    raw_dataset_uuid_column = "uuid",
    clean_dataset = clean_data,
    clean_dataset_uuid_column = "uuid",
    cleaning_log = cleaning_log_only,
    cleaning_log_uuid_column = "uuid",
    cleaning_log_change_type_column = "change_type",
    cleaning_log_question_column = "question",
    cleaning_log_new_value_column = "new_value",
    cleaning_log_old_value_column = "old_value",
    cleaning_log_no_change_value = c(
      "no_action",
      "no_change"
    ),
    check_for_deletion_log = FALSE
  )
}

message(
  "Cleaning review completed."
)

################################################################################
# 20. SAVE CLEAN DATA
################################################################################

saveRDS(
  clean_data,
  clean_rds_file
)

clean_data_for_excel <- clean_data

posix_columns <- names(clean_data_for_excel)[
  vapply(
    clean_data_for_excel,
    inherits,
    logical(1),
    what = "POSIXt"
  )
]

if (length(posix_columns) > 0) {
  
  clean_data_for_excel[posix_columns] <- lapply(
    clean_data_for_excel[posix_columns],
    function(x) {
      ifelse(
        is.na(x),
        NA_character_,
        format(
          x,
          format = "%Y-%m-%d %H:%M:%S",
          tz = "UTC"
        )
      )
    }
  )
}

openxlsx::write.xlsx(
  list(
    clean_data = clean_data_for_excel
  ),
  clean_xlsx_file,
  overwrite = TRUE
)

################################################################################
# 21. SAVE THE CLEANING REVIEW
################################################################################

saveRDS(
  cleaning_review,
  cleaning_review_rds_file
)

if (is.data.frame(cleaning_review)) {
  
  review_workbook <- list(
    cleaning_review = cleaning_review
  )
  
} else if (is.list(cleaning_review)) {
  
  review_workbook <- cleaning_review[
    vapply(
      cleaning_review,
      is.data.frame,
      logical(1)
    )
  ]
  
  review_workbook <- review_workbook[
    !duplicated(
      names(review_workbook)
    )
  ]
  
  if (length(review_workbook) == 0) {
    
    review_workbook <- list(
      cleaning_review = tibble::tibble(
        message = paste0(
          "The cleaning review returned no ",
          "tabular discrepancy outputs."
        )
      )
    )
  }
  
} else {
  
  review_workbook <- list(
    cleaning_review = tibble::tibble(
      result = as.character(
        cleaning_review
      )
    )
  )
}

review_workbook <- lapply(
  review_workbook,
  function(x) {
    
    if (ncol(x) == 0) {
      return(
        tibble::tibble(
          message = "No discrepancies were returned."
        )
      )
    }
    
    x
  }
)

openxlsx::write.xlsx(
  review_workbook,
  cleaning_review_xlsx_file,
  overwrite = TRUE
)

################################################################################
# 22. SAVE DELETION AND PARENT-CORRECTION LOGS
################################################################################

deletion_log_to_save <- deletion_log

if (nrow(deletion_log_to_save) == 0) {
  
  deletion_log_to_save <- tibble::tibble(
    message = "No surveys were marked for removal."
  )
}

openxlsx::write.xlsx(
  list(
    deletion_log = deletion_log_to_save
  ),
  deletion_log_file,
  overwrite = TRUE
)

if (nrow(parent_correction_log) > 0) {
  
  openxlsx::write.xlsx(
    list(
      parent_corrections = parent_correction_log
    ),
    parent_correction_log_file,
    overwrite = TRUE
  )
}

################################################################################
# 23. CALCULATE REVIEW ISSUE COUNT
################################################################################

review_issue_count <- if (
  is.data.frame(cleaning_review)
) {
  
  nrow(cleaning_review)
  
} else if (is.list(cleaning_review)) {
  
  sum(
    vapply(
      cleaning_review,
      function(x) {
        
        if (is.data.frame(x)) {
          nrow(x)
        } else {
          0L
        }
      },
      integer(1)
    )
  )
  
} else {
  
  NA_integer_
}

################################################################################
# 24. FINAL STATUS
################################################################################

message(
  paste0(
    "\n",
    paste(rep("=", 70), collapse = ""),
    "\nLTV DATA EDITING COMPLETED\n",
    paste(rep("=", 70), collapse = "")
  )
)

message(
  "Original raw records: ",
  nrow(raw_data)
)

message(
  "Final clean records: ",
  nrow(clean_data)
)

message(
  "Removed records: ",
  nrow(raw_data) - nrow(clean_data)
)

message(
  "Reviewed log rows: ",
  nrow(reviewed_log)
)

message(
  "Unique editing instructions: ",
  nrow(reviewed_log_for_editing)
)

if (
  !is.na(review_issue_count) &&
  review_issue_count == 0
) {
  
  message(
    "Cleaning completed with no review discrepancies."
  )
  
} else if (!is.na(review_issue_count)) {
  
  warning(
    "Cleaning completed, but ",
    review_issue_count,
    " rows were returned across the cleaning-review outputs. ",
    "Open outputs/cleaning_review.xlsx."
  )
  
} else {
  
  warning(
    "Cleaning completed, but the number of review issues ",
    "could not be calculated. ",
    "Open outputs/cleaning_review.xlsx."
  )
}

message(
  "Clean Excel dataset: ",
  clean_xlsx_file
)

message(
  "Clean R dataset: ",
  clean_rds_file
)

message(
  "Cleaning review: ",
  cleaning_review_xlsx_file
)

message(
  "Deletion log: ",
  deletion_log_file
)

message(
  paste(rep("=", 70), collapse = "")
)