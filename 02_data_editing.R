################################################################################
# LTV DATA EDITING
################################################################################

rm(list = ls())

################################################################################
# 1. Load packages
################################################################################

required_packages <- c(
  "dplyr",
  "tidyr",
  "stringr",
  "readxl",
  "openxlsx",
  "lubridate",
  "readr",
  "here",
  "cleaningtools"
)

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

dir.create(
  here::here("data", "clean"),
  recursive = TRUE,
  showWarnings = FALSE
)

################################################################################
# 3. Confirm files exist
################################################################################

if (!file.exists(raw_file)) {
  stop(
    "Raw workbook not found in data/raw/."
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
# 4. Re-import untouched raw data
################################################################################

raw_data <- readxl::read_excel(
  raw_file,
  sheet = "LTV Household V2 Form (Round1)",
  na = c("", "NA")
)

raw_data <- raw_data |>
  dplyr::mutate(
    uuid = as.character(.data[["_id"]]),
    submission_uuid =
      as.character(.data[["_uuid"]])
  )

################################################################################
# 5. Read reviewed cleaning log
################################################################################

available_sheets <- readxl::excel_sheets(
  reviewed_log_file
)

log_sheet <- if (
  "cleaning_log" %in%
  available_sheets
) {
  "cleaning_log"
} else {
  available_sheets[[1]]
}

reviewed_log <- readxl::read_excel(
  reviewed_log_file,
  sheet = log_sheet,
  col_types = "text"
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
    "Reviewed log is missing: ",
    paste(
      missing_log_columns,
      collapse = ", "
    )
  )
}

################################################################################
# 6. Confirm all rows were reviewed
################################################################################

unreviewed_rows <- reviewed_log |>
  dplyr::filter(
    is.na(.data[["change_type"]]) |
      trimws(
        .data[["change_type"]]
      ) == ""
  )

if (nrow(unreviewed_rows) > 0) {
  stop(
    nrow(unreviewed_rows),
    " rows still have no change_type."
  )
}

################################################################################
# 7. Validate change types
################################################################################

allowed_change_types <- c(
  "no_action",
  "change_response",
  "blank_response",
  "remove_survey"
)

invalid_change_types <- setdiff(
  unique(
    reviewed_log$change_type
  ),
  allowed_change_types
)

if (length(invalid_change_types) > 0) {
  stop(
    "Invalid change_type values: ",
    paste(
      invalid_change_types,
      collapse = ", "
    )
  )
}

################################################################################
# 8. Check conflicting decisions
################################################################################

conflicting_decisions <- reviewed_log |>
  dplyr::group_by(
    .data[["uuid"]],
    .data[["question"]]
  ) |>
  dplyr::summarise(
    decisions = dplyr::n_distinct(
      paste(
        .data[["change_type"]],
        .data[["new_value"]]
      )
    ),
    .groups = "drop"
  ) |>
  dplyr::filter(
    .data[["decisions"]] > 1
  )

if (nrow(conflicting_decisions) > 0) {
  
  openxlsx::write.xlsx(
    conflicting_decisions,
    here::here(
      "outputs",
      "conflicting_decisions.xlsx"
    ),
    overwrite = TRUE
  )
  
  stop(
    "Conflicting decisions found. ",
    "Open outputs/conflicting_decisions.xlsx."
  )
}

################################################################################
# 9. Apply reviewed changes
################################################################################

clean_data <- cleaningtools::create_clean_data(
  raw_dataset = raw_data,
  raw_data_uuid_column = "uuid",
  cleaning_log = reviewed_log,
  cleaning_log_uuid_column = "uuid",
  cleaning_log_question_column = "question",
  cleaning_log_new_value_column = "new_value",
  cleaning_log_change_type_column =
    "change_type",
  change_response_value =
    "change_response",
  NA_response_value =
    "blank_response",
  no_change_value =
    "no_action",
  remove_survey_value =
    "remove_survey"
)

# create_clean_data() applies the reviewed log to a copy of the raw data.
# 【7-c97e20】

################################################################################
# 10. Recreate select-multiple parent fields if their child fields were edited
################################################################################

select_multiple_edits <- reviewed_log |>
  dplyr::filter(
    .data[["change_type"]] %in%
      c(
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
  
  parent_result <-
    cleaningtools::recreate_parent_column(
      dataset = clean_data,
      uuid_column = "uuid",
      sm_separator = "/"
    )
  
  clean_data <-
    parent_result$data_with_fix_concat
  
  parent_correction_log <-
    parent_result$correction_parent_sm_log
}

# recreate_parent_column() can identify select-multiple parents and children
# from the "/" separator without requiring the XLSForm. 【8-b460cb】【9-25d15a】

################################################################################
# 11. Separate deletion decisions
################################################################################

deletion_log <- reviewed_log |>
  dplyr::filter(
    .data[["change_type"]] ==
      "remove_survey"
  ) |>
  dplyr::distinct(
    .data[["uuid"]],
    .keep_all = TRUE
  )

cleaning_log_only <- reviewed_log |>
  dplyr::filter(
    .data[["change_type"]] !=
      "remove_survey"
  )

################################################################################
# 12. Review the cleaning
################################################################################

cleaning_review <-
  cleaningtools::review_cleaning(
    raw_dataset = raw_data,
    raw_dataset_uuid_column = "uuid",
    clean_dataset = clean_data,
    clean_dataset_uuid_column = "uuid",
    cleaning_log =
      cleaning_log_only,
    cleaning_log_uuid_column =
      "uuid",
    cleaning_log_change_type_column =
      "change_type",
    cleaning_log_question_column =
      "question",
    cleaning_log_new_value_column =
      "new_value",
    cleaning_log_old_value_column =
      "old_value",
    cleaning_log_no_change_value =
      c(
        "no_action",
        "no_change"
      ),
    deletion_log = if (
      nrow(deletion_log) > 0
    ) {
      deletion_log
    } else {
      NULL
    },
    deletion_log_uuid_column = if (
      nrow(deletion_log) > 0
    ) {
      "uuid"
    } else {
      NULL
    },
    check_for_deletion_log =
      nrow(deletion_log) > 0
  )

# review_cleaning() identifies unapplied edits, unresolved deletions and log/data
# mismatches. 【10-7ac5c5】

################################################################################
# 13. Save clean data
################################################################################

saveRDS(
  clean_data,
  clean_rds_file
)

openxlsx::write.xlsx(
  clean_data,
  clean_xlsx_file,
  overwrite = TRUE
)

################################################################################
# 14. Save review outputs
################################################################################

saveRDS(
  cleaning_review,
  here::here(
    "outputs",
    "cleaning_review.rds"
  )
)

openxlsx::write.xlsx(
  cleaning_review,
  here::here(
    "outputs",
    "cleaning_review.xlsx"
  ),
  overwrite = TRUE
)

openxlsx::write.xlsx(
  deletion_log,
  here::here(
    "outputs",
    "deletion_log.xlsx"
  ),
  overwrite = TRUE
)

if (nrow(parent_correction_log) > 0) {
  
  openxlsx::write.xlsx(
    parent_correction_log,
    here::here(
      "outputs",
      "select_multiple_parent_corrections.xlsx"
    ),
    overwrite = TRUE
  )
}

################################################################################
# Final message
################################################################################

if (nrow(cleaning_review) == 0) {
  
  message(
    "Cleaning completed successfully."
  )
  
} else {
  
  warning(
    "Cleaning discrepancies remain. ",
    "Open outputs/cleaning_review.xlsx."
  )
}

message(
  "Clean dataset: ",
  clean_xlsx_file
)