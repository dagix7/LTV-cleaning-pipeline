rm(list = ls())

source(here::here("utils", "helpers.R"))

packages <- c("dplyr", "tidyr", "stringr", "readxl", "openxlsx", "readr", 
              "here", "tibble", "cleaningtools")
load_packages(packages)

options(scipen = 999, dplyr.summarise.inform = FALSE)

raw_file <- here("data", "raw", "LTV_Household_V2_IMPACT_training_diversified.xlsx")
reviewed_log_file <- here("outputs", "cleaning_log_reviewed.xlsx")
clean_rds_file <- here("data", "clean", "LTV_clean_data.rds")
clean_xlsx_file <- here("data", "clean", "LTV_clean_data.xlsx")
cleaning_review_file <- here("outputs", "cleaning_review.xlsx")
deletion_log_file <- here("outputs", "deletion_log.xlsx")

dir.create(here("data", "clean"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("outputs"), recursive = TRUE, showWarnings = FALSE)

if (!file.exists(raw_file)) stop("Raw data not found: ", raw_file)
if (!file.exists(reviewed_log_file)) stop("Reviewed log not found: ", reviewed_log_file)

raw_data <- readxl::read_excel(raw_file, sheet = "LTV Household V2 Form (Round1)", 
                                col_types = "text", na = c("", "NA"))

raw_data <- raw_data |>
  dplyr::mutate(
    uuid = trimws(as.character(`_id`)),
    submission_uuid = trimws(as.character(`_uuid`)),
    start = convert_datetime(start),
    end = convert_datetime(end)
  )

if (any(is.na(raw_data$uuid) | raw_data$uuid == "")) stop("Missing _id values found")
if (anyDuplicated(raw_data$uuid)) stop("Duplicate _id values found")

numeric_vars <- intersect(
  c("resp_age", "hoh_age", "hh_size", "hh_children_u18", "hh_children_u5", 
    "disp_count", "_hh_location_latitude", "_hh_location_longitude", 
    "_hh_location_altitude", "_hh_location_precision", "amount_recieved"),
  names(raw_data)
)

raw_data[numeric_vars] <- lapply(raw_data[numeric_vars], function(x) {
  readr::parse_number(as.character(x), na = c("", "NA", "N/A", "NULL"))
})

raw_data <- raw_data |>
  dplyr::mutate(
    duration_minutes = ifelse(!is.na(start) & !is.na(end),
                              as.numeric(difftime(end, start, units = "mins")),
                              NA_real_)
  )

message("Imported ", nrow(raw_data), " records")

reviewed_log <- readxl::read_excel(reviewed_log_file, sheet = "cleaning_log", col_types = "text", na = c("", "NA"))

reviewed_log <- reviewed_log |>
  dplyr::mutate(
    uuid = trimws(as.character(uuid)),
    question = trimws(as.character(question)),
    old_value = trimws(as.character(old_value)),
    change_type = tolower(trimws(as.character(change_type))),
    new_value = trimws(as.character(new_value))
  )

if (any(is.na(reviewed_log$change_type))) {
  stop("Unreviewed rows found. All rows must have a change_type.")
}

allowed_types <- c("no_action", "change_response", "blank_response", "remove_survey")
invalid_types <- setdiff(unique(reviewed_log$change_type), allowed_types)
if (length(invalid_types) > 0) {
  stop("Invalid change_type values: ", paste(invalid_types, collapse = ", "))
}

missing_new_values <- reviewed_log |>
  dplyr::filter(change_type == "change_response", is.na(new_value) | new_value == "")

if (nrow(missing_new_values) > 0) {
  stop("change_response rows missing new_value: ", nrow(missing_new_values))
}

unknown_questions <- setdiff(reviewed_log$question, names(raw_data))
if (length(unknown_questions) > 0) {
  stop("Unknown questions in log: ", paste(head(unknown_questions, 5), collapse = ", "))
}

unknown_uuids <- setdiff(reviewed_log$uuid, raw_data$uuid)
if (length(unknown_uuids) > 0) {
  stop("Unknown UUIDs in log: ", length(unknown_uuids))
}

reviewed_log_unique <- reviewed_log |>
  dplyr::distinct(uuid, question, change_type, new_value, .keep_all = TRUE)

message("Applying ", nrow(reviewed_log_unique), " unique edits")

clean_data <- cleaningtools::create_clean_data(
  raw_dataset = raw_data,
  raw_data_uuid_column = "uuid",
  cleaning_log = reviewed_log_unique,
  cleaning_log_uuid_column = "uuid",
  cleaning_log_question_column = "question",
  cleaning_log_new_value_column = "new_value",
  cleaning_log_change_type_column = "change_type",
  change_response_value = "change_response",
  NA_response_value = "blank_response",
  no_change_value = "no_action",
  remove_survey_value = "remove_survey"
)

select_multiple_edits <- reviewed_log_unique |>
  dplyr::filter(change_type %in% c("change_response", "blank_response"),
                stringr::str_detect(question, stringr::fixed("/")))

if (nrow(select_multiple_edits) > 0) {
  parent_result <- cleaningtools::recreate_parent_column(
    dataset = clean_data,
    uuid_column = "uuid",
    sm_separator = "/"
  )
  clean_data <- parent_result$data_with_fix_concat
  message("Reconstructed select-multiple parent fields")
}

deletion_log <- reviewed_log_unique |>
  dplyr::filter(change_type == "remove_survey") |>
  dplyr::distinct(uuid, .keep_all = TRUE)

cleaning_log_only <- reviewed_log_unique |>
  dplyr::filter(change_type != "remove_survey")

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
    cleaning_log_no_change_value = c("no_action", "no_change"),
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
    cleaning_log_no_change_value = c("no_action", "no_change"),
    check_for_deletion_log = FALSE
  )
}

saveRDS(clean_data, clean_rds_file)

clean_data_export <- clean_data
posix_cols <- names(clean_data_export)[vapply(clean_data_export, inherits, logical(1), "POSIXt")]

if (length(posix_cols) > 0) {
  clean_data_export[posix_cols] <- lapply(clean_data_export[posix_cols], function(x) {
    format(x, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  })
}

openxlsx::write.xlsx(list(clean_data = clean_data_export), clean_xlsx_file, overwrite = TRUE)

if (is.data.frame(cleaning_review)) {
  review_wb <- list(cleaning_review = cleaning_review)
} else if (is.list(cleaning_review)) {
  review_wb <- cleaning_review[vapply(cleaning_review, is.data.frame, logical(1))]
} else {
  review_wb <- list(cleaning_review = tibble::tibble(result = as.character(cleaning_review)))
}

openxlsx::write.xlsx(review_wb, cleaning_review_file, overwrite = TRUE)

openxlsx::write.xlsx(
  list(deletion_log = if(nrow(deletion_log) > 0) deletion_log else 
         tibble::tibble(message = "No surveys removed")),
  deletion_log_file, overwrite = TRUE
)

message("\n", paste(rep("=", 60), collapse = ""))
message("LTV DATA CLEANING COMPLETE")
message(paste(rep("=", 60), collapse = ""))
message("Raw records: ", nrow(raw_data))
message("Clean records: ", nrow(clean_data))
message("Removed: ", nrow(raw_data) - nrow(clean_data))
message("\nOutputs:")
message("  Clean data (Excel): ", clean_xlsx_file)
message("  Clean data (RDS): ", clean_rds_file)
message("  Review: ", cleaning_review_file)
message(paste(rep("=", 60), collapse = ""))