################################################################################
# Install packages
################################################################################

cran_packages <- c(
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
  "pak"
)

missing_packages <- setdiff(
  cran_packages,
  rownames(installed.packages())
)

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

if (!requireNamespace("cleaningtools", quietly = TRUE)) {
  pak::pkg_install(
    "impact-initiatives/cleaningtools"
  )
}

message("Setup complete.")
message("Restart R before running the checking script.")