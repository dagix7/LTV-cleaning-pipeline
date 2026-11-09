packages <- c("dplyr", "tidyr", "purrr", "stringr", "readxl", "openxlsx", 
              "lubridate", "readr", "here", "rlang", "tibble")

missing <- setdiff(packages, rownames(installed.packages()))
if (length(missing) > 0) install.packages(missing)

if (!requireNamespace("cleaningtools", quietly = TRUE)) {
  if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
  pak::pkg_install("impact-initiatives/cleaningtools")
}

message("Setup complete. Restart R before running data checking.")
