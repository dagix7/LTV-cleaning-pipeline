convert_datetime <- function(x, tz = 'UTC') {
  if (length(x) == 0) return(as.POSIXct(character(0), tz = tz))
  if (inherits(x, c('POSIXt', 'Date'))) return(as.POSIXct(x, tz = tz))
  
  x <- trimws(as.character(x))
  x[is.na(x) | x == '' | toupper(x) %in% c('NA', 'N/A', 'NULL')] <- NA_character_
  
  result <- rep(NA_real_, length(x))
  numeric_vals <- suppressWarnings(as.numeric(x))
  excel_idx <- which(!is.na(numeric_vals) & numeric_vals >= 1 & numeric_vals <= 100000)
  
  if (length(excel_idx) > 0) {
    result[excel_idx] <- as.numeric(as.POSIXct(numeric_vals[excel_idx] * 86400, 
                                                 origin = '1899-12-30', tz = tz))
  }
  
  formats <- c(
    '%Y-%m-%d %H:%M:%OS', '%Y-%m-%d %H:%M:%S', '%Y-%m-%d %H:%M',
    '%Y/%m/%d %H:%M:%S', '%Y-%m-%dT%H:%M:%S', '%Y-%m-%dT%H:%M:%SZ',
    '%d/%m/%Y %H:%M:%S', '%d/%m/%Y %H:%M', '%m/%d/%Y %H:%M:%S'
  )
  
  for (fmt in formats) {
    remaining <- which(is.na(result) & !is.na(x))
    if (length(remaining) == 0) break
    
    parsed <- suppressWarnings(as.POSIXct(strptime(x[remaining], fmt, tz = tz)))
    success <- which(!is.na(parsed))
    if (length(success) > 0) result[remaining[success]] <- as.numeric(parsed[success])
  }
  
  as.POSIXct(result, origin = '1970-01-01', tz = tz)
}

load_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop('Missing packages: ', paste(missing, collapse = ', '), 
         '. Run source(\"00_setup.R\") first.')
  }
  invisible(lapply(packages, library, character.only = TRUE))
}