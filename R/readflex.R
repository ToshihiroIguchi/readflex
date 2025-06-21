#' Flexible CSV reader with auto-detected encoding
#'
#' @description
#' `readflex()` attempts to read a CSV file by automatically detecting its character encoding
#' (using `readr::guess_encoding()` or `stringi::stri_enc_detect()`) and falls back to
#' a user-specified list of common encodings if detection fails.
#'
#' @param file Character. Path to the CSV file to read.
#' @param ... Additional arguments passed to `utils::read.csv()`, such as `sep`, `header`, etc.
#' @param encodings Character vector. Common encodings to try if auto-detection fails.
#' @param guess_n_max Integer. Number of lines to sample when guessing encoding. Default is 1000.
#' @param verbose Logical. If TRUE, prints messages about detection and trial attempts. Default FALSE.
#' @param stringsAsFactors Logical. Should character vectors be converted to factors? Default FALSE.
#' @param max_file_size_mb Numeric. Maximum file size in MB to prevent memory issues in Shiny apps. Default 100.
#' @return A `data.frame` containing the imported CSV data.
#' @examples
#' # Basic usage (auto-detect encoding)
#' df <- readflex("data.csv")
#'
#' # Specify additional read.csv options and enable verbose output
#' df <- readflex("data.csv", sep = ";", stringsAsFactors = FALSE, verbose = TRUE)
#'
#' # For Shiny apps, set a custom file size limit
#' df <- readflex("data.csv", max_file_size_mb = 50)
#'
#' @import utils
#' @importFrom readr guess_encoding read_lines
#' @importFrom stringi stri_enc_detect
#' @export
readflex <- function(file,
                     ...,
                     encodings = c(
                       "UTF-8", "UTF-8-BOM", "UTF-16LE", "UTF-16BE",
                       "Shift_JIS", "CP932", "EUC-JP", "ISO-2022-JP",
                       "ISO-8859-1", "Windows-1252", "latin1",
                       "GB18030", "GB2312", "GBK", "Big5", "Big5-HKSCS",
                       "EUC-KR", "ISO-2022-KR"
                     ),
                     guess_n_max = 1000,
                     verbose = FALSE,
                     stringsAsFactors = FALSE,
                     max_file_size_mb = 100) { # Added parameter for factor conversion control
  stopifnot(is.character(file), length(file) == 1)
  stopifnot(is.numeric(guess_n_max), guess_n_max > 0)
  stopifnot(is.logical(verbose), length(verbose) == 1)
  stopifnot(is.character(encodings), length(encodings) > 0)
  stopifnot(is.logical(stringsAsFactors), length(stringsAsFactors) == 1)
  stopifnot(is.numeric(max_file_size_mb), max_file_size_mb > 0)
  
  # Check file existence
  if (!file.exists(file)) {
    stop(sprintf("[readflex] File not found: %s", file))
  }
  
  # Check for empty file
  if (file.size(file) == 0) {
    warning(sprintf("[readflex] File is empty: %s", file))
    return(data.frame())
  }
  
  # Check file size limit (important for Shiny apps)
  file_size_mb <- file.size(file) / (1024 * 1024)
  if (file_size_mb > max_file_size_mb) {
    stop(sprintf(
      "[readflex] File size (%.1f MB) exceeds limit (%.1f MB). Consider using a smaller file or increasing max_file_size_mb parameter.",
      file_size_mb, max_file_size_mb
    ))
  }

  # Helper: try reading with a given encoding
  try_read <- function(enc) {
    if (verbose) message(sprintf("[readflex] Trying encoding: %s", enc))
    tryCatch(
      utils::read.csv(
        file,
        fileEncoding = enc,
        ...,
        stringsAsFactors = stringsAsFactors # Apply stringsAsFactors setting
      ),
      error   = function(e) e,
      warning = function(w) w
    )
  }


  # 1) Auto-detect encodings
  detected <- character(0)
  if (requireNamespace("readr", quietly = TRUE)) {
    info <- readr::guess_encoding(file, n_max = guess_n_max)
    if (nrow(info) > 0) {
      detected <- unique(info$encoding)
      if (verbose) message("[readflex] Detected with readr: ", paste(detected, collapse = ", "))
    }
  }
  if (length(detected) == 0 && requireNamespace("stringi", quietly = TRUE)) {
    txt <- tryCatch(base::readLines(file, n = guess_n_max, warn = FALSE),
                    error = function(e) character(0))
    if (length(txt) > 0) {
      info2 <- stringi::stri_enc_detect(paste(txt, collapse = "\n"))[[1]]
      detected <- unique(info2$Encoding[order(-info2$Confidence)])
      if (verbose) message("[readflex] Detected with stringi: ", paste(detected, collapse = ", "))
    }
  }

  # 2) Build trial list
  trial_encs <- unique(c(detected, encodings))
  if (verbose) message("[readflex] Trial order: ", paste(trial_encs, collapse = ", "))

  # 3) Loop through encodings
  for (enc in trial_encs) {
    res <- try_read(enc)
    if (inherits(res, "data.frame")) {
      if (verbose) message(sprintf("[readflex] Success with: %s", enc))
      else if (verbose) message(sprintf("[readflex] Read OK (encoding: %s)", enc))
      return(res)
    }
  }

  stop(sprintf(
    "[readflex] Failed to read '%s'. Tried encodings: %s",
    file, paste(trial_encs, collapse = ", ")
  ))
}
