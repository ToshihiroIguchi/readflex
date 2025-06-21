# Source all required modules
if (file.exists("R/performance.R")) source("R/performance.R")
if (file.exists("R/diagnostics.R")) source("R/diagnostics.R")  
if (file.exists("R/config.R")) source("R/config.R")

#' Enhanced flexible CSV reader with comprehensive features
#'
#' @description
#' `readflex_enhanced()` is an advanced version of readflex that includes
#' performance optimizations, caching, diagnostics, and enhanced error handling.
#'
#' @param file Character. Path to the CSV file to read.
#' @param ... Additional arguments passed to `utils::read.csv()`.
#' @param encodings Character vector. Encodings to try if auto-detection fails.
#'   If NULL, uses global configuration.
#' @param guess_n_max Integer. Number of lines to sample when guessing encoding.
#' @param verbose Logical. If TRUE, prints detailed processing information.
#' @param stringsAsFactors Logical. Should character vectors be converted to factors?
#' @param max_file_size_mb Numeric. Maximum file size in MB.
#' @param use_cache Logical. Whether to use encoding cache.
#' @param use_parallel Logical. Whether to use parallel processing.
#' @param auto_fix Logical. Whether to attempt automatic error fixes.
#' @param validate_data Logical. Whether to perform data validation.
#' @param profile Character. Regional encoding profile to use ("auto", "japan", etc.).
#' @return A `data.frame` with additional attributes for encoding and processing info.
#' 
#' @examples
#' \dontrun{
#' # Basic usage with auto-detection
#' df <- readflex_enhanced("your_data.csv")
#' 
#' # With regional profile
#' df <- readflex_enhanced("your_japanese_data.csv", profile = "japan")
#' 
#' # With verbose output and validation
#' df <- readflex_enhanced("your_data.csv", verbose = TRUE, validate_data = TRUE)
#' 
#' # With auto-fix for problematic files
#' df <- readflex_enhanced("your_problematic_file.csv", auto_fix = TRUE)
#' }
#' 
#' @export
readflex_enhanced <- function(file,
                            ...,
                            encodings = NULL,
                            guess_n_max = NULL,
                            verbose = NULL,
                            stringsAsFactors = NULL,
                            max_file_size_mb = NULL,
                            use_cache = NULL,
                            use_parallel = NULL,
                            auto_fix = FALSE,
                            validate_data = FALSE,
                            profile = NULL) {
  
  # Record start time for performance tracking
  start_time <- Sys.time()
  
  # Load configuration defaults
  config <- get_readflex_config()
  
  # Apply parameter defaults from configuration
  if (is.null(encodings)) encodings <- config$default_encodings
  if (is.null(guess_n_max)) guess_n_max <- config$guess_n_max
  if (is.null(verbose)) verbose <- config$verbose_level > 0
  if (is.null(stringsAsFactors)) stringsAsFactors <- FALSE
  if (is.null(max_file_size_mb)) max_file_size_mb <- config$max_file_size_mb
  if (is.null(use_cache)) use_cache <- config$cache_enabled
  if (is.null(use_parallel)) use_parallel <- config$parallel_enabled
  
  # Apply regional profile if specified
  if (!is.null(profile) && profile != "auto") {
    profile_config <- readflex_profile(profile, apply_immediately = FALSE)
    encodings <- profile_config$encodings
    if (verbose) {
      message(sprintf("[readflex] Applied %s profile", profile_config$name))
    }
  } else if (!is.null(profile) && profile == "auto") {
    detected_region <- detect_system_locale()
    profile_config <- readflex_profile(detected_region, apply_immediately = FALSE)
    encodings <- profile_config$encodings
    if (verbose) {
      message(sprintf("[readflex] Auto-detected %s profile", profile_config$name))
    }
  }
  
  # Parameter validation
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
    result <- data.frame()
    attr(result, "encoding") <- "empty"
    attr(result, "processing_info") <- list(
      file_size = 0,
      encoding_detected = "empty",
      processing_time = 0,
      cache_used = FALSE
    )
    return(result)
  }
  
  # Check file size limit
  file_size_mb <- file.size(file) / (1024 * 1024)
  if (file_size_mb > max_file_size_mb) {
    stop(sprintf(
      "[readflex] File size (%.1f MB) exceeds limit (%.1f MB). Consider using max_file_size_mb parameter.",
      file_size_mb, max_file_size_mb
    ))
  }
  
  # Check cache first
  cached_result <- NULL
  if (use_cache) {
    cached_result <- get_cached_encoding(file)
    if (!is.null(cached_result) && verbose) {
      message(sprintf("[readflex] Using cached encoding: %s (confidence: %.1f%%)", 
                     cached_result$encoding, cached_result$confidence * 100))
    }
  }
  
  # Determine encoding order
  if (!is.null(cached_result)) {
    # Prioritize cached encoding
    trial_encs <- unique(c(cached_result$encoding, encodings))
  } else {
    # Use smart ordering based on content analysis
    sample_content <- tryCatch({
      readLines(file, n = min(10, guess_n_max), warn = FALSE)
    }, error = function(e) character(0))
    
    if (length(sample_content) > 0) {
      smart_order <- smart_encoding_order(paste(sample_content, collapse = " "), file_size_mb)
      trial_encs <- unique(c(smart_order, encodings))
    } else {
      trial_encs <- encodings
    }
  }
  
  if (verbose) {
    message(sprintf("[readflex] Trial order: %s", paste(head(trial_encs, 10), collapse = ", ")))
    if (length(trial_encs) > 10) {
      message(sprintf("[readflex] ... and %d more encodings", length(trial_encs) - 10))
    }
  }
  
  # Try parallel processing first for large files
  successful_encoding <- NULL
  if (use_parallel && file_size_mb > 1 && length(trial_encs) > 5) {
    if (verbose) message("[readflex] Attempting parallel encoding detection...")
    
    successful_encoding <- parallel_encoding_detection(
      file, trial_encs, 
      n_cores = NULL, 
      chunk_size = 3
    )
    
    if (!is.null(successful_encoding) && verbose) {
      message(sprintf("[readflex] Parallel detection successful: %s", successful_encoding))
    }
  }
  
  # Helper function for reading with encoding
  try_read <- function(enc) {
    if (verbose) message(sprintf("[readflex] Trying encoding: %s", enc))
    tryCatch({
      utils::read.csv(
        file,
        fileEncoding = enc,
        ...,
        stringsAsFactors = stringsAsFactors
      )
    }, error = function(e) e, warning = function(w) w)
  }
  
  # Sequential encoding attempts
  result <- NULL
  used_encoding <- NULL
  
  if (!is.null(successful_encoding)) {
    # Use parallel result
    result <- try_read(successful_encoding)
    if (inherits(result, "data.frame")) {
      used_encoding <- successful_encoding
    }
  }
  
  if (is.null(result)) {
    # Sequential attempts
    for (enc in trial_encs) {
      test_result <- try_read(enc)
      if (inherits(test_result, "data.frame")) {
        result <- test_result
        used_encoding <- enc
        if (verbose) {
          message(sprintf("[readflex] Success with: %s", enc))
        }
        break
      }
    }
  }
  
  # Auto-fix attempt if reading failed
  if (is.null(result) && auto_fix) {
    if (verbose) message("[readflex] Attempting auto-fix...")
    
    tryCatch({
      result <- readflex_auto_fix(file, 
                                encodings = trial_encs,
                                verbose = verbose,
                                stringsAsFactors = stringsAsFactors,
                                ...)
      used_encoding <- "auto-fixed"
      if (verbose) message("[readflex] Auto-fix successful")
    }, error = function(e) {
      if (verbose) message(sprintf("[readflex] Auto-fix failed: %s", e$message))
    })
  }
  
  # Final error if nothing worked
  if (is.null(result)) {
    stop(sprintf(
      "[readflex] Failed to read '%s'. Tried encodings: %s",
      file, paste(head(trial_encs, 10), collapse = ", ")
    ))
  }
  
  # Cache successful encoding
  if (use_cache && !is.null(used_encoding) && used_encoding != "auto-fixed") {
    cache_encoding_result(file, used_encoding, confidence = 0.9)
  }
  
  # Calculate processing time
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Record performance statistics
  if (config$stats_enabled) {
    record_performance("readflex_enhanced", processing_time, file_size_mb * 1024^2, used_encoding)
  }
  
  # Add processing information as attributes
  attr(result, "encoding") <- used_encoding
  attr(result, "processing_info") <- list(
    file_size_mb = file_size_mb,
    encoding_detected = used_encoding,
    processing_time_seconds = round(processing_time, 3),
    cache_used = !is.null(cached_result),
    parallel_used = !is.null(successful_encoding),
    total_encodings_tested = length(trial_encs),
    profile_used = profile
  )
  
  # Data validation if requested
  if (validate_data) {
    if (verbose) message("[readflex] Performing data validation...")
    
    validation_result <- validate_readflex_data(result, strict = FALSE)
    attr(result, "validation") <- validation_result
    
    if (!validation_result$valid) {
      warning(sprintf("Data validation found %d errors. Check attr(result, 'validation') for details.",
                     validation_result$summary$error_count))
    }
    
    if (verbose && length(validation_result$warnings) > 0) {
      message(sprintf("[readflex] Data validation: %d warnings", 
                     length(validation_result$warnings)))
    }
  }
  
  # Verbose summary
  if (verbose) {
    message(sprintf("[readflex] Complete! %d rows, %d columns in %.3f seconds", 
                   nrow(result), ncol(result), processing_time))
  }
  
  return(result)
}

#' Get processing information from readflex result
#' 
#' @param data Data frame returned by readflex_enhanced
#' @return List with processing information
get_readflex_info <- function(data) {
  info <- attr(data, "processing_info")
  validation <- attr(data, "validation")
  
  result <- list(
    encoding = attr(data, "encoding"),
    processing_info = info,
    validation = validation
  )
  
  class(result) <- "readflex_info"
  return(result)
}

#' Print method for readflex info
#' 
#' @param x readflex_info object
#' @param ... Additional arguments
print.readflex_info <- function(x, ...) {
  cat("READFLEX PROCESSING INFORMATION\n")
  cat(rep("=", 45), "\n")
  
  cat(sprintf("Encoding used: %s\n", x$encoding))
  
  if (!is.null(x$processing_info)) {
    info <- x$processing_info
    cat(sprintf("File size: %.2f MB\n", info$file_size_mb))
    cat(sprintf("  Processing time: %.3f seconds\n", info$processing_time_seconds))
    cat(sprintf(" Cache used: %s\n", if (info$cache_used) "Yes" else "No"))
    cat(sprintf(" Parallel processing: %s\n", if (info$parallel_used) "Yes" else "No"))
    cat(sprintf(" Encodings tested: %d\n", info$total_encodings_tested))
    
    if (!is.null(info$profile_used)) {
      cat(sprintf(" Profile used: %s\n", info$profile_used))
    }
  }
  
  if (!is.null(x$validation)) {
    val <- x$validation
    cat(sprintf(" Data validation: %s\n", if (val$valid) "PASSED" else "FAILED"))
    cat(sprintf(" Quality score: %.1f%%\n", val$summary$quality_score * 100))
    
    if (val$summary$error_count > 0) {
      cat(sprintf(" Errors: %d\n", val$summary$error_count))
    }
    
    if (val$summary$warning_count > 0) {
      cat(sprintf("  Warnings: %d\n", val$summary$warning_count))
    }
  }
  
  cat(rep("=", 45), "\n")
}

#' Backward compatibility wrapper
#' 
#' @param ... Arguments passed to readflex_enhanced
#' @return Data frame
readflex <- function(...) {
  # Check if user wants the original simple version
  args <- list(...)
  
  if (isTRUE(args$simple) || isTRUE(getOption("readflex.use_simple", FALSE))) {
    # Remove the 'simple' argument before passing to original function
    args$simple <- NULL
    
    # Call original readflex function
    if (file.exists("R/readflex.R")) {
      source("R/readflex.R", local = TRUE)
      return(do.call(get("readflex", envir = environment()), args))
    }
  }
  
  # Default to enhanced version
  readflex_enhanced(...)
}