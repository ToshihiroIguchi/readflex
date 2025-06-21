#' Enhanced error handling and diagnostics for readflex
#' 
#' @description
#' Comprehensive error handling, diagnostics, and user feedback features
#' to help troubleshoot file reading issues.

#' Simple ASCII progress bar formatting function
#' 
#' @param current Integer current progress value
#' @param total Integer total progress value
#' @param width Integer width of progress bar (default 40)
#' @return Character string formatted progress bar
format_progress_bar <- function(current, total, width = 40) {
  percent <- current / total
  filled <- round(percent * width)
  bar <- paste0(
    "[", 
    paste(rep("=", filled), collapse = ""),
    paste(rep(" ", width - filled), collapse = ""),
    "] ",
    sprintf("%3.0f%% (%d/%d)", percent * 100, current, total)
  )
  return(bar)
}

if (!requireNamespace("progress", quietly = TRUE)) {
  # Simple ASCII progress bar fallback for CRAN compliance
  simple_progress_bar <- function(total) {
    current <- 0
    list(
      tick = function() {
        current <<- current + 1
        progress_text <- format_progress_bar(current, total, 40)
        cat(sprintf("\r%s", progress_text))
        if (current == total) cat("\n")
      },
      finish = function() cat("\n")
    )
  }
} else {
  simple_progress_bar <- function(total) {
    progress::progress_bar$new(
      format = "[:bar] :percent (:current/:total) :eta",
      total = total,
      clear = FALSE,
      width = 60
    )
  }
}

#' Comprehensive file diagnostics
#' 
#' @param file Character path to file
#' @param sample_lines Integer number of lines to sample
#' @return List with diagnostic information
#' @export
readflex_diagnostic <- function(file, sample_lines = 100) {
  if (!file.exists(file)) {
    stop(sprintf(MESSAGES$file_not_found, file))
  }
  
  # Basic file information
  file_info <- file.info(file)
  
  # Sample file content safely
  sample_content <- tryCatch({
    # Try reading as binary first
    con <- file(file, "rb")
    on.exit(close(con))
    
    # Read first few bytes to check for BOM
    bom_bytes <- readBin(con, "raw", 4)
    has_bom <- detect_bom(bom_bytes)
    
    # Read sample lines
    close(con)
    readLines(file, n = sample_lines, warn = FALSE, encoding = "UTF-8")
  }, error = function(e) {
    tryCatch({
      readLines(file, n = sample_lines, warn = FALSE, encoding = "latin1")
    }, error = function(e2) {
      "Unable to read file sample"
    })
  })
  
  # Detect potential encodings
  detected_encodings <- detect_all_possible_encodings(file, sample_content)
  
  # Analyze content characteristics
  content_analysis <- analyze_content_characteristics(sample_content)
  
  # Generate recommendations
  recommendations <- generate_recommendations(file_info, content_analysis, detected_encodings)
  
  result <- list(
    file_path = file,
    file_info = list(
      size_bytes = file_info$size,
      size_mb = round(file_info$size / (1024^2), 3),
      modified = file_info$mtime,
      readable = file_info$mode
    ),
    content_sample = if (is.character(sample_content) && length(sample_content) > 0) {
      head(sample_content, OUTPUT_CONFIG$max_sample_display)
    } else {
      "No readable content"
    },
    bom_detected = if (exists("has_bom")) has_bom else NULL,
    detected_encodings = detected_encodings,
    content_analysis = content_analysis,
    recommendations = recommendations,
    timestamp = Sys.time()
  )
  
  class(result) <- "readflex_diagnostic"
  return(result)
}

#' Detect BOM (Byte Order Mark)
#' 
#' @param bytes Raw vector of first few bytes
#' @return Character BOM type or NULL
detect_bom <- function(bytes) {
  if (length(bytes) >= 3) {
    if (identical(bytes[1:3], as.raw(c(0xEF, 0xBB, 0xBF)))) {
      return("UTF-8")
    }
  }
  if (length(bytes) >= 2) {
    if (identical(bytes[1:2], as.raw(c(0xFF, 0xFE)))) {
      return("UTF-16LE")
    }
    if (identical(bytes[1:2], as.raw(c(0xFE, 0xFF)))) {
      return("UTF-16BE")
    }
  }
  return(NULL)
}

#' Detect all possible encodings with confidence scores
#' 
#' @param file Character path to file
#' @param sample_content Character vector of sample lines
#' @return Data frame with encoding and confidence
detect_all_possible_encodings <- function(file, sample_content) {
  encodings_to_test <- c(
    "UTF-8", "UTF-8-BOM", "UTF-16LE", "UTF-16BE",
    "Shift_JIS", "EUC-JP", "ISO-2022-JP",
    "GB18030", "GBK", "GB2312", "Big5",
    "EUC-KR", "ISO-2022-KR",
    "Windows-1251", "KOI8-R", "ISO-8859-5",
    "ISO-8859-1", "Windows-1252", "latin1"
  )
  
  results <- data.frame(
    encoding = character(0),
    confidence = numeric(0),
    error_message = character(0),
    stringsAsFactors = FALSE
  )
  
  for (enc in encodings_to_test) {
    confidence <- 0
    error_msg <- ""
    
    tryCatch({
      test_result <- utils::read.csv(file, fileEncoding = enc, nrows = 5)
      if (is.data.frame(test_result) && nrow(test_result) > 0) {
        confidence <- calculate_encoding_confidence(test_result, sample_content, enc)
      }
    }, error = function(e) {
      error_msg <<- e$message
    })
    
    results <- rbind(results, data.frame(
      encoding = enc,
      confidence = confidence,
      error_message = error_msg,
      stringsAsFactors = FALSE
    ))
  }
  
  return(results[order(results$confidence, decreasing = TRUE), ])
}

#' Calculate confidence score for encoding
#' 
#' @param data Data frame read with encoding
#' @param sample_content Character sample content
#' @param encoding Character encoding used
#' @return Numeric confidence score (0-1)
calculate_encoding_confidence <- function(data, sample_content, encoding) {
  confidence <- 0.5  # Base confidence
  
  # Check for readable characters
  text_cols <- sapply(data, is.character)
  if (any(text_cols)) {
    text_data <- unlist(data[text_cols])
    
    # Penalty for garbled characters
    if (any(grepl("\\?{2,}", text_data))) {
      confidence <- confidence - 0.3
    }
    
    # Bonus for proper language characters
    # Check for Japanese characters (Hiragana, Katakana, Kanji)
    if (encoding %in% c("UTF-8", "Shift_JIS", "EUC-JP") && 
        any(grepl("[\u3040-\u309F\u30A0-\u30FF\u4E00-\u9FAF]", text_data))) {
      confidence <- confidence + 0.3
    }
    
    # Check for Chinese characters (CJK Unified Ideographs)
    if (encoding %in% c("UTF-8", "GB18030", "GBK", "Big5") && 
        any(grepl("[\u4E00-\u9FAF]", text_data))) {
      confidence <- confidence + 0.3
    }
    
    # Check for Korean characters (Hangul)
    if (encoding %in% c("UTF-8", "EUC-KR") && 
        any(grepl("[\uAC00-\uD7AF]", text_data))) {
      confidence <- confidence + 0.3
    }
  }
  
  return(max(0, min(1, confidence)))
}

#' Analyze content characteristics
#' 
#' @param sample_content Character vector of content
#' @return List with content analysis
analyze_content_characteristics <- function(sample_content) {
  if (!is.character(sample_content) || length(sample_content) == 0) {
    return(list(
      line_count = 0,
      avg_line_length = 0,
      detected_separator = ",",
      has_header = FALSE,
      language_hints = "unknown"
    ))
  }
  
  # Basic statistics
  line_count <- length(sample_content)
  avg_line_length <- mean(nchar(sample_content), na.rm = TRUE)
  
  # Detect separator
  separators <- c(",", ";", "\t", "|")
  sep_counts <- sapply(separators, function(sep) {
    if (requireNamespace("stringr", quietly = TRUE)) {
      mean(stringr::str_count(sample_content, stringr::fixed(sep)))
    } else {
      mean(lengths(regmatches(sample_content, gregexpr(sep, sample_content, fixed = TRUE))))
    }
  })
  detected_separator <- names(sep_counts)[which.max(sep_counts)]
  
  # Check for header
  if (line_count > 1) {
    first_line <- sample_content[1]
    second_line <- sample_content[2]
    
    # Simple heuristic: if first line has fewer numbers, likely header
    first_nums <- length(regmatches(first_line, gregexpr("\\d+", first_line))[[1]])
    second_nums <- length(regmatches(second_line, gregexpr("\\d+", second_line))[[1]])
    has_header <- first_nums < second_nums
  } else {
    has_header <- FALSE
  }
  
  # Language detection
  content_text <- paste(sample_content, collapse = " ")
  language_hints <- detect_language_hints(content_text)
  
  return(list(
    line_count = line_count,
    avg_line_length = round(avg_line_length, 1),
    detected_separator = detected_separator,
    has_header = has_header,
    language_hints = language_hints
  ))
}

#' Detect language hints from content
#' 
#' @param text Character text to analyze
#' @return Character language hint
detect_language_hints <- function(text) {
  # Check for CJK characters
  if (grepl("[\u4E00-\u9FAF]", text)) {
    # Check for Japanese-specific characters (Hiragana/Katakana)
    if (grepl("[\u3040-\u309F\u30A0-\u30FF]", text)) {
      return("japanese")
    } else {
      return("chinese")
    }
  } else if (grepl("[\uAC00-\uD7AF]", text)) {
    return("korean")
  } else if (grepl("[\u0400-\u04FF]", text)) {
    return("russian/cyrillic")
  } else if (grepl("[\u00C0-\u00FF]", text)) {
    return("european")
  } else {
    return("latin/ascii")
  }
}

#' Generate recommendations based on analysis
#' 
#' @param file_info List with file information
#' @param content_analysis List with content analysis
#' @param encoding_results Data frame with encoding test results
#' @return Character vector of recommendations
generate_recommendations <- function(file_info, content_analysis, encoding_results) {
  recommendations <- character(0)
  
  # File size recommendations
  if (file_info$size_mb > 100) {
    recommendations <- c(recommendations,
      "Large file detected. Consider using readflex with max_file_size_mb parameter or chunk processing.")
  }
  
  # Encoding recommendations
  best_encodings <- encoding_results[encoding_results$confidence > 0.5, ]
  if (nrow(best_encodings) > 0) {
    recommendations <- c(recommendations,
      sprintf("Recommended encodings: %s", paste(head(best_encodings$encoding, 3), collapse = ", ")))
  } else {
    recommendations <- c(recommendations,
      "No encoding detected with high confidence. Try readflex_auto_fix() function.")
  }
  
  # Separator recommendations
  if (content_analysis$detected_separator != ",") {
    recommendations <- c(recommendations,
      sprintf("Non-comma separator detected (%s). Use sep='%s' parameter.", 
              content_analysis$detected_separator, content_analysis$detected_separator))
  }
  
  # Language-specific recommendations
  lang_hint <- content_analysis$language_hints
  if (lang_hint == "japanese") {
    recommendations <- c(recommendations,
      "Japanese text detected. Recommended character encodings: UTF-8 or Shift_JIS.")
  } else if (lang_hint == "chinese") {
    recommendations <- c(recommendations,
      "Chinese text detected. Recommended character encodings: UTF-8, GB18030, or Big5.")
  } else if (lang_hint == "korean") {
    recommendations <- c(recommendations,
      "Korean text detected. Recommended character encodings: UTF-8 or EUC-KR.")
  }
  
  return(recommendations)
}

#' Print method for diagnostic results
#' 
#' @param x readflex_diagnostic object
#' @param verbose Logical whether to show detailed output
#' @param ... Additional arguments
print.readflex_diagnostic <- function(x, verbose = TRUE, ...) {
  # Use standardized formatting from constants
  cat(format_section_header(MESSAGES$diagnostic_header))
  
  # Basic file information (always shown)
  cat(sprintf("File: %s\n", basename(x$file_path)))
  cat(sprintf("Size: %.2f MB (%d bytes)\n", x$file_info$size_mb, x$file_info$size_bytes))
  
  if (verbose) {
    cat(sprintf("Modified: %s\n", x$file_info$modified))
    cat(sprintf("Full path: %s\n", x$file_path))
  }
  
  # BOM detection
  if (!is.null(x$bom_detected)) {
    cat(format_validation_success(sprintf("Byte Order Mark detected: %s", x$bom_detected)), "\n")
  }
  
  # Best encoding result (always show)
  if (nrow(x$detected_encodings) > 0 && x$detected_encodings$confidence[1] > 0) {
    best_enc <- x$detected_encodings$encoding[1]
    confidence <- x$detected_encodings$confidence[1] * 100
    cat(format_validation_success(sprintf("Best character encoding: %s (%.1f%% confidence)", best_enc, confidence)), "\n")
  } else {
    cat(format_validation_warning("Character encoding", "No suitable encoding detected"), "\n")
  }
  
  if (verbose) {
    # Content sample section
    cat("\n", format_section_header("CONTENT SAMPLE", 2))
    if (is.character(x$content_sample)) {
      for (i in seq_along(x$content_sample)) {
        cat(format_indented_text(sprintf("%d: %s", i, substr(x$content_sample[i], 1, 80))), "\n")
      }
    }
    
    # Encoding analysis section
    cat("\n", format_section_header(MESSAGES$encoding_analysis_header, 2))
    top_encodings <- head(x$detected_encodings[x$detected_encodings$confidence > 0, ], 5)
    if (nrow(top_encodings) > 0) {
      for (i in 1:nrow(top_encodings)) {
        cat(format_indented_text(sprintf("%s: %.1f%% confidence", 
                                       top_encodings$encoding[i], 
                                       top_encodings$confidence[i] * 100)), "\n")
      }
    } else {
      cat(format_indented_text("No character encodings with positive confidence detected"), "\n")
    }
    
    # Content analysis section
    cat("\n", format_section_header(MESSAGES$content_analysis_header, 2))
    cat(format_indented_text(sprintf(MESSAGES$lines_sampled, x$content_analysis$line_count)), "\n")
    cat(format_indented_text(sprintf(MESSAGES$avg_line_length, x$content_analysis$avg_line_length)), "\n")
    cat(format_indented_text(sprintf(MESSAGES$detected_delimiter, x$content_analysis$detected_separator)), "\n")
    cat(format_indented_text(sprintf(MESSAGES$has_header, x$content_analysis$has_header)), "\n")
    cat(format_indented_text(sprintf(MESSAGES$language_hints, x$content_analysis$language_hints)), "\n")
  }
  
  # Recommendations section
  cat("\n", format_section_header(MESSAGES$recommendations_header, 2))
  cat(format_numbered_list(x$recommendations), "\n")
  
  # Final separator
  cat(paste(rep(OUTPUT_CONFIG$section_separator, 50), collapse = ""), "\n")
}

#' Auto-fix common file issues
#' 
#' @param file Character path to file
#' @param ... Additional arguments passed to readflex
#' @return Data frame or error with suggestions
#' @export
readflex_auto_fix <- function(file, ...) {
  # First, run diagnostics
  diag <- readflex_diagnostic(file)
  
  # Try the most promising encoding first
  best_encoding <- diag$detected_encodings$encoding[1]
  
  if (diag$detected_encodings$confidence[1] > 0.5) {
    tryCatch({
      if (!is.null(diag$bom_detected)) {
        # Handle BOM
        return(readflex_remove_bom(file, encoding = best_encoding, ...))
      } else {
        # Try with detected separator
        return(readflex(file, 
                       encodings = c(best_encoding), 
                       sep = diag$content_analysis$detected_separator,
                       ...))
      }
    }, error = function(e) {
      # Fall back to aggressive cleaning
      return(readflex_aggressive_clean(file, ...))
    })
  } else {
    stop(sprintf(
      "Auto-fix failed. Manual intervention required. Run readflex_diagnostic('%s') for details.",
      file
    ))
  }
}

#' Remove BOM from file
#' 
#' @param file Character path to file
#' @param encoding Character encoding to use
#' @param ... Additional arguments
#' @return Data frame
readflex_remove_bom <- function(file, encoding = "UTF-8", ...) {
  # Create temporary file without BOM
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file))
  
  # Read as binary and remove BOM
  con_in <- file(file, "rb")
  on.exit(close(con_in), add = TRUE)
  
  # Skip BOM bytes
  bom_bytes <- readBin(con_in, "raw", 3)
  if (identical(bom_bytes, as.raw(c(0xEF, 0xBB, 0xBF)))) {
    # UTF-8 BOM detected, already skipped
  } else {
    # No BOM or different BOM, reset position
    close(con_in)
    con_in <- file(file, "rb")
  }
  
  # Copy rest of file
  con_out <- file(temp_file, "wb")
  on.exit(close(con_out), add = TRUE)
  
  while (length(chunk <- readBin(con_in, "raw", 8192)) > 0) {
    writeBin(chunk, con_out)
  }
  
  close(con_out)
  close(con_in)
  
  # Now read the cleaned file
  return(readflex(temp_file, encodings = c(encoding), ...))
}

#' Aggressive cleaning for problematic files
#' 
#' @param file Character path to file
#' @param ... Additional arguments
#' @return Data frame
readflex_aggressive_clean <- function(file, ...) {
  warning("Using aggressive cleaning mode. Results may be imperfect.")
  
  # Try forcing UTF-8 with error handling
  tryCatch({
    utils::read.csv(file, fileEncoding = "UTF-8", encoding = "UTF-8", ...)
  }, error = function(e) {
    # Last resort: read as latin1 and convert
    tryCatch({
      result <- utils::read.csv(file, fileEncoding = "latin1", ...)
      # Try to convert character columns to UTF-8
      char_cols <- sapply(result, is.character)
      result[char_cols] <- lapply(result[char_cols], function(x) {
        iconv(x, from = "latin1", to = "UTF-8", sub = "?")
      })
      return(result)
    }, error = function(e2) {
      stop(sprintf("All auto-fix attempts failed. Original errors:\n1. %s\n2. %s", 
                   e$message, e2$message))
    })
  })
}