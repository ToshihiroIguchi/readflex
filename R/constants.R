#' Constants and configuration for readflex package
#' 
#' @description
#' Centralized constants, messages, and configuration settings
#' for consistent output formatting and internationalization.

# Output formatting configuration
OUTPUT_CONFIG <- list(
  max_line_width = 80,
  indent_size = 2,
  section_separator = "=",
  subsection_separator = "-", 
  item_prefix = "  ",
  number_format = "%d. ",
  max_sample_display = 3,    # Reduced for performance
  progress_width = 40,       # Optimized width
  verbose_threshold_mb = 10  # Only verbose for small files
)

# Status indicators for consistent messaging
STATUS_INDICATORS <- list(
  success = "[OK]",
  warning = "[WARNING]", 
  error = "[ERROR]",
  info = "[INFO]",
  processing = "[PROCESSING]",
  critical = "[CRITICAL]",
  debug = "[DEBUG]"
)

# Severity levels for message prioritization
SEVERITY_LEVELS <- c(
  "CRITICAL: ",
  "ERROR: ",
  "WARNING: ",
  "INFO: ",
  "DEBUG: "
)

# Standard messages for internationalization
MESSAGES <- list(
  # File operations
  file_not_found = "File not found: %s",
  file_too_large = "File too large: %.2f MB (limit: %.2f MB)",
  file_read_success = "File read successfully: %s",
  
  # Character encoding detection (clearer for international users)
  encoding_detected = "Character encoding successfully detected: %s",
  encoding_confidence = "Character encoding: %s (%.1f%% detection confidence)",
  no_encoding_found = "Unable to detect suitable character encoding",
  multiple_encodings = "Multiple possible character encodings detected",
  
  # Processing status
  processing_start = "Processing file: %s",
  processing_complete = "File processing completed successfully",
  processing_failed = "File processing failed",
  
  # Diagnostic headers
  diagnostic_header = "READFLEX DIAGNOSTIC REPORT",
  file_info_header = "FILE INFORMATION",
  content_analysis_header = "CONTENT ANALYSIS",
  encoding_analysis_header = "CHARACTER ENCODING ANALYSIS",
  recommendations_header = "RECOMMENDATIONS",
  
  # Content analysis (clear descriptions)
  lines_sampled = "Number of lines analyzed: %d",
  avg_line_length = "Average line length: %.1f characters per line",
  detected_delimiter = "Detected column delimiter: '%s'",
  has_header = "Contains header row: %s",
  language_hints = "Detected text language: %s",
  
  # Validation
  validation_passed = "Data validation passed",
  validation_failed = "Data validation failed",
  validation_warnings = "Data validation completed with warnings"
)

# Technical terminology standardization for international users
TERMINOLOGY <- list(
  encoding = "character encoding",
  separator = "column delimiter", 
  confidence = "detection confidence",
  diagnostic = "diagnostic report",
  bom = "byte order mark (BOM)",
  sample = "data sample",
  validation = "data validation",
  optimization = "performance optimization"
)

# Format helper functions
format_section_header <- function(title, level = 1) {
  separator <- if (level == 1) OUTPUT_CONFIG$section_separator else OUTPUT_CONFIG$subsection_separator
  width <- if (level == 1) 50 else 30
  
  paste0(title, "\n", paste(rep(separator, width), collapse = ""), "\n")
}

format_indented_text <- function(text, indent_level = 1) {
  indent <- paste(rep(" ", OUTPUT_CONFIG$indent_size * indent_level), collapse = "")
  paste0(indent, text)
}

format_numbered_list <- function(items) {
  if (length(items) == 0) return("  No items")
  
  formatted <- character(length(items))
  for (i in seq_along(items)) {
    formatted[i] <- sprintf("  %d. %s", i, items[i])
  }
  paste(formatted, collapse = "\n")
}

format_file_info <- function(file_info) {
  sprintf("File: %s (%.2f MB, modified: %s)",
          basename(file_info$path), 
          file_info$size_mb, 
          format(file_info$modified, "%Y-%m-%d %H:%M"))
}

format_encoding_results <- function(encodings) {
  if (nrow(encodings) == 0) {
    return("No character encodings detected")
  }
  
  best <- encodings[1, ]
  if (best$confidence > 0) {
    sprintf("Best character encoding: %s (%.1f%% confidence)",
            best$encoding, best$confidence * 100)
  } else {
    "No suitable character encoding found"
  }
}

# Validation message formatters
format_validation_error <- function(field, message) {
  sprintf("%s %s: %s", STATUS_INDICATORS$error, field, message)
}

format_validation_warning <- function(field, message) {
  sprintf("%s %s: %s", STATUS_INDICATORS$warning, field, message)
}

format_validation_success <- function(message) {
  sprintf("%s %s", STATUS_INDICATORS$success, message)
}