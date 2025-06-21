#' Multi-format file support for readflex
#' 
#' @description
#' This module extends readflex to support various file formats
#' including TSV, PSV, compressed files, and Office formats.

#' Multi-format flexible file reader
#' 
#' @param file Character path to file
#' @param format Character format override ("auto", "csv", "tsv", "psv", "excel", "ods")
#' @param ... Additional arguments passed to appropriate reader
#' @return Data frame
readflex_multi <- function(file, format = "auto", ...) {
  if (!file.exists(file)) {
    stop(sprintf("[readflex] File not found: %s", file))
  }
  
  # Determine format
  if (format == "auto") {
    format <- detect_file_format(file)
  }
  
  # Route to appropriate reader
  switch(format,
    "csv" = readflex(file, ...),
    "tsv" = readflex_tsv(file, ...),
    "psv" = readflex_psv(file, ...),
    "excel" = readflex_excel(file, ...),
    "ods" = readflex_ods(file, ...),
    "compressed" = readflex_compressed(file, ...),
    "fixed_width" = readflex_fixed_width(file, ...),
    stop(sprintf("Unsupported format: %s", format))
  )
}

#' Detect file format from extension and content
#' 
#' @param file Character path to file
#' @return Character format identifier
detect_file_format <- function(file) {
  ext <- tolower(tools::file_ext(file))
  
  # Check for compressed files first
  if (ext %in% c("gz", "bz2", "xz", "zip")) {
    return("compressed")
  }
  
  # Office formats
  if (ext %in% c("xlsx", "xls")) {
    return("excel")
  }
  
  if (ext == "ods") {
    return("ods")
  }
  
  # Text formats
  if (ext == "tsv") {
    return("tsv")
  }
  
  if (ext == "psv") {
    return("psv")
  }
  
  if (ext %in% c("csv", "txt")) {
    # Analyze content to determine separator
    sample_lines <- tryCatch({
      readLines(file, n = 5, warn = FALSE)
    }, error = function(e) character(0))
    
    if (length(sample_lines) > 0) {
      # Count separators
      comma_count <- mean(stringr::str_count(sample_lines, ","))
      tab_count <- mean(stringr::str_count(sample_lines, "\t"))
      pipe_count <- mean(stringr::str_count(sample_lines, "\\|"))
      
      if (tab_count > comma_count && tab_count > pipe_count) {
        return("tsv")
      } else if (pipe_count > comma_count && pipe_count > tab_count) {
        return("psv")
      }
    }
    
    return("csv")
  }
  
  # Fixed width detection
  if (ext %in% c("dat", "txt") && is_fixed_width_file(file)) {
    return("fixed_width")
  }
  
  # Default to CSV
  return("csv")
}

#' TSV (Tab-Separated Values) reader
#' 
#' @param file Character path to TSV file
#' @param ... Additional arguments passed to readflex
#' @return Data frame
readflex_tsv <- function(file, ...) {
  readflex(file, sep = "\t", ...)
}

#' PSV (Pipe-Separated Values) reader
#' 
#' @param file Character path to PSV file
#' @param ... Additional arguments passed to readflex
#' @return Data frame
readflex_psv <- function(file, ...) {
  readflex(file, sep = "|", ...)
}

#' Excel file reader with encoding detection
#' 
#' @param file Character path to Excel file
#' @param sheet Character or numeric sheet identifier
#' @param ... Additional arguments
#' @return Data frame
readflex_excel <- function(file, sheet = 1, ...) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("readxl package required for Excel files. Install with: install.packages('readxl')")
  }
  
  tryCatch({
    # Try reading with readxl
    result <- readxl::read_excel(file, sheet = sheet, ...)
    
    # Convert to regular data frame
    result <- as.data.frame(result)
    
    # Fix character encoding if needed
    char_cols <- sapply(result, is.character)
    if (any(char_cols)) {
      result[char_cols] <- lapply(result[char_cols], function(x) {
        # Ensure UTF-8 encoding
        Encoding(x) <- "UTF-8"
        x
      })
    }
    
    return(result)
    
  }, error = function(e) {
    stop(sprintf("Failed to read Excel file: %s", e$message))
  })
}

#' ODS (OpenDocument Spreadsheet) reader
#' 
#' @param file Character path to ODS file
#' @param sheet Character or numeric sheet identifier
#' @param ... Additional arguments
#' @return Data frame
readflex_ods <- function(file, sheet = 1, ...) {
  if (!requireNamespace("readODS", quietly = TRUE)) {
    stop("readODS package required for ODS files. Install with: install.packages('readODS')")
  }
  
  tryCatch({
    result <- readODS::read_ods(file, sheet = sheet, ...)
    
    # Convert to regular data frame and fix encoding
    result <- as.data.frame(result)
    char_cols <- sapply(result, is.character)
    if (any(char_cols)) {
      result[char_cols] <- lapply(result[char_cols], function(x) {
        Encoding(x) <- "UTF-8"
        x
      })
    }
    
    return(result)
    
  }, error = function(e) {
    stop(sprintf("Failed to read ODS file: %s", e$message))
  })
}

#' Compressed file reader
#' 
#' @param file Character path to compressed file
#' @param ... Additional arguments
#' @return Data frame
readflex_compressed <- function(file, ...) {
  ext <- tolower(tools::file_ext(file))
  
  # Create temporary file
  temp_file <- tempfile()
  on.exit(unlink(temp_file))
  
  # Decompress based on format
  if (ext == "gz") {
    if (!requireNamespace("R.utils", quietly = TRUE)) {
      stop("R.utils package required for .gz files. Install with: install.packages('R.utils')")
    }
    R.utils::gunzip(file, temp_file, remove = FALSE)
    
  } else if (ext == "bz2") {
    if (!requireNamespace("R.utils", quietly = TRUE)) {
      stop("R.utils package required for .bz2 files. Install with: install.packages('R.utils')")
    }
    R.utils::bunzip2(file, temp_file, remove = FALSE)
    
  } else if (ext == "xz") {
    # Use system xz command
    system2("xz", args = c("-d", "-c", file), stdout = temp_file)
    
  } else if (ext == "zip") {
    # Extract ZIP file
    temp_dir <- tempdir()
    extracted_files <- unzip(file, exdir = temp_dir)
    
    # Find CSV-like file in extraction
    csv_files <- extracted_files[grepl("\\.(csv|txt|tsv)$", extracted_files, ignore.case = TRUE)]
    
    if (length(csv_files) == 0) {
      stop("No CSV/text files found in ZIP archive")
    }
    
    if (length(csv_files) > 1) {
      warning(sprintf("Multiple files found in ZIP. Using: %s", basename(csv_files[1])))
    }
    
    temp_file <- csv_files[1]
    
  } else {
    stop(sprintf("Unsupported compression format: %s", ext))
  }
  
  # Read the decompressed file
  return(readflex_multi(temp_file, format = "auto", ...))
}

#' Fixed-width file reader
#' 
#' @param file Character path to fixed-width file
#' @param widths Numeric vector of column widths
#' @param col_names Character vector of column names
#' @param ... Additional arguments
#' @return Data frame
readflex_fixed_width <- function(file, widths = NULL, col_names = NULL, ...) {
  if (is.null(widths)) {
    widths <- detect_column_widths(file)
  }
  
  if (!requireNamespace("readr", quietly = TRUE)) {
    # Fallback to base R
    return(read_fixed_width_base(file, widths, col_names, ...))
  }
  
  # Use readr for better performance
  tryCatch({
    result <- readr::read_fwf(
      file,
      col_positions = readr::fwf_widths(widths, col_names),
      locale = readr::locale(encoding = "UTF-8"),
      ...
    )
    return(as.data.frame(result))
    
  }, error = function(e) {
    # Fallback to base R method
    return(read_fixed_width_base(file, widths, col_names, ...))
  })
}

#' Detect if file is fixed-width format
#' 
#' @param file Character path to file
#' @return Logical whether file appears to be fixed-width
is_fixed_width_file <- function(file) {
  sample_lines <- tryCatch({
    readLines(file, n = 10, warn = FALSE)
  }, error = function(e) character(0))
  
  if (length(sample_lines) < 3) return(FALSE)
  
  # Check if lines have consistent length
  line_lengths <- nchar(sample_lines)
  length_variance <- var(line_lengths)
  
  # Check if separators are rare
  comma_count <- mean(stringr::str_count(sample_lines, ","))
  tab_count <- mean(stringr::str_count(sample_lines, "\t"))
  
  # Fixed-width heuristic
  return(length_variance < 5 && comma_count < 1 && tab_count < 1)
}

#' Detect column widths in fixed-width file
#' 
#' @param file Character path to file
#' @param sample_lines Integer number of lines to analyze
#' @return Numeric vector of column widths
detect_column_widths <- function(file, sample_lines = 20) {
  lines <- tryCatch({
    readLines(file, n = sample_lines, warn = FALSE)
  }, error = function(e) {
    stop("Cannot read file for width detection")
  })
  
  if (length(lines) == 0) {
    stop("File is empty")
  }
  
  # Simple algorithm: find consistent space patterns
  max_length <- max(nchar(lines))
  
  # Look for columns of spaces
  space_positions <- integer(0)
  
  for (pos in 1:max_length) {
    chars_at_pos <- substr(lines, pos, pos)
    space_ratio <- sum(chars_at_pos == " " | chars_at_pos == "", na.rm = TRUE) / length(lines)
    
    if (space_ratio > 0.7) {  # 70% of lines have space at this position
      space_positions <- c(space_positions, pos)
    }
  }
  
  if (length(space_positions) == 0) {
    # No clear pattern, assume single column
    return(max_length)
  }
  
  # Calculate widths from space positions
  breaks <- c(1, space_positions, max_length + 1)
  widths <- diff(breaks)
  
  # Remove very small columns (likely just separators)
  widths <- widths[widths > 1]
  
  return(widths)
}

#' Base R implementation for fixed-width reading
#' 
#' @param file Character path to file
#' @param widths Numeric vector of column widths
#' @param col_names Character vector of column names
#' @param ... Additional arguments
#' @return Data frame
read_fixed_width_base <- function(file, widths, col_names = NULL, ...) {
  lines <- readLines(file, warn = FALSE)
  
  if (length(lines) == 0) {
    return(data.frame())
  }
  
  # Parse each line
  result_list <- vector("list", length(widths))
  names(result_list) <- if (!is.null(col_names)) {
    col_names
  } else {
    paste0("V", seq_along(widths))
  }
  
  for (i in seq_along(widths)) {
    start_pos <- if (i == 1) 1 else sum(widths[1:(i-1)]) + 1
    end_pos <- start_pos + widths[i] - 1
    
    column_data <- substr(lines, start_pos, end_pos)
    column_data <- trimws(column_data)  # Remove leading/trailing spaces
    
    result_list[[i]] <- column_data
  }
  
  # Convert to data frame
  result <- as.data.frame(result_list, stringsAsFactors = FALSE)
  
  # Try to convert numeric columns
  for (i in seq_along(result)) {
    if (all(grepl("^[0-9.-]+$", result[[i]], na.rm = TRUE))) {
      numeric_version <- suppressWarnings(as.numeric(result[[i]]))
      if (!all(is.na(numeric_version))) {
        result[[i]] <- numeric_version
      }
    }
  }
  
  return(result)
}

#' Batch processing for multiple files
#' 
#' @param file_pattern Character glob pattern for files
#' @param output_format Character output format ("list", "combined", "separate")
#' @param progress Logical whether to show progress bar
#' @param ... Additional arguments passed to readflex_multi
#' @return List of data frames or combined data frame
readflex_batch <- function(file_pattern, output_format = "list", progress = TRUE, ...) {
  files <- Sys.glob(file_pattern)
  
  if (length(files) == 0) {
    stop(sprintf("No files found matching pattern: %s", file_pattern))
  }
  
  if (progress && length(files) > 1) {
    pb <- simple_progress_bar(length(files))
  }
  
  results <- vector("list", length(files))
  names(results) <- basename(files)
  
  for (i in seq_along(files)) {
    if (progress && length(files) > 1) {
      pb$tick()
    }
    
    tryCatch({
      results[[i]] <- readflex_multi(files[i], ...)
      attr(results[[i]], "source_file") <- files[i]
    }, error = function(e) {
      warning(sprintf("Failed to read %s: %s", files[i], e$message))
      results[[i]] <- NULL
    })
  }
  
  # Remove failed reads
  results <- Filter(Negate(is.null), results)
  
  if (output_format == "combined") {
    if (length(results) == 0) {
      return(data.frame())
    }
    
    # Add source file column
    for (i in seq_along(results)) {
      results[[i]]$source_file <- attr(results[[i]], "source_file")
    }
    
    # Combine all data frames
    tryCatch({
      combined <- do.call(rbind, results)
      return(combined)
    }, error = function(e) {
      warning("Could not combine all files (different structures). Returning list.")
      return(results)
    })
  }
  
  return(results)
}