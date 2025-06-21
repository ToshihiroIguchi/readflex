#' Analytics and monitoring features for readflex
#' 
#' @description
#' This module provides comprehensive usage analytics, benchmarking,
#' and monitoring capabilities for readflex.

#' Get comprehensive usage statistics
#' 
#' @param detailed Logical whether to include detailed breakdowns
#' @return List with usage statistics
readflex_stats <- function(detailed = TRUE) {
  if (length(.performance_stats) == 0) {
    return(structure(list(
      summary = list(
        total_operations = 0,
        total_files_processed = 0,
        average_processing_time = 0,
        total_data_processed_mb = 0
      ),
      message = "No statistics available. Enable stats with readflex_config(stats_enabled = TRUE)"
    ), class = "readflex_stats"))
  }
  
  stats_list <- as.list(.performance_stats)
  
  # Basic metrics
  total_operations <- length(stats_list)
  durations <- sapply(stats_list, function(x) x$duration)
  file_sizes <- sapply(stats_list, function(x) x$file_size)
  encodings <- sapply(stats_list, function(x) x$encoding)
  timestamps <- sapply(stats_list, function(x) x$timestamp)
  
  # Summary statistics
  summary_stats <- list(
    total_operations = total_operations,
    total_files_processed = total_operations,  # Assuming one file per operation
    average_processing_time = mean(durations, na.rm = TRUE),
    total_processing_time = sum(durations, na.rm = TRUE),
    total_data_processed_mb = sum(file_sizes, na.rm = TRUE) / (1024^2),
    fastest_operation = min(durations, na.rm = TRUE),
    slowest_operation = max(durations, na.rm = TRUE)
  )
  
  result <- list(summary = summary_stats)
  
  if (detailed) {
    # Encoding frequency analysis
    encoding_freq <- sort(table(encodings), decreasing = TRUE)
    result$encoding_frequency <- as.list(encoding_freq)
    
    # Performance by encoding
    encoding_performance <- aggregate(durations, by = list(encoding = encodings), 
                                    FUN = function(x) c(mean = mean(x), median = median(x), count = length(x)))
    result$performance_by_encoding <- encoding_performance
    
    # Time series analysis
    if (length(timestamps) > 1) {
      ts_data <- data.frame(
        timestamp = as.POSIXct(timestamps),
        duration = durations,
        file_size = file_sizes,
        stringsAsFactors = FALSE
      )
      ts_data <- ts_data[order(ts_data$timestamp), ]
      
      result$time_series <- ts_data
      
      # Performance trends
      if (nrow(ts_data) >= 5) {
        # Simple moving average
        window_size <- min(5, nrow(ts_data))
        ts_data$moving_avg <- stats::filter(ts_data$duration, rep(1/window_size, window_size), sides = 2)
        
        result$performance_trend <- list(
          improving = tail(ts_data$moving_avg, 1) < head(ts_data$moving_avg, 1),
          trend_data = ts_data
        )
      }
    }
    
    # File size analysis
    size_categories <- cut(file_sizes / (1024^2), 
                          breaks = c(0, 1, 10, 100, Inf),
                          labels = c("Small (<1MB)", "Medium (1-10MB)", "Large (10-100MB)", "Very Large (>100MB)"))
    
    result$file_size_distribution <- table(size_categories)
    
    # Recent activity (last 24 hours)
    recent_cutoff <- Sys.time() - 24 * 3600
    recent_stats <- stats_list[sapply(stats_list, function(x) {
      as.POSIXct(x$timestamp) > recent_cutoff
    })]
    
    result$recent_activity <- list(
      operations_24h = length(recent_stats),
      avg_duration_24h = if (length(recent_stats) > 0) {
        mean(sapply(recent_stats, function(x) x$duration))
      } else 0
    )
  }
  
  class(result) <- "readflex_stats"
  return(result)
}

#' Print method for readflex statistics
#' 
#' @param x readflex_stats object
#' @param ... Additional arguments
print.readflex_stats <- function(x, ...) {
  cat("READFLEX USAGE STATISTICS\n")
  cat(rep("=", 50), "\n")
  
  if (!is.null(x$message)) {
    cat(x$message, "\n")
    return(invisible(x))
  }
  
  # Summary
  s <- x$summary
  cat(sprintf("Total Operations: %d\n", s$total_operations))
  cat(sprintf("Files Processed: %d\n", s$total_files_processed))
  cat(sprintf("Average Time: %.3f seconds\n", s$average_processing_time))
  cat(sprintf("Total Data: %.2f MB\n", s$total_data_processed_mb))
  cat(sprintf("Fastest: %.3f sec | Slowest: %.3f sec\n", 
              s$fastest_operation, s$slowest_operation))
  
  # Recent activity
  if (!is.null(x$recent_activity)) {
    cat(sprintf("Last 24h: %d operations (avg: %.3f sec)\n", 
                x$recent_activity$operations_24h, 
                x$recent_activity$avg_duration_24h))
  }
  
  # Top encodings
  if (!is.null(x$encoding_frequency) && length(x$encoding_frequency) > 0) {
    cat("\nMost Used Encodings:\n")
    top_encodings <- head(x$encoding_frequency, 5)
    for (i in seq_along(top_encodings)) {
      cat(sprintf("  %d. %s: %d times\n", i, names(top_encodings)[i], top_encodings[[i]]))
    }
  }
  
  # File size distribution
  if (!is.null(x$file_size_distribution)) {
    cat("\nFile Size Distribution:\n")
    for (i in seq_along(x$file_size_distribution)) {
      cat(sprintf("  %s: %d files\n", 
                  names(x$file_size_distribution)[i], 
                  x$file_size_distribution[i]))
    }
  }
  
  # Performance trend
  if (!is.null(x$performance_trend)) {
    trend_text <- if (x$performance_trend$improving) "improving" else "declining"
    cat(sprintf("\nPerformance Trend: %s\n", trend_text))
  }
  
  cat(rep("=", 50), "\n")
}

#' Benchmark readflex performance
#' 
#' @param test_files Character vector of file paths to test
#' @param iterations Integer number of iterations per file
#' @param compare_with Character vector of alternative functions to compare
#' @param verbose Logical whether to show progress
#' @return Data frame with benchmark results
benchmark_readflex <- function(test_files, iterations = 3, compare_with = NULL, verbose = TRUE) {
  if (length(test_files) == 0) {
    stop("No test files provided")
  }
  
  # Check that files exist
  missing_files <- test_files[!file.exists(test_files)]
  if (length(missing_files) > 0) {
    warning(sprintf("Missing files: %s", paste(missing_files, collapse = ", ")))
    test_files <- test_files[file.exists(test_files)]
  }
  
  if (length(test_files) == 0) {
    stop("No valid test files found")
  }
  
  results <- data.frame(
    file = character(0),
    method = character(0),
    iteration = integer(0),
    duration_seconds = numeric(0),
    rows = integer(0),
    columns = integer(0),
    file_size_mb = numeric(0),
    encoding_detected = character(0),
    success = logical(0),
    error_message = character(0),
    stringsAsFactors = FALSE
  )
  
  total_tests <- length(test_files) * iterations * (1 + length(compare_with))
  
  if (verbose) {
    pb <- simple_progress_bar(total_tests)
    test_count <- 0
  }
  
  for (file in test_files) {
    file_size_mb <- file.size(file) / (1024^2)
    
    # Test readflex_enhanced
    for (iter in 1:iterations) {
      if (verbose) {
        test_count <- test_count + 1
        pb$tick()
      }
      
      start_time <- Sys.time()
      success <- TRUE
      error_msg <- ""
      rows <- 0
      columns <- 0
      encoding <- "unknown"
      
      tryCatch({
        result <- readflex_enhanced(file, verbose = FALSE)
        rows <- nrow(result)
        columns <- ncol(result)
        encoding <- attr(result, "encoding") %||% "unknown"
      }, error = function(e) {
        success <<- FALSE
        error_msg <<- e$message
      })
      
      end_time <- Sys.time()
      duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
      
      results <- rbind(results, data.frame(
        file = basename(file),
        method = "readflex_enhanced",
        iteration = iter,
        duration_seconds = duration,
        rows = rows,
        columns = columns,
        file_size_mb = file_size_mb,
        encoding_detected = encoding,
        success = success,
        error_message = error_msg,
        stringsAsFactors = FALSE
      ))
    }
    
    # Test comparison methods
    for (compare_method in compare_with) {
      for (iter in 1:iterations) {
        if (verbose) {
          test_count <- test_count + 1
          pb$tick()
        }
        
        start_time <- Sys.time()
        success <- TRUE
        error_msg <- ""
        rows <- 0
        columns <- 0
        
        tryCatch({
          if (compare_method == "read.csv") {
            result <- read.csv(file)
          } else if (compare_method == "readr::read_csv" && requireNamespace("readr", quietly = TRUE)) {
            result <- readr::read_csv(file, show_col_types = FALSE)
          } else if (compare_method == "data.table::fread" && requireNamespace("data.table", quietly = TRUE)) {
            result <- data.table::fread(file)
          } else {
            stop(sprintf("Unknown comparison method: %s", compare_method))
          }
          
          rows <- nrow(result)
          columns <- ncol(result)
          
        }, error = function(e) {
          success <<- FALSE
          error_msg <<- e$message
        })
        
        end_time <- Sys.time()
        duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
        
        results <- rbind(results, data.frame(
          file = basename(file),
          method = compare_method,
          iteration = iter,
          duration_seconds = duration,
          rows = rows,
          columns = columns,
          file_size_mb = file_size_mb,
          encoding_detected = "unknown",
          success = success,
          error_message = error_msg,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  if (verbose) {
    cat("\n")
  }
  
  # Calculate summary statistics
  summary_results <- aggregate(
    duration_seconds ~ file + method, 
    data = results[results$success, ], 
    FUN = function(x) c(mean = mean(x), median = median(x), sd = sd(x), min = min(x), max = max(x))
  )
  
  attr(results, "summary") <- summary_results
  class(results) <- c("readflex_benchmark", "data.frame")
  
  return(results)
}

#' Print method for benchmark results
#' 
#' @param x readflex_benchmark object
#' @param ... Additional arguments
print.readflex_benchmark <- function(x, ...) {
  cat("READFLEX BENCHMARK RESULTS\n")
  cat(rep("=", 50), "\n")
  
  # Overall statistics
  total_tests <- nrow(x)
  successful_tests <- sum(x$success)
  
  cat(sprintf("Total Tests: %d\n", total_tests))
  cat(sprintf("Successful: %d (%.1f%%)\n", successful_tests, (successful_tests/total_tests)*100))
  
  if (successful_tests < total_tests) {
    failed_tests <- total_tests - successful_tests
    cat(sprintf("Failed: %d (%.1f%%)\n", failed_tests, (failed_tests/total_tests)*100))
  }
  
  # Performance by method
  successful_data <- x[x$success, ]
  if (nrow(successful_data) > 0) {
    method_stats <- aggregate(duration_seconds ~ method, data = successful_data, 
                             FUN = function(x) c(mean = mean(x), median = median(x)))
    
    cat("\nAverage Performance by Method:\n")
    for (i in 1:nrow(method_stats)) {
      method <- method_stats$method[i]
      avg_time <- method_stats$duration_seconds[i, "mean"]
      cat(sprintf("  %s: %.3f seconds\n", method, avg_time))
    }
    
    # Relative performance
    if ("readflex_enhanced" %in% method_stats$method) {
      baseline <- method_stats[method_stats$method == "readflex_enhanced", "duration_seconds"][,"mean"]
      
      cat("\nRelative Performance (vs readflex_enhanced):\n")
      for (i in 1:nrow(method_stats)) {
        method <- method_stats$method[i]
        avg_time <- method_stats$duration_seconds[i, "mean"]
        relative <- avg_time / baseline
        
        if (method != "readflex_enhanced") {
          comparison <- if (relative < 1) {
            sprintf("%.1fx faster", 1/relative)
          } else {
            sprintf("%.1fx slower", relative)
          }
          cat(sprintf("  %s: %s\n", method, comparison))
        }
      }
    }
  }
  
  # File-specific performance
  if (length(unique(x$file)) > 1 && nrow(successful_data) > 0) {
    cat("\nPerformance by File:\n")
    file_stats <- aggregate(duration_seconds ~ file, data = successful_data, 
                           FUN = function(x) c(mean = mean(x), count = length(x)))
    
    for (i in 1:nrow(file_stats)) {
      file_name <- file_stats$file[i]
      avg_time <- file_stats$duration_seconds[i, "mean"]
      test_count <- file_stats$duration_seconds[i, "count"]
      cat(sprintf("  %s: %.3f sec (n=%d)\n", file_name, avg_time, test_count))
    }
  }
  
  cat(rep("=", 50), "\n")
}

#' Generate a comprehensive readflex report
#' 
#' @param include_benchmarks Logical whether to run benchmarks
#' @param benchmark_files Character vector of files for benchmarking
#' @param save_to_file Character path to save report (optional)
#' @return List with report data
generate_readflex_report <- function(include_benchmarks = FALSE, 
                                    benchmark_files = NULL, 
                                    save_to_file = NULL) {
  
  report_time <- Sys.time()
  
  # System information
  system_info <- list(
    r_version = R.version.string,
    platform = R.version$platform,
    os = Sys.info()["sysname"],
    readflex_version = "Enhanced v2.0",
    report_timestamp = format(report_time)
  )
  
  # Configuration
  config <- get_readflex_config()
  
  # Usage statistics
  stats <- readflex_stats(detailed = TRUE)
  
  # Cache status
  cache_info <- list(
    cache_entries = length(ls(.encoding_cache)),
    stats_entries = length(ls(.performance_stats)),
    cache_enabled = config$cache_enabled
  )
  
  # Benchmarks
  benchmark_results <- NULL
  if (include_benchmarks && !is.null(benchmark_files)) {
    benchmark_results <- tryCatch({
      benchmark_readflex(benchmark_files, verbose = FALSE)
    }, error = function(e) {
      list(error = e$message)
    })
  }
  
  # Compile report
  report <- list(
    metadata = list(
      title = "Readflex Performance and Usage Report",
      generated_at = report_time,
      system_info = system_info
    ),
    configuration = config,
    usage_statistics = stats,
    cache_status = cache_info,
    benchmarks = benchmark_results
  )
  
  # Save to file if requested
  if (!is.null(save_to_file)) {
    tryCatch({
      if (requireNamespace("jsonlite", quietly = TRUE)) {
        jsonlite::write_json(report, save_to_file, pretty = TRUE, auto_unbox = TRUE)
      } else {
        saveRDS(report, save_to_file)
      }
      cat(sprintf("Report saved to: %s\n", save_to_file))
    }, error = function(e) {
      warning(sprintf("Failed to save report: %s", e$message))
    })
  }
  
  class(report) <- "readflex_report"
  return(report)
}

#' Print method for readflex report
#' 
#' @param x readflex_report object
#' @param ... Additional arguments
print.readflex_report <- function(x, ...) {
  cat("READFLEX COMPREHENSIVE REPORT\n")
  cat(rep("=", 60), "\n")
  
  # Metadata
  cat(sprintf("Generated: %s\n", x$metadata$generated_at))
  cat(sprintf("System: %s on %s\n", 
              x$metadata$system_info$r_version,
              x$metadata$system_info$platform))
  
  # Configuration summary
  cat("\nCONFIGURATION:\n")
  cat(sprintf("  Default encodings: %d\n", length(x$configuration$default_encodings)))
  cat(sprintf("  Cache enabled: %s\n", x$configuration$cache_enabled))
  cat(sprintf("  Parallel enabled: %s\n", x$configuration$parallel_enabled))
  cat(sprintf("  Stats enabled: %s\n", x$configuration$stats_enabled))
  
  # Usage summary
  if (!is.null(x$usage_statistics$summary)) {
    cat("\nUSAGE STATISTICS:\n")
    s <- x$usage_statistics$summary
    cat(sprintf("  Operations: %d\n", s$total_operations))
    cat(sprintf("  Data processed: %.2f MB\n", s$total_data_processed_mb))
    cat(sprintf("  Average time: %.3f sec\n", s$average_processing_time))
  }
  
  # Cache status
  cat("\nCACHE STATUS:\n")
  cat(sprintf("  Cached encodings: %d\n", x$cache_status$cache_entries))
  cat(sprintf("  Performance stats: %d\n", x$cache_status$stats_entries))
  
  # Benchmarks
  if (!is.null(x$benchmarks) && !is.null(x$benchmarks$error)) {
    cat("\nBENCHMARKS: Error occurred\n")
    cat(sprintf("  %s\n", x$benchmarks$error))
  } else if (!is.null(x$benchmarks)) {
    cat("\nBENCHMARKS: Available\n")
    cat(sprintf("  Total tests: %d\n", nrow(x$benchmarks)))
    cat(sprintf("  Success rate: %.1f%%\n", 
                sum(x$benchmarks$success) / nrow(x$benchmarks) * 100))
  }
  
  cat(rep("=", 60), "\n")
}

# Utility function for null coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x