#' Integration and usability enhancements for readflex
#' 
#' @description
#' This module provides Shiny integration, data validation,
#' and enhanced usability features.

#' Shiny module for file upload and preview
#' 
#' @param id Character module ID
#' @param max_file_size Numeric maximum file size in MB
#' @return Shiny UI
readflex_shiny_ui <- function(id, max_file_size = 100) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::div(
      class = "readflex-upload-panel",
      
      # File input
      shiny::fileInput(
        ns("file"),
        "Choose CSV/Text File",
        accept = c(".csv", ".txt", ".tsv", ".psv", ".xlsx", ".xls", ".ods", ".gz", ".zip"),
        width = "100%"
      ),
      
      # Options panel
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] != null", ns("file")),
        shiny::wellPanel(
          shiny::h4("[ANALYSIS] File Analysis"),
          shiny::verbatimTextOutput(ns("file_info")),
          
          shiny::h4(" Options"),
          shiny::fluidRow(
            shiny::column(6,
              shiny::selectInput(
                ns("encoding_profile"),
                "Encoding Profile:",
                choices = c("Auto-detect" = "auto", "Japan" = "japan", "China" = "china", 
                           "Korea" = "korea", "Europe" = "europe", "Global" = "global"),
                selected = "auto"
              )
            ),
            shiny::column(6,
              shiny::checkboxInput(ns("verbose"), "Verbose Output", FALSE)
            )
          ),
          
          shiny::actionButton(ns("process"), "[PROCESS] Analyze File", class = "btn-primary"),
          
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] > 0", ns("process")),
            shiny::br(),
            shiny::downloadButton(ns("download"), " Download Data", class = "btn-success")
          )
        )
      )
    ),
    
    # Results panel
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] > 0", ns("process")),
      
      shiny::wellPanel(
        shiny::h4("[RESULTS] Processing Results"),
        shiny::verbatimTextOutput(ns("encoding_info")),
        
        shiny::h4("[PREVIEW] Data Preview"),
        DT::dataTableOutput(ns("data_preview"))
      )
    )
  )
}

#' Shiny module server for file processing
#' 
#' @param id Character module ID
#' @param max_file_size Numeric maximum file size in MB
#' @return Reactive data frame
readflex_shiny_server <- function(id, max_file_size = 100) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Reactive values
    values <- shiny::reactiveValues(
      data = NULL,
      encoding_result = NULL,
      processing_log = ""
    )
    
    # File info output
    output$file_info <- shiny::renderText({
      shiny::req(input$file)
      
      file_path <- input$file$datapath
      file_info <- file.info(file_path)
      
      sprintf(
        "File: %s\nSize: %.2f MB\nModified: %s",
        input$file$name,
        file_info$size / (1024^2),
        file_info$mtime
      )
    })
    
    # Process file when button is clicked
    shiny::observeEvent(input$process, {
      shiny::req(input$file)
      
      file_path <- input$file$datapath
      
      # Show progress
      progress <- shiny::Progress$new()
      progress$set(message = "Processing file...", value = 0)
      on.exit(progress$close())
      
      tryCatch({
        progress$set(value = 0.2, detail = "Analyzing file structure...")
        
        # Apply encoding profile if selected
        if (input$encoding_profile != "auto") {
          readflex_profile(input$encoding_profile, apply_immediately = TRUE)
        }
        
        progress$set(value = 0.4, detail = "Detecting encoding...")
        
        # Process file with readflex
        if (input$verbose) {
          # Capture verbose output
          log_output <- capture.output({
            result <- readflex_multi(file_path, verbose = TRUE)
          }, type = "message")
          values$processing_log <- paste(log_output, collapse = "\n")
        } else {
          result <- readflex_multi(file_path, verbose = FALSE)
          values$processing_log <- "File processed successfully (verbose output disabled)"
        }
        
        progress$set(value = 0.8, detail = "Finalizing...")
        
        values$data <- result
        values$encoding_result <- attr(result, "encoding")
        
        progress$set(value = 1, detail = "Complete!")
        
      }, error = function(e) {
        values$processing_log <- sprintf("[ERROR] %s", e$message)
        values$data <- NULL
        values$encoding_result <- NULL
      })
    })
    
    # Encoding info output
    output$encoding_info <- shiny::renderText({
      if (!is.null(values$data)) {
        sprintf(
          "[SUCCESS] Processing completed\nRows: %d, Columns: %d\nCharacter encoding: %s\n\nProcessing log:\n%s",
          nrow(values$data),
          ncol(values$data),
          if (!is.null(values$encoding_result)) values$encoding_result else "Unknown",
          values$processing_log
        )
      } else if (nchar(values$processing_log) > 0) {
        values$processing_log
      } else {
        "Click 'Process File' to analyze your data."
      }
    })
    
    # Data preview table
    output$data_preview <- DT::renderDataTable({
      shiny::req(values$data)
      
      DT::datatable(
        values$data,
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          lengthMenu = c(5, 10, 25, 50)
        ),
        class = "cell-border stripe hover"
      )
    })
    
    # Download handler
    output$download <- shiny::downloadHandler(
      filename = function() {
        base_name <- tools::file_path_sans_ext(input$file$name)
        paste0(base_name, "_processed.csv")
      },
      content = function(file) {
        if (!is.null(values$data)) {
          write.csv(values$data, file, row.names = FALSE, fileEncoding = "UTF-8")
        }
      }
    )
    
    # Return reactive data for external use
    return(shiny::reactive(values$data))
  })
}

#' Data validation with schema
#' 
#' @param data Data frame to validate
#' @param schema List with validation rules
#' @param strict Logical whether to use strict validation
#' @return List with validation results
validate_readflex_data <- function(data, schema = NULL, strict = FALSE) {
  
  if (is.null(schema)) {
    # Auto-generate basic schema
    schema <- auto_generate_schema(data)
  }
  
  validation_results <- list(
    valid = TRUE,
    errors = character(0),
    warnings = character(0),
    summary = list(),
    data_quality = list()
  )
  
  # Column validation
  if ("columns" %in% names(schema)) {
    for (col_name in names(schema$columns)) {
      if (!col_name %in% names(data)) {
        validation_results$errors <- c(
          validation_results$errors,
          sprintf("Missing required column: %s", col_name)
        )
        validation_results$valid <- FALSE
      } else {
        # Type validation
        col_spec <- schema$columns[[col_name]]
        actual_type <- class(data[[col_name]])[1]
        expected_type <- col_spec$type
        
        if (!is.null(expected_type) && !type_matches(actual_type, expected_type)) {
          if (strict) {
            validation_results$errors <- c(
              validation_results$errors,
              sprintf("Column %s: expected %s, got %s", col_name, expected_type, actual_type)
            )
            validation_results$valid <- FALSE
          } else {
            validation_results$warnings <- c(
              validation_results$warnings,
              sprintf("Column %s: expected %s, got %s", col_name, expected_type, actual_type)
            )
          }
        }
        
        # Range validation for numeric columns
        if (is.numeric(data[[col_name]]) && !is.null(col_spec$range)) {
          out_of_range <- data[[col_name]] < col_spec$range[1] | data[[col_name]] > col_spec$range[2]
          out_of_range <- sum(out_of_range, na.rm = TRUE)
          
          if (out_of_range > 0) {
            validation_results$warnings <- c(
              validation_results$warnings,
              sprintf("Column %s: %d values out of expected range [%g, %g]", 
                      col_name, out_of_range, col_spec$range[1], col_spec$range[2])
            )
          }
        }
      }
    }
  }
  
  # Data quality checks
  validation_results$data_quality <- assess_data_quality(data)
  
  # Overall summary
  validation_results$summary <- list(
    total_rows = nrow(data),
    total_columns = ncol(data),
    error_count = length(validation_results$errors),
    warning_count = length(validation_results$warnings),
    completeness_score = validation_results$data_quality$completeness_score,
    quality_score = calculate_quality_score(validation_results)
  )
  
  class(validation_results) <- "readflex_validation"
  return(validation_results)
}

#' Auto-generate schema from data
#' 
#' @param data Data frame
#' @return List with schema
auto_generate_schema <- function(data) {
  schema <- list(columns = list())
  
  for (col_name in names(data)) {
    col_data <- data[[col_name]]
    col_type <- class(col_data)[1]
    
    col_spec <- list(type = col_type)
    
    if (is.numeric(col_data)) {
      col_spec$range <- range(col_data, na.rm = TRUE)
    }
    
    if (is.character(col_data)) {
      col_spec$max_length <- max(nchar(col_data), na.rm = TRUE)
    }
    
    schema$columns[[col_name]] <- col_spec
  }
  
  return(schema)
}

#' Check if types match for validation
#' 
#' @param actual Character actual type
#' @param expected Character expected type
#' @return Logical whether types match
type_matches <- function(actual, expected) {
  # Define type compatibility
  type_map <- list(
    "character" = c("character", "string", "text"),
    "numeric" = c("numeric", "double", "number"),
    "integer" = c("integer", "int", "numeric"),
    "logical" = c("logical", "boolean", "bool"),
    "factor" = c("factor", "categorical"),
    "Date" = c("Date", "date")
  )
  
  for (canonical_type in names(type_map)) {
    if (actual %in% type_map[[canonical_type]] && expected %in% type_map[[canonical_type]]) {
      return(TRUE)
    }
  }
  
  return(actual == expected)
}

#' Assess data quality
#' 
#' @param data Data frame
#' @return List with quality metrics
assess_data_quality <- function(data) {
  quality <- list()
  
  # Completeness (non-NA ratio)
  total_cells <- nrow(data) * ncol(data)
  na_cells <- sum(is.na(data))
  quality$completeness_score <- 1 - (na_cells / total_cells)
  
  # Column-wise completeness
  quality$column_completeness <- sapply(data, function(x) 1 - sum(is.na(x)) / length(x))
  
  # Uniqueness for character columns
  char_cols <- sapply(data, is.character)
  if (any(char_cols)) {
    quality$uniqueness_ratios <- sapply(data[char_cols], function(x) {
      length(unique(x[!is.na(x)])) / length(x[!is.na(x)])
    })
  }
  
  # Detect potential duplicates
  quality$duplicate_rows <- sum(duplicated(data))
  quality$duplicate_ratio <- quality$duplicate_rows / nrow(data)
  
  return(quality)
}

#' Calculate overall quality score
#' 
#' @param validation_results List with validation results
#' @return Numeric quality score (0-1)
calculate_quality_score <- function(validation_results) {
  base_score <- 1.0
  
  # Penalty for errors
  if (validation_results$summary$error_count > 0) {
    base_score <- base_score - 0.3
  }
  
  # Penalty for warnings
  warning_penalty <- min(0.2, validation_results$summary$warning_count * 0.05)
  base_score <- base_score - warning_penalty
  
  # Bonus for completeness
  completeness_bonus <- validation_results$data_quality$completeness_score * 0.2
  base_score <- base_score + completeness_bonus - 0.2  # Normalize
  
  return(max(0, min(1, base_score)))
}

#' Print method for validation results
#' 
#' @param x readflex_validation object
#' @param ... Additional arguments
print.readflex_validation <- function(x, ...) {
  cat("READFLEX DATA VALIDATION REPORT\n")
  cat(rep("=", 50), "\n")
  
  # Overall status
  status_text <- if (x$valid) "[VALID]" else "[INVALID]"
  cat(sprintf("%s Overall Status: %s\n", status_text, if (x$valid) "VALID" else "INVALID"))
  
  # Summary
  cat(sprintf("Data summary: %d rows, %d columns\n", 
              x$summary$total_rows, x$summary$total_columns))
  cat(sprintf(" Quality Score: %.1f%%\n", x$summary$quality_score * 100))
  cat(sprintf("Data completeness: %.1f%%\n", x$summary$completeness_score * 100))
  
  # Errors
  if (length(x$errors) > 0) {
    cat(sprintf("\n[ERROR] Validation errors (%d):\n", length(x$errors)))
    for (i in seq_along(x$errors)) {
      cat(sprintf("  %d. %s\n", i, x$errors[i]))
    }
  }
  
  # Warnings
  if (length(x$warnings) > 0) {
    cat(sprintf("\n[WARNING] Validation warnings (%d):\n", length(x$warnings)))
    for (i in seq_along(x$warnings)) {
      cat(sprintf("  %d. %s\n", i, x$warnings[i]))
    }
  }
  
  # Data quality details
  if (x$data_quality$duplicate_rows > 0) {
    cat(sprintf("\n Duplicates: %d rows (%.1f%%)\n", 
                x$data_quality$duplicate_rows, 
                x$data_quality$duplicate_ratio * 100))
  }
  
  cat(rep("=", 50), "\n")
}

#' Create a complete Shiny app for readflex
#' 
#' @param title Character app title
#' @param theme Character Bootstrap theme
#' @return Shiny app object
create_readflex_app <- function(title = "Readflex File Processor", theme = "flatly") {
  
  ui <- shiny::fluidPage(
    theme = if (requireNamespace("shinythemes", quietly = TRUE)) {
      shinythemes::shinytheme(theme)
    } else {
      NULL
    },
    
    shiny::titlePanel(
      shiny::div(
        shiny::h1("Readflex File Processor"),
        shiny::p("Universal CSV and text file reader with automatic encoding detection")
      )
    ),
    
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 4,
        
        shiny::h3(" Configuration"),
        
        shiny::selectInput(
          "global_profile",
          "Global Encoding Profile:",
          choices = c("Auto-detect" = "auto", "Japan" = "japan", "China" = "china", 
                     "Korea" = "korea", "Europe" = "europe", "Global" = "global"),
          selected = "auto"
        ),
        
        shiny::actionButton("apply_profile", "Apply Profile", class = "btn-info"),
        
        shiny::hr(),
        
        shiny::h4("[STATUS] System Status"),
        shiny::verbatimTextOutput("system_status"),
        
        shiny::br(),
        shiny::actionButton("clear_cache", " Clear Cache", class = "btn-warning")
      ),
      
      shiny::mainPanel(
        width = 8,
        readflex_shiny_ui("file_processor")
      )
    ),
    
    shiny::hr(),
    
    shiny::fluidRow(
      shiny::column(12,
        shiny::h4(" About Readflex"),
        shiny::p("Readflex is an advanced file reading package that automatically detects character encodings and handles various file formats. It supports:"),
        shiny::tags$ul(
          shiny::tags$li(" Multiple text formats: CSV, TSV, PSV, fixed-width"),
          shiny::tags$li("Office formats: Excel (.xlsx, .xls), OpenDocument (.ods)"),
          shiny::tags$li(" Compressed files: .gz, .bz2, .xz, .zip"),
          shiny::tags$li(" International encodings: UTF-8, Shift_JIS, GB18030, Big5, EUC-KR, KOI8-R, and more"),
          shiny::tags$li("Automatic character encoding detection with fallback mechanisms"),
          shiny::tags$li(" Performance optimization with caching and parallel processing")
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    
    # Process files with the module
    processed_data <- readflex_shiny_server("file_processor")
    
    # Apply global profile
    shiny::observeEvent(input$apply_profile, {
      if (input$global_profile != "auto") {
        readflex_profile(input$global_profile, apply_immediately = TRUE)
        shiny::showNotification(
          sprintf("Applied %s encoding profile", input$global_profile),
          type = "success"
        )
      }
    })
    
    # System status
    output$system_status <- shiny::renderText({
      config <- get_readflex_config()
      cache_info <- capture.output(manage_readflex_cache("info"))
      
      sprintf(
        "Cache: %s\nStatistics: %s\nParallel processing: %s\nSupported encodings: %d",
        if (config$cache_enabled) "Enabled" else "Disabled",
        if (config$stats_enabled) "Enabled" else "Disabled",
        if (config$parallel_enabled) "Enabled" else "Disabled",
        length(config$default_encodings)
      )
    })
    
    # Clear cache
    shiny::observeEvent(input$clear_cache, {
      manage_readflex_cache("clear")
      shiny::showNotification("Cache cleared successfully", type = "info")
    })
  }
  
  shiny::shinyApp(ui = ui, server = server)
}