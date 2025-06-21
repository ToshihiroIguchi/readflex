#' Configuration and caching system for readflex
#' 
#' @description
#' Global configuration management, regional profiles, 
#' and caching features for optimized file reading.

# Global configuration environment
.readflex_config <- new.env(parent = emptyenv())

# Initialize default configuration
.readflex_config$default_encodings <- c(
  "UTF-8", "UTF-8-BOM", "UTF-16LE", "UTF-16BE",
  "Shift_JIS", "CP932", "EUC-JP", "ISO-2022-JP",
  "ISO-8859-1", "Windows-1252", "latin1",
  "GB18030", "GB2312", "GBK", "Big5", "Big5-HKSCS",
  "EUC-KR", "ISO-2022-KR",
  "Windows-1251", "KOI8-R", "ISO-8859-5"
)

.readflex_config$cache_enabled <- TRUE
.readflex_config$cache_ttl_hours <- 24
.readflex_config$parallel_enabled <- TRUE
.readflex_config$stats_enabled <- FALSE
.readflex_config$verbose_level <- 1
.readflex_config$max_file_size_mb <- 100
.readflex_config$guess_n_max <- 1000

#' Configure readflex global settings
#' 
#' @param default_encodings Character vector of default encodings to try
#' @param cache_enabled Logical whether to enable result caching
#' @param cache_ttl_hours Numeric cache time-to-live in hours
#' @param parallel_enabled Logical whether to enable parallel processing
#' @param stats_enabled Logical whether to collect usage statistics
#' @param verbose_level Integer verbosity level (0=silent, 1=normal, 2=debug)
#' @param max_file_size_mb Numeric default maximum file size in MB
#' @param guess_n_max Integer default number of lines for encoding detection
#' @param save_config Logical whether to save configuration to file
#' @return Invisible list of current configuration
readflex_config <- function(
  default_encodings = NULL,
  cache_enabled = NULL,
  cache_ttl_hours = NULL,
  parallel_enabled = NULL,
  stats_enabled = NULL,
  verbose_level = NULL,
  max_file_size_mb = NULL,
  guess_n_max = NULL,
  save_config = FALSE
) {
  
  # Update configuration values
  if (!is.null(default_encodings)) {
    .readflex_config$default_encodings <- default_encodings
  }
  if (!is.null(cache_enabled)) {
    .readflex_config$cache_enabled <- cache_enabled
  }
  if (!is.null(cache_ttl_hours)) {
    .readflex_config$cache_ttl_hours <- cache_ttl_hours
  }
  if (!is.null(parallel_enabled)) {
    .readflex_config$parallel_enabled <- parallel_enabled
  }
  if (!is.null(stats_enabled)) {
    .readflex_config$stats_enabled <- stats_enabled
  }
  if (!is.null(verbose_level)) {
    .readflex_config$verbose_level <- verbose_level
  }
  if (!is.null(max_file_size_mb)) {
    .readflex_config$max_file_size_mb <- max_file_size_mb
  }
  if (!is.null(guess_n_max)) {
    .readflex_config$guess_n_max <- guess_n_max
  }
  
  # Save configuration if requested
  if (save_config) {
    save_readflex_config()
  }
  
  # Return current configuration
  invisible(as.list(.readflex_config))
}

#' Get current readflex configuration
#' 
#' @param key Character specific configuration key to retrieve
#' @return List of configuration or specific value
get_readflex_config <- function(key = NULL) {
  if (is.null(key)) {
    return(as.list(.readflex_config))
  } else {
    return(.readflex_config[[key]])
  }
}

#' Reset readflex configuration to defaults
#' 
#' @return Invisible NULL
reset_readflex_config <- function() {
  rm(list = ls(.readflex_config), envir = .readflex_config)
  
  # Reinitialize defaults
  .readflex_config$default_encodings <- c(
    "UTF-8", "UTF-8-BOM", "UTF-16LE", "UTF-16BE",
    "Shift_JIS", "CP932", "EUC-JP", "ISO-2022-JP",
    "ISO-8859-1", "Windows-1252", "latin1",
    "GB18030", "GB2312", "GBK", "Big5", "Big5-HKSCS",
    "EUC-KR", "ISO-2022-KR",
    "Windows-1251", "KOI8-R", "ISO-8859-5"
  )
  .readflex_config$cache_enabled <- TRUE
  .readflex_config$cache_ttl_hours <- 24
  .readflex_config$parallel_enabled <- TRUE
  .readflex_config$stats_enabled <- FALSE
  .readflex_config$verbose_level <- 1
  .readflex_config$max_file_size_mb <- 100
  .readflex_config$guess_n_max <- 1000
  
  invisible(NULL)
}

#' Load regional encoding profile
#' 
#' @param region Character region identifier or "auto" for system detection
#' @param apply_immediately Logical whether to apply profile to global config
#' @return List with regional configuration
readflex_profile <- function(region = "auto", apply_immediately = FALSE) {
  
  if (region == "auto") {
    region <- detect_system_locale()
  }
  
  profiles <- list(
    "japan" = list(
      name = "Japan",
      encodings = c("UTF-8", "Shift_JIS", "EUC-JP", "ISO-2022-JP", "CP932"),
      default_encoding = "UTF-8",
      common_separators = c(",", "\t"),
      description = "Optimized for Japanese text files"
    ),
    
    "china" = list(
      name = "China (Simplified)",
      encodings = c("UTF-8", "GB18030", "GBK", "GB2312"),
      default_encoding = "UTF-8",
      common_separators = c(",", "\t"),
      description = "Optimized for Simplified Chinese text files"
    ),
    
    "taiwan" = list(
      name = "Taiwan (Traditional Chinese)",
      encodings = c("UTF-8", "Big5", "Big5-HKSCS"),
      default_encoding = "UTF-8",
      common_separators = c(",", "\t"),
      description = "Optimized for Traditional Chinese text files"
    ),
    
    "korea" = list(
      name = "Korea",
      encodings = c("UTF-8", "EUC-KR", "ISO-2022-KR"),
      default_encoding = "UTF-8",
      common_separators = c(",", "\t"),
      description = "Optimized for Korean text files"
    ),
    
    "russia" = list(
      name = "Russia/Eastern Europe",
      encodings = c("UTF-8", "Windows-1251", "KOI8-R", "ISO-8859-5"),
      default_encoding = "UTF-8",
      common_separators = c(",", ";", "\t"),
      description = "Optimized for Russian and Cyrillic text files"
    ),
    
    "europe" = list(
      name = "Western Europe",
      encodings = c("UTF-8", "ISO-8859-1", "Windows-1252", "ISO-8859-15"),
      default_encoding = "UTF-8",
      common_separators = c(",", ";", "\t"),
      description = "Optimized for Western European text files"
    ),
    
    "global" = list(
      name = "Global/Universal",
      encodings = c("UTF-8", "UTF-8-BOM", "UTF-16LE", "UTF-16BE"),
      default_encoding = "UTF-8",
      common_separators = c(",", "\t"),
      description = "Universal profile for international files"
    )
  )
  
  if (!region %in% names(profiles)) {
    warning(sprintf("Unknown region '%s'. Using 'global' profile.", region))
    region <- "global"
  }
  
  profile <- profiles[[region]]
  
  if (apply_immediately) {
    readflex_config(default_encodings = profile$encodings)
    message(sprintf("Applied %s profile: %s", profile$name, profile$description))
  }
  
  return(profile)
}

#' Detect system locale for auto-profile selection
#' 
#' @return Character region identifier
detect_system_locale <- function() {
  locale <- Sys.getlocale("LC_CTYPE")
  
  if (grepl("ja_JP|japanese", locale, ignore.case = TRUE)) {
    return("japan")
  } else if (grepl("zh_CN|chinese", locale, ignore.case = TRUE)) {
    return("china")
  } else if (grepl("zh_TW|zh_HK", locale, ignore.case = TRUE)) {
    return("taiwan")
  } else if (grepl("ko_KR|korean", locale, ignore.case = TRUE)) {
    return("korea")
  } else if (grepl("ru_RU|russian", locale, ignore.case = TRUE)) {
    return("russia")
  } else if (grepl("^(en|de|fr|es|it|pt|nl|sv|no|da|fi)_", locale, ignore.case = TRUE)) {
    return("europe")
  } else {
    return("global")
  }
}

#' Save current configuration to file
#' 
#' @param config_file Character path to configuration file
#' @return Logical success status
save_readflex_config <- function(config_file = NULL) {
  if (is.null(config_file)) {
    config_dir <- file.path(Sys.getenv("HOME"), ".readflex")
    if (!dir.exists(config_dir)) {
      dir.create(config_dir, recursive = TRUE)
    }
    config_file <- file.path(config_dir, "config.json")
  }
  
  config_list <- as.list(.readflex_config)
  
  tryCatch({
    if (requireNamespace("jsonlite", quietly = TRUE)) {
      jsonlite::write_json(config_list, config_file, pretty = TRUE)
    } else {
      # Fallback: save as R object
      config_file <- sub("\\.json$", ".rds", config_file)
      saveRDS(config_list, config_file)
    }
    
    message(sprintf("Configuration saved to: %s", config_file))
    return(TRUE)
    
  }, error = function(e) {
    warning(sprintf("Failed to save configuration: %s", e$message))
    return(FALSE)
  })
}

#' Load configuration from file
#' 
#' @param config_file Character path to configuration file
#' @param apply_immediately Logical whether to apply loaded config
#' @return List with loaded configuration
load_readflex_config <- function(config_file = NULL, apply_immediately = TRUE) {
  if (is.null(config_file)) {
    # Try to find default config file
    config_dir <- file.path(Sys.getenv("HOME"), ".readflex")
    json_file <- file.path(config_dir, "config.json")
    rds_file <- file.path(config_dir, "config.rds")
    
    if (file.exists(json_file)) {
      config_file <- json_file
    } else if (file.exists(rds_file)) {
      config_file <- rds_file
    } else {
      stop("No configuration file found. Use save_readflex_config() first.")
    }
  }
  
  if (!file.exists(config_file)) {
    stop(sprintf("Configuration file not found: %s", config_file))
  }
  
  tryCatch({
    if (grepl("\\.json$", config_file) && requireNamespace("jsonlite", quietly = TRUE)) {
      config_list <- jsonlite::read_json(config_file, simplifyVector = TRUE)
    } else {
      config_list <- readRDS(config_file)
    }
    
    if (apply_immediately) {
      # Apply loaded configuration
      for (key in names(config_list)) {
        .readflex_config[[key]] <- config_list[[key]]
      }
      message(sprintf("Configuration loaded from: %s", config_file))
    }
    
    return(config_list)
    
  }, error = function(e) {
    stop(sprintf("Failed to load configuration: %s", e$message))
  })
}

#' Print method for configuration
#' 
#' @param config List with readflex configuration
print_readflex_config <- function(config = NULL) {
  if (is.null(config)) {
    config <- as.list(.readflex_config)
  }
  
  cat("READFLEX CONFIGURATION\n")
  cat(rep("=", 40), "\n")
  
  cat(sprintf(" Default encodings: %d encodings\n", length(config$default_encodings)))
  cat("   ", paste(head(config$default_encodings, 6), collapse = ", "))
  if (length(config$default_encodings) > 6) {
    cat(sprintf(", ... (%d more)", length(config$default_encodings) - 6))
  }
  cat("\n")
  
  cat(sprintf(" Cache enabled: %s\n", if (config$cache_enabled) "Yes" else "No"))
  cat(sprintf(" Cache TTL: %d hours\n", config$cache_ttl_hours))
  cat(sprintf(" Parallel processing: %s\n", if (config$parallel_enabled) "Enabled" else "Disabled"))
  cat(sprintf(" Statistics collection: %s\n", if (config$stats_enabled) "Enabled" else "Disabled"))
  cat(sprintf(" Verbose level: %d\n", config$verbose_level))
  cat(sprintf(" Max file size: %d MB\n", config$max_file_size_mb))
  cat(sprintf(" Encoding detection lines: %d\n", config$guess_n_max))
  
  cat(rep("=", 40), "\n")
}

#' Show available regional profiles
#' 
#' @return Invisible data frame with profile information
show_readflex_profiles <- function() {
  regions <- c("japan", "china", "taiwan", "korea", "russia", "europe", "global")
  
  profiles_info <- data.frame(
    Region = character(0),
    Name = character(0),
    Primary_Encodings = character(0),
    Description = character(0),
    stringsAsFactors = FALSE
  )
  
  for (region in regions) {
    profile <- readflex_profile(region, apply_immediately = FALSE)
    profiles_info <- rbind(profiles_info, data.frame(
      Region = region,
      Name = profile$name,
      Primary_Encodings = paste(head(profile$encodings, 3), collapse = ", "),
      Description = profile$description,
      stringsAsFactors = FALSE
    ))
  }
  
  cat(" AVAILABLE READFLEX PROFILES\n")
  cat(rep("=", 60), "\n")
  
  for (i in 1:nrow(profiles_info)) {
    cat(sprintf(" %s (%s)\n", profiles_info$Name[i], profiles_info$Region[i]))
    cat(sprintf("   Encodings: %s\n", profiles_info$Primary_Encodings[i]))
    cat(sprintf("   %s\n\n", profiles_info$Description[i]))
  }
  
  cat(" Usage: readflex_profile('region_name', apply_immediately = TRUE)\n")
  cat(" Auto-detect: readflex_profile('auto', apply_immediately = TRUE)\n")
  
  invisible(profiles_info)
}

#' Enhanced cache management
#' 
#' @param action Character action to perform ("info", "clear", "optimize")
#' @return Depends on action
manage_readflex_cache <- function(action = "info") {
  switch(action,
    "info" = {
      cache_size <- length(ls(.encoding_cache))
      stats_size <- length(ls(.performance_stats))
      
      cat(" READFLEX CACHE STATUS\n")
      cat(rep("=", 30), "\n")
      cat(sprintf(" Cached encodings: %d entries\n", cache_size))
      cat(sprintf(" Performance stats: %d entries\n", stats_size))
      cat(sprintf("  Cache enabled: %s\n", if (get_readflex_config("cache_enabled")) "Yes" else "No"))
      cat(sprintf(" Cache TTL: %d hours\n", get_readflex_config("cache_ttl_hours")))
      
      if (cache_size > 0) {
        # Show cache age distribution
        cache_ages <- sapply(ls(.encoding_cache), function(key) {
          entry <- .encoding_cache[[key]]
          difftime(Sys.time(), entry$timestamp, units = "hours")
        })
        
        cat(sprintf(" Cache age: %.1f - %.1f hours (avg: %.1f)\n", 
                    min(cache_ages), max(cache_ages), mean(cache_ages)))
      }
    },
    
    "clear" = {
      clear_readflex_cache(clear_cache = TRUE, clear_stats = TRUE)
      cat(" Cache and statistics cleared\n")
    },
    
    "optimize" = {
      # Remove expired cache entries
      ttl_hours <- get_readflex_config("cache_ttl_hours")
      current_time <- Sys.time()
      
      cache_keys <- ls(.encoding_cache)
      removed_count <- 0
      
      for (key in cache_keys) {
        entry <- .encoding_cache[[key]]
        if (difftime(current_time, entry$timestamp, units = "hours") > ttl_hours) {
          rm(list = key, envir = .encoding_cache)
          removed_count <- removed_count + 1
        }
      }
      
      cat(sprintf(" Cache optimized: %d expired entries removed\n", removed_count))
    },
    
    stop(sprintf("Unknown action: %s. Use 'info', 'clear', or 'optimize'", action))
  )
}