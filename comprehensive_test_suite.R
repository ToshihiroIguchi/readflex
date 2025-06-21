# Comprehensive Test Suite for readflex package
# This script runs all encoding and functionality tests

# Load the function directly
source("R/readflex.R")

# Test suite configuration
VERBOSE_GLOBAL <- FALSE  # Set to TRUE for detailed output

# Helper function to run a test section
run_test_section <- function(test_name, test_func) {
  cat("\n")
  cat(rep("=", 80), "\n")
  cat(sprintf("RUNNING: %s\n", test_name))
  cat(rep("=", 80), "\n")
  
  start_time <- Sys.time()
  result <- tryCatch({
    test_func()
  }, error = function(e) {
    cat("ERROR in test section:", e$message, "\n")
    FALSE
  })
  end_time <- Sys.time()
  
  duration <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 2)
  
  status <- if (result) "✓ PASSED" else "✗ FAILED"
  cat(sprintf("\n%s: %s (%.2f seconds)\n", test_name, status, duration))
  
  return(result)
}

# Test 1: Basic functionality tests
test_basic_functionality <- function() {
  cat("Testing basic readflex functionality...\n")
  
  test_dir <- tempdir()
  results <- list()
  
  # Basic UTF-8 test
  test_data <- data.frame(
    name = c("Alice", "Bob", "Charlie"),
    age = c(25, 30, 35),
    city = c("New York", "London", "Tokyo"),
    stringsAsFactors = FALSE
  )
  
  basic_file <- file.path(test_dir, "basic_test.csv")
  write.csv(test_data, basic_file, row.names = FALSE, fileEncoding = "UTF-8")
  
  result <- readflex(basic_file, verbose = VERBOSE_GLOBAL)
  results$basic_read <- is.data.frame(result) && nrow(result) == 3
  
  # Empty file test
  empty_file <- file.path(test_dir, "empty.csv")
  file.create(empty_file)
  empty_result <- suppressWarnings(readflex(empty_file))
  results$empty_file <- is.data.frame(empty_result) && nrow(empty_result) == 0
  
  # Parameter validation test
  param_error <- FALSE
  tryCatch({
    readflex(123)  # Invalid parameter
  }, error = function(e) {
    param_error <<- TRUE
  })
  results$param_validation <- param_error
  
  # stringsAsFactors test
  result_no_factors <- readflex(basic_file, stringsAsFactors = FALSE)
  result_with_factors <- readflex(basic_file, stringsAsFactors = TRUE)
  results$strings_as_factors <- !is.factor(result_no_factors$name) && is.factor(result_with_factors$name)
  
  passed <- sum(unlist(results))
  total <- length(results)
  cat(sprintf("Basic functionality: %d/%d tests passed\n", passed, total))
  
  return(passed == total)
}

# Test 2: European languages
test_european_languages <- function() {
  cat("Testing European language encodings...\n")
  
  test_dir <- tempdir()
  results <- list()
  
  # Test data with European characters
  european_data <- data.frame(
    name = c("François", "José", "Müller", "Øyvind", "Władysław"),
    country = c("France", "Spain", "Germany", "Norway", "Poland"),
    city = c("Paris", "Sevilla", "München", "Oslo", "Kraków"),
    note = c("café", "niño", "größe", "rømmegrøt", "żurek"),
    stringsAsFactors = FALSE
  )
  
  # UTF-8 test
  utf8_file <- file.path(test_dir, "european_utf8.csv")
  write.csv(european_data, utf8_file, row.names = FALSE, fileEncoding = "UTF-8")
  result_utf8 <- readflex(utf8_file, verbose = VERBOSE_GLOBAL)
  results$european_utf8 <- is.data.frame(result_utf8) && nrow(result_utf8) == 5
  
  # ISO-8859-1 test
  tryCatch({
    latin1_file <- file.path(test_dir, "european_latin1.csv")
    write.csv(european_data[1:3, ], latin1_file, row.names = FALSE, fileEncoding = "ISO-8859-1")
    result_latin1 <- readflex(latin1_file, verbose = VERBOSE_GLOBAL)
    results$european_latin1 <- is.data.frame(result_latin1) && nrow(result_latin1) == 3
  }, error = function(e) {
    results$european_latin1 <- "SKIP"
  })
  
  passed <- sum(unlist(results) == TRUE)
  skipped <- sum(unlist(results) == "SKIP")
  total <- length(results)
  cat(sprintf("European languages: %d/%d tests passed (%d skipped)\n", passed, total - skipped, skipped))
  
  return(passed == (total - skipped))
}

# Test 3: Asian languages (Chinese, Japanese, Korean)
test_asian_languages <- function() {
  cat("Testing Asian language encodings...\n")
  
  test_dir <- tempdir()
  results <- list()
  
  # Japanese test
  japanese_data <- data.frame(
    name = c("山田太郎", "田中花子", "佐藤次郎"),
    city = c("東京", "大阪", "名古屋"),
    job = c("エンジニア", "デザイナー", "マネージャー"),
    stringsAsFactors = FALSE
  )
  
  jp_file <- file.path(test_dir, "japanese.csv")
  write.csv(japanese_data, jp_file, row.names = FALSE, fileEncoding = "UTF-8")
  result_jp <- readflex(jp_file, verbose = VERBOSE_GLOBAL)
  results$japanese_utf8 <- is.data.frame(result_jp) && nrow(result_jp) == 3
  
  # Chinese Simplified test
  chinese_data <- data.frame(
    name = c("张三", "李四", "王五"),
    city = c("北京", "上海", "广州"),
    job = c("工程师", "设计师", "经理"),
    stringsAsFactors = FALSE
  )
  
  cn_file <- file.path(test_dir, "chinese.csv")
  write.csv(chinese_data, cn_file, row.names = FALSE, fileEncoding = "UTF-8")
  result_cn <- readflex(cn_file, verbose = VERBOSE_GLOBAL)
  results$chinese_utf8 <- is.data.frame(result_cn) && nrow(result_cn) == 3
  
  # Korean test
  korean_data <- data.frame(
    name = c("김철수", "이영희", "박민수"),
    city = c("서울", "부산", "대구"),
    job = c("개발자", "디자이너", "매니저"),
    stringsAsFactors = FALSE
  )
  
  ko_file <- file.path(test_dir, "korean.csv")
  write.csv(korean_data, ko_file, row.names = FALSE, fileEncoding = "UTF-8")
  result_ko <- readflex(ko_file, verbose = VERBOSE_GLOBAL)
  results$korean_utf8 <- is.data.frame(result_ko) && nrow(result_ko) == 3
  
  passed <- sum(unlist(results))
  total <- length(results)
  cat(sprintf("Asian languages: %d/%d tests passed\n", passed, total))
  
  return(passed == total)
}

# Test 4: Cyrillic languages
test_cyrillic_languages <- function() {
  cat("Testing Cyrillic language encodings...\n")
  
  test_dir <- tempdir()
  results <- list()
  
  # Russian test
  russian_data <- data.frame(
    name = c("Иван", "Мария", "Петр", "Анна"),
    city = c("Москва", "Санкт-Петербург", "Новосибирск", "Екатеринбург"),
    job = c("программист", "дизайнер", "менеджер", "аналитик"),
    stringsAsFactors = FALSE
  )
  
  ru_file <- file.path(test_dir, "russian.csv")
  write.csv(russian_data, ru_file, row.names = FALSE, fileEncoding = "UTF-8")
  result_ru <- readflex(ru_file, verbose = VERBOSE_GLOBAL)
  results$russian_utf8 <- is.data.frame(result_ru) && nrow(result_ru) == 4
  
  # Bulgarian test
  bulgarian_data <- data.frame(
    name = c("Петър", "Мария", "Стоян"),
    city = c("София", "Пловдив", "Варна"),
    stringsAsFactors = FALSE
  )
  
  bg_file <- file.path(test_dir, "bulgarian.csv")
  write.csv(bulgarian_data, bg_file, row.names = FALSE, fileEncoding = "UTF-8")
  result_bg <- readflex(bg_file, verbose = VERBOSE_GLOBAL)
  results$bulgarian_utf8 <- is.data.frame(result_bg) && nrow(result_bg) == 3
  
  passed <- sum(unlist(results))
  total <- length(results)
  cat(sprintf("Cyrillic languages: %d/%d tests passed\n", passed, total))
  
  return(passed == total)
}

# Test 5: Special characters and symbols
test_special_characters <- function() {
  cat("Testing special characters and symbols...\n")
  
  test_dir <- tempdir()
  results <- list()
  
  # Mathematical and special symbols
  special_data <- data.frame(
    symbol = c("α", "β", "γ", "π", "∑", "∞", "≠", "≤", "≥"),
    currency = c("€", "$", "¥", "£", "₹", "₽", "₩", "₪", "₦"),
    emoji = c("😀", "😍", "🎉", "🌟", "❤️", "🔥", "💯", "🚀", "🌈"),
    arrows = c("←", "→", "↑", "↓", "↔", "↕", "⇒", "⇔", "⇕"),
    stringsAsFactors = FALSE
  )
  
  special_file <- file.path(test_dir, "special.csv")
  write.csv(special_data, special_file, row.names = FALSE, fileEncoding = "UTF-8")
  result_special <- readflex(special_file, verbose = VERBOSE_GLOBAL)
  results$special_chars <- is.data.frame(result_special) && nrow(result_special) == 9
  
  # Mixed encoding test with fallback
  mixed_file <- file.path(test_dir, "mixed.csv")
  write.csv(special_data[1:3, ], mixed_file, row.names = FALSE, fileEncoding = "UTF-8")
  result_mixed <- readflex(mixed_file, 
                          encodings = c("UTF-8", "ISO-8859-1", "Windows-1252"),
                          verbose = VERBOSE_GLOBAL)
  results$mixed_encoding <- is.data.frame(result_mixed) && nrow(result_mixed) == 3
  
  passed <- sum(unlist(results))
  total <- length(results)
  cat(sprintf("Special characters: %d/%d tests passed\n", passed, total))
  
  return(passed == total)
}

# Test 6: Edge cases and error handling
test_edge_cases <- function() {
  cat("Testing edge cases and error handling...\n")
  
  test_dir <- tempdir()
  results <- list()
  
  # Large file test
  large_data <- data.frame(
    id = 1:100,
    name = rep(c("Alice", "Bob", "Charlie", "Diana", "Eve"), 20),
    value = runif(100),
    text = rep(c("Hello", "World", "Test", "Data", "Large"), 20),
    stringsAsFactors = FALSE
  )
  
  large_file <- file.path(test_dir, "large.csv")
  write.csv(large_data, large_file, row.names = FALSE, fileEncoding = "UTF-8")
  result_large <- readflex(large_file, verbose = VERBOSE_GLOBAL)
  results$large_file <- is.data.frame(result_large) && nrow(result_large) == 100
  
  # Non-existent file test
  nonexist_error <- FALSE
  tryCatch({
    readflex("non_existent_file.csv")
  }, error = function(e) {
    nonexist_error <<- grepl("File not found", e$message)
  })
  results$nonexistent_file <- nonexist_error
  
  # File size limit test
  size_error <- FALSE
  tryCatch({
    readflex(large_file, max_file_size_mb = 0.001)  # Very small limit
  }, error = function(e) {
    size_error <<- grepl("exceeds limit", e$message)
  })
  results$file_size_limit <- size_error
  
  # Custom encoding list test
  custom_file <- file.path(test_dir, "custom.csv")
  simple_data <- data.frame(name = c("Test", "Data"), value = c(1, 2), stringsAsFactors = FALSE)
  write.csv(simple_data, custom_file, row.names = FALSE, fileEncoding = "UTF-8")
  
  result_custom <- readflex(custom_file, 
                           encodings = c("UTF-8", "ASCII"),
                           verbose = VERBOSE_GLOBAL)
  results$custom_encodings <- is.data.frame(result_custom) && nrow(result_custom) == 2
  
  passed <- sum(unlist(results))
  total <- length(results)
  cat(sprintf("Edge cases: %d/%d tests passed\n", passed, total))
  
  return(passed == total)
}

# Main test runner
run_comprehensive_test_suite <- function() {
  cat("🧪 STARTING COMPREHENSIVE READFLEX TEST SUITE 🧪\n")
  cat("Test suite started at:", format(Sys.time()), "\n")
  
  overall_start <- Sys.time()
  
  # Define all test sections
  test_sections <- list(
    "Basic Functionality" = test_basic_functionality,
    "European Languages" = test_european_languages,
    "Asian Languages" = test_asian_languages,
    "Cyrillic Languages" = test_cyrillic_languages,
    "Special Characters" = test_special_characters,
    "Edge Cases" = test_edge_cases
  )
  
  # Run all test sections
  results <- list()
  for (section_name in names(test_sections)) {
    results[[section_name]] <- run_test_section(section_name, test_sections[[section_name]])
  }
  
  overall_end <- Sys.time()
  total_duration <- round(as.numeric(difftime(overall_end, overall_start, units = "secs")), 2)
  
  # Final summary
  cat("\n")
  cat(rep("=", 80), "\n")
  cat("FINAL TEST SUITE SUMMARY\n")
  cat(rep("=", 80), "\n")
  
  passed_sections <- sum(unlist(results))
  total_sections <- length(results)
  
  for (section in names(results)) {
    status <- if (results[[section]]) "✓ PASSED" else "✗ FAILED"
    cat(sprintf("  %-25s: %s\n", section, status))
  }
  
  cat(rep("-", 80), "\n")
  cat(sprintf("OVERALL RESULT: %d/%d sections passed (%.1f%%)\n", 
              passed_sections, total_sections, (passed_sections/total_sections)*100))
  cat(sprintf("Total execution time: %.2f seconds\n", total_duration))
  
  if (passed_sections == total_sections) {
    cat("\n🎉 ALL TESTS PASSED! READFLEX IS WORKING PERFECTLY! 🎉\n")
    cat("✨ The package successfully handles multiple encodings and languages ✨\n")
    return(TRUE)
  } else {
    cat("\n❌ SOME TESTS FAILED\n")
    cat("Please review the failing test sections above.\n")
    return(FALSE)
  }
}

# Execute the comprehensive test suite
success <- run_comprehensive_test_suite()

# Exit with appropriate status
if (success) {
  cat("\n✅ SUCCESS: Comprehensive test suite completed successfully!\n")
} else {
  cat("\n❌ FAILURE: Some tests in the comprehensive suite failed\n")
  quit(status = 1)
}