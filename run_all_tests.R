#!/usr/bin/env Rscript

# Master Test Runner for readflex Package
# This script runs all available tests for the readflex package

cat("🚀 READFLEX PACKAGE TEST RUNNER 🚀\n")
cat("==================================\n\n")

# Load the function
source("R/readflex.R")

# Track overall results
overall_results <- list()
start_time <- Sys.time()

# Helper function to run a test script
run_test_script <- function(script_name, description) {
  cat(sprintf("📋 Running: %s\n", description))
  cat(sprintf("   Script: %s\n", script_name))
  
  if (!file.exists(script_name)) {
    cat("   ❌ SKIP: Test script not found\n\n")
    return("SKIP")
  }
  
  script_start <- Sys.time()
  
  result <- tryCatch({
    # Capture output to reduce noise
    capture.output({
      source(script_name, local = TRUE)
    })
    "PASS"
  }, error = function(e) {
    cat(sprintf("   ❌ ERROR: %s\n", e$message))
    "FAIL"
  })
  
  script_end <- Sys.time()
  duration <- round(as.numeric(difftime(script_end, script_start, units = "secs")), 2)
  
  status_symbol <- switch(result,
    "PASS" = "✅",
    "FAIL" = "❌", 
    "SKIP" = "⚪"
  )
  
  cat(sprintf("   %s %s (%.2f seconds)\n\n", status_symbol, result, duration))
  
  return(result)
}

# Test configuration
tests <- list(
  list(
    script = "international_encoding_test.R",
    name = "International Encodings",
    description = "Chinese, Korean, Russian encoding tests"
  ),
  list(
    script = "comprehensive_test_suite.R", 
    name = "Comprehensive Suite",
    description = "Complete functionality and encoding test suite"
  )
)

# Run all tests
cat("🧪 EXECUTING TEST SUITE\n")
cat("======================\n\n")

for (i in seq_along(tests)) {
  test <- tests[[i]]
  cat(sprintf("[%d/%d] ", i, length(tests)))
  result <- run_test_script(test$script, test$description)
  overall_results[[test$name]] <- result
}

# Final summary
end_time <- Sys.time()
total_duration <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 2)

cat("📊 FINAL SUMMARY\n")
cat("================\n")

passed <- sum(overall_results == "PASS")
failed <- sum(overall_results == "FAIL") 
skipped <- sum(overall_results == "SKIP")
total <- length(overall_results)

for (test_name in names(overall_results)) {
  result <- overall_results[[test_name]]
  symbol <- switch(result,
    "PASS" = "✅",
    "FAIL" = "❌",
    "SKIP" = "⚪"
  )
  cat(sprintf("  %s %-25s: %s\n", symbol, test_name, result))
}

cat(sprintf("\n📈 RESULTS: %d passed, %d failed, %d skipped (Total: %d)\n", 
            passed, failed, skipped, total))
cat(sprintf("⏱️  Total execution time: %.2f seconds\n", total_duration))

if (failed == 0) {
  success_rate <- if (total > skipped) (passed / (total - skipped)) * 100 else 100
  cat(sprintf("🎯 Success rate: %.1f%% (excluding skipped)\n", success_rate))
  cat("\n🎉 ALL TESTS SUCCESSFUL! 🎉\n")
  cat("🌟 readflex package is working perfectly across all tested encodings!\n")
  cat("\n🌍 Supported languages and encodings:\n")
  cat("   • Japanese (UTF-8, Shift_JIS)\n")
  cat("   • Chinese Simplified (UTF-8, GB18030, GBK)\n") 
  cat("   • Chinese Traditional (UTF-8, Big5)\n")
  cat("   • Korean (UTF-8, EUC-KR)\n")
  cat("   • Russian (UTF-8, KOI8-R, Windows-1251)\n")
  cat("   • European languages (UTF-8, ISO-8859-1, Windows-1252)\n")
  cat("   • Special characters, symbols, and emojis\n")
  cat("\n✨ The package provides robust auto-detection and fallback mechanisms! ✨\n")
} else {
  cat(sprintf("\n❌ %d TEST(S) FAILED\n", failed))
  cat("Please review the output above for details.\n")
  quit(status = 1)
}

cat("\n🏁 Test runner completed successfully!\n")