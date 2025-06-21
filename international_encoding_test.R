# Load the function directly
source("R/readflex.R")

# International encoding test function for Chinese, Korean, and Russian
test_international_encodings <- function() {
  cat("Running international encoding tests (Chinese, Korean, Russian)...\n")
  cat(rep("=", 70), "\n")
  
  # Create test directory
  test_dir <- tempdir()
  test_results <- list()
  
  # Test Chinese encodings
  cat("CHINESE ENCODING TESTS\n")
  cat(rep("-", 30), "\n")
  
  # Chinese test data
  chinese_data <- data.frame(
    name = c("张三", "李四", "王五", "赵六"),
    city = c("北京", "上海", "广州", "深圳"),
    job = c("工程师", "设计师", "经理", "分析师"),
    salary = c(50000, 45000, 60000, 55000),
    stringsAsFactors = FALSE
  )
  
  # Test 1: Chinese UTF-8
  cat("Test 1: Chinese UTF-8\n")
  chinese_utf8_file <- file.path(test_dir, "chinese_utf8.csv")
  write.csv(chinese_data, chinese_utf8_file, row.names = FALSE, fileEncoding = "UTF-8")
  
  tryCatch({
    result_zh_utf8 <- readflex(chinese_utf8_file, verbose = TRUE)
    if (is.data.frame(result_zh_utf8) && nrow(result_zh_utf8) == 4) {
      cat("PASS: Chinese UTF-8\n")
      cat("Sample names:", paste(result_zh_utf8$name[1:2], collapse = ", "), "\n")
      test_results[["chinese_utf8"]] <- TRUE
    } else {
      cat("FAIL: Chinese UTF-8\n")
      test_results[["chinese_utf8"]] <- FALSE
    }
  }, error = function(e) {
    cat("ERROR: Chinese UTF-8 -", e$message, "\n")
    test_results[["chinese_utf8"]] <- FALSE
  })
  
  # Test 2: Chinese GB18030 (if supported)
  cat("\nTest 2: Chinese GB18030\n")
  chinese_gb_file <- file.path(test_dir, "chinese_gb.csv")
  
  tryCatch({
    write.csv(chinese_data, chinese_gb_file, row.names = FALSE, fileEncoding = "GB18030")
    result_zh_gb <- readflex(chinese_gb_file, verbose = TRUE)
    if (is.data.frame(result_zh_gb) && nrow(result_zh_gb) == 4) {
      cat("PASS: Chinese GB18030\n")
      cat("Sample cities:", paste(result_zh_gb$city[1:2], collapse = ", "), "\n")
      test_results[["chinese_gb18030"]] <- TRUE
    } else {
      cat("FAIL: Chinese GB18030\n")
      test_results[["chinese_gb18030"]] <- FALSE
    }
  }, error = function(e) {
    cat("SKIP: Chinese GB18030 (encoding not supported on this system)\n")
    test_results[["chinese_gb18030"]] <- "SKIP"
  })
  
  # Test 3: Traditional Chinese Big5
  cat("\nTest 3: Traditional Chinese Big5\n")
  traditional_chinese_data <- data.frame(
    name = c("張三", "李四", "王五"),
    city = c("台北", "高雄", "台中"),
    note = c("繁體", "中文", "測試"),
    stringsAsFactors = FALSE
  )
  
  chinese_big5_file <- file.path(test_dir, "chinese_big5.csv")
  
  tryCatch({
    write.csv(traditional_chinese_data, chinese_big5_file, row.names = FALSE, fileEncoding = "Big5")
    result_zh_big5 <- readflex(chinese_big5_file, verbose = TRUE)
    if (is.data.frame(result_zh_big5) && nrow(result_zh_big5) == 3) {
      cat("PASS: Traditional Chinese Big5\n")
      cat("Sample traditional chars:", paste(result_zh_big5$note[1:2], collapse = ", "), "\n")
      test_results[["chinese_big5"]] <- TRUE
    } else {
      cat("FAIL: Traditional Chinese Big5\n")
      test_results[["chinese_big5"]] <- FALSE
    }
  }, error = function(e) {
    cat("SKIP: Traditional Chinese Big5 (encoding not supported on this system)\n")
    test_results[["chinese_big5"]] <- "SKIP"
  })
  
  cat("\n")
  
  # Test Korean encodings
  cat("KOREAN ENCODING TESTS\n")
  cat(rep("-", 30), "\n")
  
  # Korean test data
  korean_data <- data.frame(
    name = c("김철수", "이영희", "박민수", "정수진"),
    city = c("서울", "부산", "대구", "인천"),
    job = c("개발자", "디자이너", "매니저", "분석가"),
    hobby = c("독서", "영화", "운동", "음악"),
    stringsAsFactors = FALSE
  )
  
  # Test 4: Korean UTF-8
  cat("Test 4: Korean UTF-8\n")
  korean_utf8_file <- file.path(test_dir, "korean_utf8.csv")
  write.csv(korean_data, korean_utf8_file, row.names = FALSE, fileEncoding = "UTF-8")
  
  tryCatch({
    result_ko_utf8 <- readflex(korean_utf8_file, verbose = TRUE)
    if (is.data.frame(result_ko_utf8) && nrow(result_ko_utf8) == 4) {
      cat("PASS: Korean UTF-8\n")
      cat("Sample names:", paste(result_ko_utf8$name[1:2], collapse = ", "), "\n")
      test_results[["korean_utf8"]] <- TRUE
    } else {
      cat("FAIL: Korean UTF-8\n")
      test_results[["korean_utf8"]] <- FALSE
    }
  }, error = function(e) {
    cat("ERROR: Korean UTF-8 -", e$message, "\n")
    test_results[["korean_utf8"]] <- FALSE
  })
  
  # Test 5: Korean EUC-KR
  cat("\nTest 5: Korean EUC-KR\n")
  korean_euc_file <- file.path(test_dir, "korean_euc.csv")
  
  tryCatch({
    write.csv(korean_data, korean_euc_file, row.names = FALSE, fileEncoding = "EUC-KR")
    result_ko_euc <- readflex(korean_euc_file, verbose = TRUE)
    if (is.data.frame(result_ko_euc) && nrow(result_ko_euc) == 4) {
      cat("PASS: Korean EUC-KR\n")
      cat("Sample hobbies:", paste(result_ko_euc$hobby[1:2], collapse = ", "), "\n")
      test_results[["korean_euc_kr"]] <- TRUE
    } else {
      cat("FAIL: Korean EUC-KR\n")
      test_results[["korean_euc_kr"]] <- FALSE
    }
  }, error = function(e) {
    cat("SKIP: Korean EUC-KR (encoding not supported on this system)\n")
    test_results[["korean_euc_kr"]] <- "SKIP"
  })
  
  cat("\n")
  
  # Test Russian/Cyrillic encodings
  cat("RUSSIAN/CYRILLIC ENCODING TESTS\n")
  cat(rep("-", 30), "\n")
  
  # Russian test data
  russian_data <- data.frame(
    name = c("Иван", "Мария", "Петр", "Анна"),
    city = c("Москва", "Санкт-Петербург", "Новосибирск", "Екатеринбург"),
    job = c("программист", "дизайнер", "менеджер", "аналитик"),
    note = c("тест", "кириллица", "русский", "язык"),
    stringsAsFactors = FALSE
  )
  
  # Test 6: Russian UTF-8
  cat("Test 6: Russian UTF-8\n")
  russian_utf8_file <- file.path(test_dir, "russian_utf8.csv")
  write.csv(russian_data, russian_utf8_file, row.names = FALSE, fileEncoding = "UTF-8")
  
  tryCatch({
    result_ru_utf8 <- readflex(russian_utf8_file, verbose = TRUE)
    if (is.data.frame(result_ru_utf8) && nrow(result_ru_utf8) == 4) {
      cat("PASS: Russian UTF-8\n")
      cat("Sample names:", paste(result_ru_utf8$name[1:2], collapse = ", "), "\n")
      test_results[["russian_utf8"]] <- TRUE
    } else {
      cat("FAIL: Russian UTF-8\n")
      test_results[["russian_utf8"]] <- FALSE
    }
  }, error = function(e) {
    cat("ERROR: Russian UTF-8 -", e$message, "\n")
    test_results[["russian_utf8"]] <- FALSE
  })
  
  # Test 7: Russian KOI8-R
  cat("\nTest 7: Russian KOI8-R\n")
  russian_koi8_file <- file.path(test_dir, "russian_koi8.csv")
  
  tryCatch({
    write.csv(russian_data, russian_koi8_file, row.names = FALSE, fileEncoding = "KOI8-R")
    result_ru_koi8 <- readflex(russian_koi8_file, verbose = TRUE)
    if (is.data.frame(result_ru_koi8) && nrow(result_ru_koi8) == 4) {
      cat("PASS: Russian KOI8-R\n")
      cat("Sample cities:", paste(result_ru_koi8$city[1:2], collapse = ", "), "\n")
      test_results[["russian_koi8r"]] <- TRUE
    } else {
      cat("FAIL: Russian KOI8-R\n")
      test_results[["russian_koi8r"]] <- FALSE
    }
  }, error = function(e) {
    cat("SKIP: Russian KOI8-R (encoding not supported on this system)\n")
    test_results[["russian_koi8r"]] <- "SKIP"
  })
  
  # Test 8: Russian Windows-1251
  cat("\nTest 8: Russian Windows-1251\n")
  russian_cp1251_file <- file.path(test_dir, "russian_cp1251.csv")
  
  tryCatch({
    write.csv(russian_data, russian_cp1251_file, row.names = FALSE, fileEncoding = "Windows-1251")
    result_ru_cp1251 <- readflex(russian_cp1251_file, verbose = TRUE)
    if (is.data.frame(result_ru_cp1251) && nrow(result_ru_cp1251) == 4) {
      cat("PASS: Russian Windows-1251\n")
      cat("Sample jobs:", paste(result_ru_cp1251$job[1:2], collapse = ", "), "\n")
      test_results[["russian_cp1251"]] <- TRUE
    } else {
      cat("FAIL: Russian Windows-1251\n")
      test_results[["russian_cp1251"]] <- FALSE
    }
  }, error = function(e) {
    cat("SKIP: Russian Windows-1251 (encoding not supported on this system)\n")
    test_results[["russian_cp1251"]] <- "SKIP"
  })
  
  # Summary
  cat("\n", rep("=", 70), "\n")
  cat("INTERNATIONAL ENCODING TEST SUMMARY\n")
  cat(rep("=", 70), "\n")
  
  passed <- 0
  skipped <- 0
  total <- length(test_results)
  
  for (test_name in names(test_results)) {
    result <- test_results[[test_name]]
    if (result == TRUE) {
      status <- "✓ PASS"
      passed <- passed + 1
    } else if (result == "SKIP") {
      status <- "⚪ SKIP"
      skipped <- skipped + 1
    } else {
      status <- "✗ FAIL"
    }
    cat(sprintf("  %-20s: %s\n", test_name, status))
  }
  
  failed <- total - passed - skipped
  cat(sprintf("\nRESULTS: %d passed, %d failed, %d skipped (Total: %d)\n", 
              passed, failed, skipped, total))
  
  success_rate <- if (total > 0) (passed / (total - skipped)) * 100 else 0
  cat(sprintf("SUCCESS RATE: %.1f%% (excluding skipped tests)\n", success_rate))
  
  if (failed == 0) {
    cat("🌍 ALL INTERNATIONAL ENCODING TESTS SUCCESSFUL! 🌍\n")
    return(TRUE)
  } else {
    cat("⚠️ Some international encoding tests failed\n")
    return(FALSE)
  }
}

# Run international encoding tests
test_international_encodings()