library(testthat)
library(readflex)

# Create test data files for testing
setup_test_files <- function() {
  # Create temp directory for test files
  test_dir <- tempdir()
  
  # UTF-8 CSV file
  utf8_file <- file.path(test_dir, "test_utf8.csv")
  write.csv(data.frame(
    name = c("Alice", "Bob", "Charlie"),
    age = c(25, 30, 35),
    city = c("Tokyo", "Osaka", "Kyoto")
  ), utf8_file, row.names = FALSE, fileEncoding = "UTF-8")
  
  # Shift_JIS CSV file
  sjis_file <- file.path(test_dir, "test_sjis.csv")
  write.csv(data.frame(
    name = c("田中", "佐藤", "鈴木"),
    age = c(25, 30, 35),
    city = c("東京", "大阪", "京都")
  ), sjis_file, row.names = FALSE, fileEncoding = "Shift_JIS")
  
  # Empty file
  empty_file <- file.path(test_dir, "empty.csv")
  file.create(empty_file)
  
  # Large file (simulate by creating a file with known size)
  large_file <- file.path(test_dir, "large.csv")
  # Create a file that's approximately 1MB
  large_data <- data.frame(
    col1 = rep(paste(rep("x", 100), collapse = ""), 1000),
    col2 = rep(paste(rep("y", 100), collapse = ""), 1000),
    col3 = rep(paste(rep("z", 100), collapse = ""), 1000)
  )
  write.csv(large_data, large_file, row.names = FALSE)
  
  list(
    utf8 = utf8_file,
    sjis = sjis_file,
    empty = empty_file,
    large = large_file,
    nonexistent = file.path(test_dir, "nonexistent.csv")
  )
}

# Test basic functionality
test_that("readflex reads UTF-8 files correctly", {
  files <- setup_test_files()
  
  result <- readflex(files$utf8)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 3)
  expect_true("name" %in% names(result))
  expect_true("age" %in% names(result))
  expect_true("city" %in% names(result))
})

test_that("readflex handles different encoding files", {
  files <- setup_test_files()
  
  # Test Shift_JIS file
  result_sjis <- readflex(files$sjis, verbose = TRUE)
  
  expect_s3_class(result_sjis, "data.frame")
  expect_equal(nrow(result_sjis), 3)
  expect_equal(ncol(result_sjis), 3)
})

# Test parameter validation
test_that("readflex validates parameters correctly", {
  files <- setup_test_files()
  
  # Invalid file parameter
  expect_error(readflex(123), "is.character\\(file\\)")
  expect_error(readflex(c("file1.csv", "file2.csv")), "length\\(file\\)")
  
  # Invalid guess_n_max
  expect_error(readflex(files$utf8, guess_n_max = -1), "guess_n_max > 0")
  expect_error(readflex(files$utf8, guess_n_max = "invalid"), "is.numeric\\(guess_n_max\\)")
  
  # Invalid verbose
  expect_error(readflex(files$utf8, verbose = "yes"), "is.logical\\(verbose\\)")
  expect_error(readflex(files$utf8, verbose = c(TRUE, FALSE)), "length\\(verbose\\)")
  
  # Invalid encodings
  expect_error(readflex(files$utf8, encodings = 123), "is.character\\(encodings\\)")
  expect_error(readflex(files$utf8, encodings = character(0)), "length\\(encodings\\) > 0")
  
  # Invalid stringsAsFactors
  expect_error(readflex(files$utf8, stringsAsFactors = "no"), "is.logical\\(stringsAsFactors\\)")
  expect_error(readflex(files$utf8, stringsAsFactors = c(TRUE, FALSE)), "length\\(stringsAsFactors\\)")
  
  # Invalid max_file_size_mb
  expect_error(readflex(files$utf8, max_file_size_mb = -1), "max_file_size_mb > 0")
  expect_error(readflex(files$utf8, max_file_size_mb = "invalid"), "is.numeric\\(max_file_size_mb\\)")
})

# Test error handling
test_that("readflex handles file errors correctly", {
  files <- setup_test_files()
  
  # Non-existent file
  expect_error(readflex(files$nonexistent), "File not found")
  
  # Empty file
  expect_warning(result <- readflex(files$empty), "File is empty")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

# Test file size limit
test_that("readflex respects file size limits", {
  files <- setup_test_files()
  
  # File should be readable with default limit
  expect_no_error(readflex(files$utf8))
  
  # File should be rejected with very small limit
  expect_error(
    readflex(files$large, max_file_size_mb = 0.001),
    "File size.*exceeds limit"
  )
  
  # File should be readable with large limit
  expect_no_error(readflex(files$large, max_file_size_mb = 10))
})

# Test stringsAsFactors parameter
test_that("readflex respects stringsAsFactors parameter", {
  files <- setup_test_files()
  
  # Default behavior (stringsAsFactors = FALSE)
  result_default <- readflex(files$utf8)
  expect_false(is.factor(result_default$name))
  
  # Explicit FALSE
  result_false <- readflex(files$utf8, stringsAsFactors = FALSE)
  expect_false(is.factor(result_false$name))
  
  # Explicit TRUE
  result_true <- readflex(files$utf8, stringsAsFactors = TRUE)
  expect_true(is.factor(result_true$name))
})

# Test verbose mode
test_that("readflex verbose mode works correctly", {
  files <- setup_test_files()
  
  # Capture messages in verbose mode
  expect_message(readflex(files$utf8, verbose = TRUE), "readflex")
  
  # No extra messages in non-verbose mode
  expect_silent(readflex(files$utf8, verbose = FALSE))
})

# Test custom encodings
test_that("readflex works with custom encoding lists", {
  files <- setup_test_files()
  
  # Custom encoding list
  result <- readflex(files$utf8, encodings = c("UTF-8", "latin1"))
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
})

# Test additional read.csv parameters
test_that("readflex passes additional parameters to read.csv", {
  files <- setup_test_files()
  
  # Test with different separator (should still work for comma-separated)
  result <- readflex(files$utf8, header = TRUE)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
})