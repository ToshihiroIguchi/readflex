test_that("readflex basic functionality works", {
  # Create test data
  test_dir <- tempdir()
  test_file <- file.path(test_dir, "test.csv")
  
  # Basic UTF-8 CSV
  test_data <- data.frame(
    name = c("Alice", "Bob", "Charlie"),
    age = c(25, 30, 35),
    stringsAsFactors = FALSE
  )
  write.csv(test_data, test_file, row.names = FALSE, fileEncoding = "UTF-8")
  
  # Test basic functionality
  result <- readflex(test_file, verbose = FALSE)
  
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 2)
  expect_equal(result$name, c("Alice", "Bob", "Charlie"))
  expect_equal(result$age, c(25, 30, 35))
})

test_that("readflex handles encoding detection", {
  # Create test data with international characters
  test_dir <- tempdir()
  test_file <- file.path(test_dir, "international.csv")
  
  # Japanese characters
  japanese_data <- data.frame(
    name = c("山田太郎", "田中花子"),
    city = c("東京", "大阪"),
    stringsAsFactors = FALSE
  )
  write.csv(japanese_data, test_file, row.names = FALSE, fileEncoding = "UTF-8")
  
  result <- readflex(test_file, verbose = FALSE)
  
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_true(any(grepl("山田", result$name)))
})

test_that("readflex parameter validation works", {
  expect_error(readflex(123), "character")
  expect_error(readflex("nonexistent_file.csv"))
})

test_that("readflex stringsAsFactors parameter works", {
  test_dir <- tempdir()
  test_file <- file.path(test_dir, "factors.csv")
  
  test_data <- data.frame(
    category = c("A", "B", "C"),
    value = c(1, 2, 3),
    stringsAsFactors = FALSE
  )
  write.csv(test_data, test_file, row.names = FALSE)
  
  # Test stringsAsFactors = FALSE
  result_false <- readflex(test_file, stringsAsFactors = FALSE)
  expect_false(is.factor(result_false$category))
  
  # Test stringsAsFactors = TRUE
  result_true <- readflex(test_file, stringsAsFactors = TRUE)
  expect_true(is.factor(result_true$category))
})