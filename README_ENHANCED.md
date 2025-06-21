# readflex - Universal CSV Reader for R

[![R Version](https://img.shields.io/badge/R-%E2%89%A5%203.6.0-blue.svg)](https://cran.r-project.org/)
[![License](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)
[![Build Status](https://img.shields.io/badge/Build-Passing-brightgreen.svg)]()

**readflex** is an R package that automatically detects character encodings and reads CSV files from various languages and regions. It includes performance optimizations, diagnostics, multi-format support, and analytics features.

## 🌟 Key Features

### 🌍 Universal Language Support
- **Auto-detection** of 20+ character encodings
- **Asian languages**: Japanese (UTF-8, Shift_JIS), Chinese (UTF-8, GB18030, Big5), Korean (UTF-8, EUC-KR)
- **Cyrillic languages**: Russian, Bulgarian, Serbian (UTF-8, KOI8-R, Windows-1251)
- **European languages**: French, German, Spanish, Italian (UTF-8, ISO-8859-1, Windows-1252)
- **Smart fallback** mechanisms for encoding detection failures

### ⚡ Performance Optimization
- **Parallel processing** for large files and multiple encodings
- **Intelligent caching** of encoding detection results
- **Smart encoding order** based on content analysis
- **Benchmark tools** for performance monitoring

### 🔍 Advanced Diagnostics
- **Comprehensive file analysis** with encoding confidence scores
- **Auto-fix functionality** for common encoding issues
- **Detailed error reporting** with actionable recommendations
- **Content structure analysis** (separators, headers, data types)

### 📊 Multi-Format Support
- **CSV, TSV, PSV** (comma, tab, pipe separated)
- **Excel files** (.xlsx, .xls) with encoding preservation
- **OpenDocument** (.ods) spreadsheets
- **Compressed files** (.gz, .bz2, .xz, .zip)
- **Fixed-width** text files with auto-detection
- **Batch processing** for multiple files

### ⚙️ Configuration & Profiles
- **Regional profiles** optimized for specific countries/languages
- **Global configuration** with persistent settings
- **Cache management** with TTL and optimization
- **Performance statistics** and usage analytics

### 🖥️ Integration Features
- **Shiny modules** for web applications
- **Data validation** with schema support
- **Progress indicators** for long operations
- **Web API** ready (with Plumber)

## 🚀 Quick Start

### Installation

```r
# From source
devtools::install_github("toshihiro-iguchi/readflex")

# Or install locally
R CMD INSTALL readflex
```

### Basic Usage

```r
library(readflex)

# Basic automatic encoding detection
df <- readflex("data.csv")

# With verbose output
df <- readflex("data.csv", verbose = TRUE)

# Japanese files optimization
df <- readflex("japanese_data.csv", profile = "japan")

# Enhanced version with all features
df <- readflex_enhanced("data.csv", 
                       profile = "auto",
                       use_cache = TRUE,
                       validate_data = TRUE)
```

## 📖 Comprehensive Examples

### 1. Multi-Language Files

```r
# Japanese data
japanese_df <- readflex("sales_japan.csv")
# Automatically detects Shift_JIS, EUC-JP, or UTF-8

# Chinese data (Simplified)
chinese_df <- readflex("data_china.csv", profile = "china")
# Optimized for GB18030, GBK, UTF-8

# Korean data
korean_df <- readflex("한국어_데이터.csv", profile = "korea") 
# Handles EUC-KR and UTF-8

# Russian data
russian_df <- readflex("данные.csv", profile = "russia")
# Supports KOI8-R, Windows-1251, UTF-8
```

### 2. File Diagnostics

```r
# Analyze file before reading
diag <- readflex_diagnostic("problematic_file.csv")
print(diag)

# Auto-fix common issues
df <- readflex_auto_fix("problematic_file.csv")

# Manual fix with diagnostics
if (!diag$valid) {
  # Apply recommended encoding
  df <- readflex("file.csv", encodings = diag$recommendations$encodings)
}
```

### 3. Multi-Format Support

```r
# Auto-detect format and read
df <- readflex_multi("data.xlsx")      # Excel
df <- readflex_multi("data.ods")       # OpenDocument  
df <- readflex_multi("data.tsv")       # Tab-separated
df <- readflex_multi("data.csv.gz")    # Compressed

# Batch processing
results <- readflex_batch("data/*.csv", output_format = "combined")
```

### 4. Performance & Caching

```r
# Enable performance features
readflex_config(
  cache_enabled = TRUE,
  parallel_enabled = TRUE,
  stats_enabled = TRUE
)

# High-performance reading
df <- readflex_enhanced("large_file.csv", 
                       use_parallel = TRUE,
                       use_cache = TRUE)

# View performance statistics  
stats <- readflex_stats()
print(stats)

# Benchmark against other readers
benchmark_results <- benchmark_readflex(
  test_files = c("file1.csv", "file2.csv"),
  compare_with = c("read.csv", "readr::read_csv")
)
```

### 5. Data Validation

```r
# Read with validation
df <- readflex_enhanced("data.csv", validate_data = TRUE)

# Check validation results
validation <- attr(df, "validation")
if (!validation$valid) {
  print(validation$errors)
  print(validation$warnings)
}

# Custom validation schema
schema <- list(
  columns = list(
    name = list(type = "character", max_length = 50),
    age = list(type = "numeric", range = c(0, 120)),
    email = list(type = "character", pattern = "@")
  )
)

df <- readflex("data.csv")
validation <- validate_readflex_data(df, schema = schema)
```

## ⚙️ Configuration

### Regional Profiles

```r
# Apply regional optimization
readflex_profile("japan", apply_immediately = TRUE)
readflex_profile("china", apply_immediately = TRUE) 
readflex_profile("korea", apply_immediately = TRUE)
readflex_profile("europe", apply_immediately = TRUE)

# Auto-detect based on system locale
readflex_profile("auto", apply_immediately = TRUE)

# View available profiles
show_readflex_profiles()
```

### Global Configuration

```r
# Configure global settings
readflex_config(
  default_encodings = c("UTF-8", "Shift_JIS", "GB18030"),
  cache_enabled = TRUE,
  cache_ttl_hours = 24,
  parallel_enabled = TRUE,
  max_file_size_mb = 100,
  stats_enabled = TRUE
)

# Save configuration
readflex_config(save_config = TRUE)

# Load saved configuration
load_readflex_config()
```

### Cache Management

```r
# View cache status
manage_readflex_cache("info")

# Optimize cache (remove expired entries)
manage_readflex_cache("optimize")

# Clear all cache
manage_readflex_cache("clear")
```

## 🖥️ Shiny Integration

```r
library(shiny)
library(readflex)

# Use readflex Shiny module
ui <- fluidPage(
  titlePanel("File Upload with Readflex"),
  readflex_shiny_ui("file_upload")
)

server <- function(input, output, session) {
  data <- readflex_shiny_server("file_upload")
  
  # Use the reactive data
  observe({
    req(data())
    print(summary(data()))
  })
}

# Or use the complete app
app <- create_readflex_app()
shinyApp(app$ui, app$server)
```

## 📊 Analytics & Monitoring

```r
# Enable statistics collection
readflex_config(stats_enabled = TRUE)

# View usage statistics
stats <- readflex_stats(detailed = TRUE)
print(stats)

# Generate comprehensive report
report <- generate_readflex_report(
  include_benchmarks = TRUE,
  benchmark_files = c("test1.csv", "test2.csv"),
  save_to_file = "readflex_report.json"
)
print(report)

# Performance monitoring
performance <- get_performance_stats()
```

## 🧪 Testing

```r
# Run comprehensive tests
source("final_test_runner.R")

# Test specific functionality
source("international_encoding_test.R")
source("comprehensive_test_suite.R")

# All tests in one command
source("run_all_tests.R")
```

## 🌐 Supported Encodings

| Region | Primary Encodings | Secondary Encodings |
|--------|-------------------|-------------------|
| **Japan** | UTF-8, Shift_JIS | EUC-JP, ISO-2022-JP, CP932 |
| **China** | UTF-8, GB18030 | GBK, GB2312 |
| **Taiwan/HK** | UTF-8, Big5 | Big5-HKSCS |
| **Korea** | UTF-8, EUC-KR | ISO-2022-KR |
| **Russia** | UTF-8, Windows-1251 | KOI8-R, ISO-8859-5 |
| **Europe** | UTF-8, ISO-8859-1 | Windows-1252, ISO-8859-15 |
| **Global** | UTF-8, UTF-8-BOM | UTF-16LE, UTF-16BE |

## 🔧 Advanced Features

### Custom Encoding Detection

```r
# Define custom encoding priority
custom_encodings <- c("UTF-8", "Shift_JIS", "EUC-JP")
df <- readflex("file.csv", encodings = custom_encodings)

# Smart content-based detection
df <- readflex_enhanced("file.csv", profile = "auto")
```

### Error Recovery

```r
# Automatic error recovery
df <- readflex_enhanced("problematic.csv", auto_fix = TRUE)

# Manual error recovery
tryCatch({
  df <- readflex("file.csv")
}, error = function(e) {
  # Try diagnostics and auto-fix
  df <- readflex_auto_fix("file.csv")
})
```

### Batch Operations

```r
# Process multiple files
results <- readflex_batch(
  file_pattern = "data/*.csv",
  output_format = "combined",
  progress = TRUE
)

# Different output formats
list_results <- readflex_batch("*.csv", output_format = "list")
combined_df <- readflex_batch("*.csv", output_format = "combined")
```

## 📈 Performance Tips

1. **Enable caching** for repeated file access
2. **Use regional profiles** for consistent file types
3. **Enable parallel processing** for large files
4. **Monitor statistics** to optimize workflows
5. **Use batch processing** for multiple files
6. **Configure appropriate file size limits**

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality  
4. Ensure all tests pass
5. Submit a pull request

## 📄 License

MIT License - see [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- **stringi** package for encoding detection
- **readr** package for CSV parsing inspiration
- **International community** for encoding expertise
- **R Core Team** for base functionality

## 📞 Support

- **Issues**: [GitHub Issues](https://github.com/toshihiro-iguchi/readflex/issues)
- **Documentation**: Use `?readflex` or `?readflex_enhanced`
- **Examples**: See `/tests/` directory for comprehensive examples

---

**readflex v2.0** - Making international data accessible to everyone! 🌍✨