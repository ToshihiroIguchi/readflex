# readflex

**Flexible CSV Reader with Automatic Encoding Detection for R**

`readflex` provides a convenient way to load CSV files with automatic encoding detection, helping you avoid character corruption when working with files in various encodings (e.g., UTF-8, Shift_JIS, EUC-JP, GBK, Big5, etc.). This package tries multiple encoding strategies and gracefully falls back to a list of common ones if needed.

---

## Installation

Install the development version from GitHub:

```r
# If you don't have devtools, install it first
install.packages("devtools")

# Install readflex from GitHub
devtools::install_github("ToshihiroIguchi/readflex")
```

## Example

``` r
library(readflex)

# Automatically detect encoding and read the CSV
df <- readflex("data.csv")

# Specify additional options such as separator
df2 <- readflex("data.csv", sep = ";", stringsAsFactors = FALSE)

# Enable verbose logging
df3 <- readflex("data.csv", verbose = TRUE)

```

