% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readflex_enhanced.R
\name{readflex_enhanced}
\alias{readflex_enhanced}
\title{Enhanced flexible CSV reader with comprehensive features}
\usage{
readflex_enhanced(
  file,
  ...,
  encodings = NULL,
  guess_n_max = NULL,
  verbose = NULL,
  stringsAsFactors = NULL,
  max_file_size_mb = NULL,
  use_cache = NULL,
  use_parallel = NULL,
  auto_fix = FALSE,
  validate_data = FALSE,
  profile = NULL
)
}
\arguments{
\item{file}{Character. Path to the CSV file to read.}

\item{...}{Additional arguments passed to `utils::read.csv()`.}

\item{encodings}{Character vector. Encodings to try if auto-detection fails.
If NULL, uses global configuration.}

\item{guess_n_max}{Integer. Number of lines to sample when guessing encoding.}

\item{verbose}{Logical. If TRUE, prints detailed processing information.}

\item{stringsAsFactors}{Logical. Should character vectors be converted to factors?}

\item{max_file_size_mb}{Numeric. Maximum file size in MB.}

\item{use_cache}{Logical. Whether to use encoding cache.}

\item{use_parallel}{Logical. Whether to use parallel processing.}

\item{auto_fix}{Logical. Whether to attempt automatic error fixes.}

\item{validate_data}{Logical. Whether to perform data validation.}

\item{profile}{Character. Regional encoding profile to use ("auto", "japan", etc.).}
}
\value{
A `data.frame` with additional attributes for encoding and processing info.
}
\description{
`readflex_enhanced()` is an advanced version of readflex that includes
performance optimizations, caching, diagnostics, and enhanced error handling.
}
\examples{
\dontrun{
# Basic usage with auto-detection
df <- readflex_enhanced("your_data.csv")

# With regional profile
df <- readflex_enhanced("your_japanese_data.csv", profile = "japan")

# With verbose output and validation
df <- readflex_enhanced("your_data.csv", verbose = TRUE, validate_data = TRUE)

# With auto-fix for problematic files
df <- readflex_enhanced("your_problematic_file.csv", auto_fix = TRUE)
}

}
