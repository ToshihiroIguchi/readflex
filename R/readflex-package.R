#' readflex: Flexible CSV Reader with Automatic Encoding Detection
#'
#' @description
#' The readflex package provides a flexible CSV reader that automatically detects
#' character encodings and handles various file formats. It supports multiple
#' languages and encoding systems including UTF-8, Shift_JIS, GB18030, Big5,
#' EUC-KR, KOI8-R, Windows-1251, ISO-8859-1, and many others commonly used worldwide.
#'
#' @details
#' The main function \code{\link{readflex}} attempts to read CSV files by
#' automatically detecting character encoding using \code{readr::guess_encoding()}
#' or \code{stringi::stri_enc_detect()} and falls back to a user-specified list
#' of common encodings if detection fails.
#'
#' Key features:
#' \itemize{
#'   \item Automatic character encoding detection
#'   \item Support for multiple international character sets
#'   \item Graceful fallback to common encoding lists
#'   \item Enhanced error handling and diagnostics
#'   \item Performance optimization with caching
#'   \item Multi-format support (CSV, TSV, Excel, etc.)
#'   \item Configuration management and regional profiles
#' }
#'
#' @section Main Functions:
#' \describe{
#'   \item{\code{\link{readflex}}}{Main function for reading CSV files with automatic encoding detection}
#'   \item{\code{\link{readflex_diagnostic}}}{Comprehensive file diagnostics and troubleshooting}
#'   \item{\code{\link{readflex_config}}}{Global configuration management}
#'   \item{\code{\link{readflex_profile}}}{Regional encoding profiles}
#' }
#'
#' @section Supported Encodings:
#' The package supports a wide range of character encodings including:
#' \itemize{
#'   \item Unicode: UTF-8, UTF-8-BOM, UTF-16LE, UTF-16BE
#'   \item Japanese: Shift_JIS, CP932, EUC-JP, ISO-2022-JP
#'   \item Chinese: GB18030, GBK, GB2312, Big5, Big5-HKSCS
#'   \item Korean: EUC-KR, ISO-2022-KR
#'   \item Russian/Cyrillic: Windows-1251, KOI8-R, ISO-8859-5
#'   \item European: ISO-8859-1, Windows-1252, latin1
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage with automatic encoding detection
#' df <- readflex("your_data.csv")
#'
#' # With verbose output to see detection process
#' df <- readflex("your_data.csv", verbose = TRUE)
#'
#' # Specify custom encodings to try
#' df <- readflex("your_data.csv", encodings = c("UTF-8", "Shift_JIS", "GB18030"))
#'
#' # Use regional profile for optimized detection
#' readflex_profile("japan", apply_immediately = TRUE)
#' df <- readflex("your_japanese_data.csv")
#'
#' # Run diagnostics on problematic files
#' diag <- readflex_diagnostic("your_problematic_file.csv")
#' print(diag)
#' }
#'
#' @author Toshihiro Iguchi \email{toshihiro.iguchi.github@@gmail.com}
#' @keywords package
#' @seealso
#' Useful links:
#' \itemize{
#'   \item \url{https://github.com/ToshihiroIguchi/readflex}
#'   \item Report bugs at \url{https://github.com/ToshihiroIguchi/readflex/issues}
#' }
#'
"_PACKAGE"