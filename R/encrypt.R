#' Load and Decrypt Encrypted Dataset
#'
#' This function loads and decrypts one of several encrypted datasets
#' using a provided password. The datasets are stored in binary encrypted form
#' and are decrypted using the `sodium` package.
#'
#' @param data Character string. One of `"arizonaVoter"`, `"electoral_contestation_wide"`,
#' or `"electoral_contestation_long"`.
#' @param password Character string. Password used to derive the decryption key.
#'
#' @return A data object corresponding to the selected dataset.
#' @importFrom sodium hash data_decrypt
#' @examples
#' \dontrun{
#' my_data <- load_data("arizonaVoter", password = "")
#' }
load_data <- function(data = c("arizonaVoter",
                               "electoral_contestation_wide",
                               "electoral_contestation_long"),
                      password = "none") {
  if (missing(password)) {
    stop("You must supply a password.")
  }

  if (!requireNamespace("sodium", quietly = TRUE)) {
    stop("The 'sodium' package is required but not installed. Please install it using install.packages('sodium').")
  }

  # Match and validate dataset choice
  dataset <- match.arg(data)

  # Map dataset name to file path
  file_map <- list(
    arizonaVoter = "inst/data/azvoter.enc",
    electoral_contestation_wide = "inst/data/ecwide.enc",
    electoral_contestation_long = "inst/data/eclong.enc"
  )

  file_path <- file_map[[dataset]]

  if (!file.exists(file_path)) {
    stop("Encrypted file not found: ", file_path)
  }

  # Read encrypted file
  encrypted_data <- readBin(file_path, what = "raw", n = 1e6)

  # Derive key from password
  key_raw <- sodium::hash(charToRaw(password))

  # Decrypt
  raw_data <- sodium::data_decrypt(encrypted_data, key)

  # Unserialize to original R object
  result <- unserialize(raw_data)

  return(result)
}
