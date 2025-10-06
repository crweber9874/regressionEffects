

#' Create a zero one normalization
#'
#' @param numeric value
#'
#' @return A standardized 0-1 variable
#' @export
#'
#' @examples #rnorm(10) %>% zero.one()
  print_metadata <- function(data) {
    cat("Title:", attr(data, "title"), "\n")
    cat("Description:", attr(data, "description"), "\n")
    cat("Source:", attr(data, "source"), "\n")
    cat("Note:", attr(data, "note"), "\n")
  }
