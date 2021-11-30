#' Binary search
#'
#' Use the binary search algorithm to efficiently find the index of elements in a vector.
#'
#' @param vec `[numeric]` A sorted vector of numbers.
#' @param x `[numeric(1)]` The search value.
#'
#' @return The index of `x` in `vec`. If `x` isn't in `vec`, returns 0.
#' @export
#'
#' @examples
#' x <- 1:10
#' binary_search(5)
binary_search <- function(vec, x) {
  low <- 1
  high <- length(vec)

  while (low <= high) {
    mid <- floor((low + high) / 2)
    mid_val <- vec[mid]

    if (mid_val < x) {
      low <- mid + 1
    } else if (mid_val > x) {
      high <- mid - 1
    } else {
      return(mid)
    }
  }

  0
}
