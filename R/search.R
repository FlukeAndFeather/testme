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
