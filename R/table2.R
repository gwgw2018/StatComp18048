
#' @title construct a table
#' @description a fast version of table function
#' @param x vector 1 (numeric)
#' @param y vector 2 (numeric)
#' @return a random sample of size \code{n}
#' @examples
#' \dontrun{
#' a<-c(1,2,3,4)
#' b<-c(2,3,4,5)
#' table(a,b)
#' }
#' @export
table2 <- function(x, y) {
  x_val <- unique(x)
  y_val <- unique(y)
  mat <- matrix(0L, length(x_val), length(y_val))
  for (i in seq_along(x)) {
    mat[which(x_val == x[[i]]), which(y_val == y[[i]])] <-
      mat[which(x_val == x[[i]]),  which(y_val == y[[i]])] + 1L
  }
  dimnames <- list(x_val, y_val)
  names(dimnames) <- as.character(as.list(match.call())[-1])  # R has names for dimnames... :/
  tab <- array(mat, dim = dim(mat), dimnames = dimnames)
  class(tab) <- "table"
  tab
}

