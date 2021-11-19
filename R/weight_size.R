#'  weight_size
#'
#' \code{weight_size} to plot size-weight relationship
#'
#' @param   t          time vector
#' @param   p          vector with values
#'
#' @examples
#'  t <- 1
#'  p <- c(0.005,3.16)
#' @export

weight_size <- function(t,p)
{
  res <- p[1]*t^p[2]
}
