#'  pope_abundance
#'
#' \code{pope_abundance} estimates N
#'
#' @param   age    from VPA_pope
#'
#'
#' @examples
#'
#'
#' @export

pope_N <- function(indice)
{
  Mat_N[indice] <- Mat_N[indice+1]*exp(Mat_M[indice])+Mat_Cage[indice]*exp(Mat_M[indice]/2)
  assign('Mat_N', Mat_N, envir=globalenv())
}
