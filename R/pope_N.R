#'  pope_N
#'
#' \code{pope_N} estimates N
#'
#' @param   indice    from F_pseudo_rectif
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
