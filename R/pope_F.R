#'  pope_F
#'
#' \code{pope_F} estimates F by pope method
#'
#' @param   indice    from VPA_pope
#'
#'
#' @examples
#'
#' @export


pope_F <- function(indice)
{
  Mat_F[indice] <- log(Mat_N[indice]/Mat_N[indice+1])-Mat_M[indice]
  assign('Mat_F', Mat_F, envir=globalenv())
}

