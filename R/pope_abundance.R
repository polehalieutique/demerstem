#'  pope_abundance
#'
#' \code{pope_abundance} estimates N at age
#'
#' @param   age    from VPA_pope
#'
#'
#' @examples
#'
#'
#' @export

pope_abundance <- function(age)
{
  res <- Mat_Cage[age]*(Mat_M[age]+Mat_F[age])/(Mat_F[age]*(1-exp(-Mat_M[age]-Mat_F[age])))
  res
}
