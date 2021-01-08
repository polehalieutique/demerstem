#'  nombre de donnees par modalite de chaque parametre
#' @param tab tableau de données
#' @param list facteurs

#' @examples
#'
#' @export
#'

param_use <- function (tab, list){
  param.use <- names(tab)[(names(tab) %in% list)]
  print(paste(length(param.use),"variables a tester"))
  return(param.use)

}
