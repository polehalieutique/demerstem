#' nombre de donnees par modalite de chaque parametre

#' @param tab : tableau de données
#' @param facteur_list : facteurs
#' @param seuil : seuil

#' @examples
#' tabz <- moda_few_rm(tableau_ab, names_facteur, 0.05)
#' @export

moda_few_rm <- function(tab, factor_list, seuil){
  h <- lapply (factor_list, FUN=moda_few, tab=tab, seuil=seuil)
  DTs = lapply(h, data.table)
  DT_keep = Reduce(intersect, DTs)
  return(DT_keep)
}

