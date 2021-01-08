#'  nombre de donnees par modalite de chaque parametre
#' @param tab tableau de données
#' @param facteur facteurs
#' @param seuil seuil

#' @examples
#' tabz <- moda_few_rm(tableau_ab, names_facteur, 0.05)
#' @export
#'
moda_few <- function(tab, factor, seuil){
  tab.new <- tab[as.factor(as.numeric(as.factor(tab[,factor]))) %in% which(table(tab[,factor]) >= seuil*nrow(tab)),]
  tab.supr <- anti_join(tab, tab.new)
  if(nrow(tab.supr)>0){
    print(paste("modalite(s)", unique(tab.supr[,factor]), "du facteur", factor, "supprimee(s) car represente - de", seuil*100,"% des donnees")) }

  if(length(unique(tab.new[,factor]))<2){
    tab.new <- tab.new %>% dplyr::select(-factor)
    print(paste("le facteur", factor, " a été supprimé car il contenait - de 0 ou 1 modalites"))
  }
  return(tab.new)
}
