#' This function removes the modality under-represented in the dataset, depending on the value of the threshold value
#' @param tab : dataset table
#' @param factor : the factor from which we test each modality
#' @param limit : percentage representing the limit value under which the modality is removed
#' @param ope_id2 : vector used in the join

#' @examples
#' tabz <- moda_removal(tableau_ab, names_factor, 0.05, ope_id2)
#' @export
#'

moda_removal <- function(tab, factor, limit, ope_id2){
  tab.new <- tab[as.factor(as.numeric(as.factor(tab[,factor]))) %in% which(table(tab[,factor]) >= limit*nrow(tab)),]
  tab.supr <- anti_join(tab, tab.new, by = ope_id2)
  if(nrow(tab.supr)>0){
    print(paste("the", unique(tab.supr[,factor]), "modality of factor", factor, "has bean deleted as it represents less than", limit*100,"% of the dataset"))
  }

  if(length(unique(tab.new[,factor]))<2){
    tab.new <- tab.new %>% dplyr::select(-factor)
    print(paste("the", factor, "factor as been deleted as it included less than 1 modality"))
  }
  return(tab.new)
}
