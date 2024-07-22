#' this function will select the modality that will be removed for each tested factor
#'
#'
#' @param tab : dataset table
#' @param factor_list : list of the factor tested in the GLMs
#' @param limit : percentage representing the limit value under which the modality is removed
#' @param ope_id2 : vector used in the join

#' @examples
#' tabz <- moda_selection(tableau_ab, names_facteur, 0.05, ope_id2)
#' @export

moda_selection <- function(tab, factor_list, limit, ope_id2){
  h <- lapply(factor_list, FUN=moda_removal, tab=tab, limit=limit, ope_id2=ope_id2)
  #DTs = lapply(h, data.table)
  DT_keep = Reduce(dplyr::intersect, h)
  return(DT_keep)
}
