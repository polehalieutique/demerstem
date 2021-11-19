#'  F_abundance
#'
#' \code{F_abundance} returns abundance of a class age
#'
#'
#' @param   indice          class age
#'

#' @examples
#'  data(tableau_sc)
#'  model_ai_plus(tableau_sc, esp = "PSEUDOTOLITHUS ELONGATUS", effort = "auto", title = "SC", list_param = c("annee", "saison", "strate"), var_eff_list=c("surface_chalutee"), espece_id='nom_taxonomique', catch_col='total_capture', interactions =FALSE, limit=0.001, formula_select = "log(i_ab+0.0001) ~ strate + annee + saison")
#'
#' @export

F_abundance <- function(indice){
  res <- Mat_N2[1]*(Mat_R[indice+1]/Mat_R[1]) #calcul du R qui conduit à l'effectif age 1
  i <- 1
  for (j in indice :1){
    if (nbflot<2){
      res<-res*exp(-(Mat_q[i,]*Mat_E[j+1,])-Mat_M[j]) # N pour un seul métier
    }else{
      res<-res*exp(-apply((Mat_q[i,]*Mat_E[j+1,]),1,sum)-Mat_M[j])  #  N pour pluri-métier
    }
    i<-i+1
  }
  res
}
