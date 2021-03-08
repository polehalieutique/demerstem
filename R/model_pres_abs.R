#' la fonction delta_presabs récapitule les actions qui concernent le sous-modele presence-absence y compris la selection des variables

#' @param   tab               : input dataset table
#' @param   esp               : exact name of the studied speciess
#' @param   effort            : "auto" for an automatic selection of the effort parameter or manual selection ex "duree_peche","nombre_operation","nb_jour_peche", "nb_sorties", "surface_chalutee"
#' @param   catch_col         : exact name of the column indicating the catches
#' @param   formula_select    : if "auto", the function select which formula as the lowest AIC. Else, run the selected formula.
#' @param   interactions      : "Y" if there is an interaction in our GLM. "N" else
#' @param   limit             : percentage representing the limit value under which the modality is removed
#' @param   title             : fraction of the title in the plots

#' @examples
#'  #PA
#'  delta_presabs(tableau_pa, "BOBO", list_param, "commercial", "auto", titre="PA", list_param,  espece_id, var_eff_list, ope_id, catch_col, interactions = "auto", limit=0.05)
#'  #PI
#'  delta_presabs(tableau_pi, "PSEUDOTOLITHUS ELONGATUS", list_param, "commercial", "auto", titre="PI", list_param,  espece_id, var_eff_list, ope_id, catch_col, interactions = "auto", limit=0.05)
#'  #SC
#'  delta_presabs(tableau_sc, "PSEUDOTOLITHUS ELONGATUS", list_param, "scientifique", "auto", titre="SC", list_param,  espece_id, var_eff_list, ope_id, catch_col, interactions = "auto", limit=0.05)
#' @export
#'

model_pres_abs <- function(tab, esp, effort, title, list_param,  espece_id, var_eff_list, catch_col, interactions, limit, formula_select){
  print("SOUS-MODELE PRESENCE ABSENCE")
  tableau_pres <- table_pres_abs(tab, effort, esp, list_param, espece_id, var_eff_list, catch_col, limit)
  print(param_use(tableau_pres, list_param) )
  param <- param_use(tableau_pres, list_param)
  print(lapply(param, pres_facto, tab=tableau_pres, title))

  #NEW
  for (i in 1:length(param)){
    tableau_pres[,param[i]] <- as.factor(tableau_pres[,param[i]])
    tableau_pres[,param[i]] <- droplevels(tableau_pres[,param[i]])
    contrasts(tableau_pres[,param[i]]) <- contr.sum(levels(tableau_pres[,param[i]]))
  }
  #NEW

  glm_presabs <- glm_pres_abs(tableau_pres, param, formula_select)

  #NEW4

  if (interactions == "Y"){
    vect_param <- c(all.vars(formula(glm_presabs))[-1])
    table_interact <- c()
    j <- 3
    for (j in 2:(length(vect_param)+2)){
      table_tempo <- as.data.frame(dummy.coef(glm_presabs)[j])
      table_tempo$namemodality <- rownames(table_tempo)
      object <- gregexpr(pattern =':',as.character(table_tempo[1,2]))
      if (length(object)>0){
        if(as.numeric(object)>0){
          table_interact <- table_tempo
        }
      }
    }
    return(table_interact)

  } else {
    vect_param <- c(all.vars(formula(glm_presabs))[-1]) # liste des paramètres
    table_finale <- c()
    table_pres <- as.data.frame(coef(summary(glm_presabs)))
    for (i in 2:(length(vect_param)+1)){
      table_tempo <- as.data.frame(dummy.coef(glm_presabs)[i])
      table_tempo$modalite <- rownames(table_tempo)
      rownames(table_tempo) <- NULL
      table_tempo$variable <- vect_param[i-1]
      colnames(table_tempo) <- c("Estimate", "modalite", "variable")
      table_tempo$ExpEstimate <- table_tempo$Estimate + table_pres[1,1]
      table_tempo$ExpEstimate <- exp(table_tempo$ExpEstimate)/(1+exp(table_tempo$ExpEstimate))
      print(ggplot(table_tempo) + geom_bar(aes(x=modalite, y=ExpEstimate), stat="identity", color = "black", fill = "white") + ylab("Estimateur") + ggtitle(paste(vect_param[i-1], "pour pres/abs")) + theme(axis.text.x = element_text(angle = 35)))
      table_finale <- rbind(table_finale, table_tempo)
    }

    #NEW4

    return(glm_presabs)
  }

}
