#' Presence/Absence-GLM modeling process
#'
#' \code{model_pres_abs} runs the actions for the GLM modeling process on presence/absence data.
#'
#' @param   tab               : input dataset table
#' @param   esp               : exact name of the studied speciess
#' @param   effort            : "auto" for an automatic selection of the effort parameter or manual selection ex "duree_peche","nombre_operation","nb_jour_peche", "nb_sorties", "surface_chalutee"
#' @param   title             : fraction of the title in the plots
#' @param   list_param        : list of the tested parameters
#' @param   var_eff_list      : list of the possible fishing effort column
#' @param   espece_id         : exact name of the column indicating the species
#' @param   catch_col         : exact name of the column indicating the catches
#' @param   interactions      : "Y" if there is an interaction in our GLM. "N" else
#' @param   limit             : percentage representing the limit value under which the modality is removed
#' @param   formula_select    : if "auto", the function select which formula as the lowest AIC. Else, run the selected formula.

#' @examples
#' data(tableau_sc)
#' glm_pres <- model_pres_abs(tableau_sc, esp="PSEUDOTOLITHUS ELONGATUS", effort="auto", title="SC", list_param=c("annee", "saison", "strate"),  espece_id='nom_taxonomique', var_eff_list=c("surface_chalutee"), catch_col='total_capture', interactions = "N", limit=0.0001, formula_select = "presence ~ strate + annee + saison")
#' @export

model_pres_abs <- function(tab, esp, effort, title, list_param,  var_eff_list, espece_id, catch_col, interactions, limit, formula_select){
  print("SOUS-MODELE PRESENCE ABSENCE")
  tableau_pres <- table_pres_abs(tab, effort, esp, list_param, var_eff_list, espece_id, catch_col, limit)
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
