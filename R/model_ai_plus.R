#' Abundance-GLM modeling process
#'
#' \code{model_ai_plus} runs the actions for the GLM modeling process on abundance data.
#'
#' @param   tab             input dataset table
#' @param   esp             exact name of the studied species
#' @param   effort          "auto" for an automatic selection of the effort parameter or manual selection ex "duree_peche","nombre_operation","nb_jour_peche", "nb_sorties", "surface_chalutee"
#' @param   title           fraction of the title in the plots
#' @param   list_param      list of the tested parameters
#' @param   var_eff_list    list of the possible fishing effort column
#' @param   espece_id       exact name of the column indicating the species
#' @param   catch_col       exact name of the column indicating the catches
#' @param   interactions    "Y" if there is an interaction in our GLM. "N" else
#' @param   limit           percentage representing the limit value under which the modality is removed
#' @param   formula_select  if "auto", the function select which formula as the lowest AIC. Else, run the selected formula.
#'
#'
#' @examples
#'  data(tableau_sc)
#'  model_ai_plus(tableau_sc, esp = "PSEUDOTOLITHUS ELONGATUS", effort = "auto", title = "SC", list_param = c("annee", "saison", "strate"), var_eff_list=c("surface_chalutee"), espece_id='nom_taxonomique', catch_col='total_capture', interactions ="N", limit=0.001, formula_select = "log(i_ab+0.0001) ~ strate + annee + saison")
#'
#' @export

model_ai_plus <- function(tab, esp, effort, title, list_param,  var_eff_list, espece_id, catch_col, interactions, limit , formula_select){
  print("SOUS-MODELE ABONDANCE")
  tableau_pres <- table_pres_abs(tab, effort, esp, list_param,  var_eff_list, espece_id, catch_col, limit)
  tableau_ab <- filter(tableau_pres, presence==1)
  print(param_use(tableau_ab, list_param))
  param <- param_use(tableau_ab, list_param)
  print(lapply(param, moda_facto, tab=as.data.frame(tableau_ab), title))

  print(evo_an(tableau_ab, title))
  print(lapply(param, evo_facto, tab=as.data.frame(tableau_ab), title))

  for (i in 1:length(param)){
    tableau_ab[,param[i]] <- as.factor(tableau_ab[,param[i]])
    tableau_ab[,param[i]] <- droplevels(tableau_ab[,param[i]])
    contrasts(tableau_ab[,param[i]]) <- contr.sum(levels(tableau_ab[,param[i]]))
  }

  glm_indice_ab <- glm_ai_plus(tableau_ab, param, formula_select)

  if (interactions == "Y"){
    vect_param <- c(all.vars(formula(glm_indice_ab))[-1])
    table_interact <- c()
    for (j in 2:(length(vect_param)+2)){
      table_tempo <- as.data.frame(dummy.coef(glm_indice_ab)[j])
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

    VAR <- var(residuals(glm_indice_ab))
    vect_param <- c(all.vars(formula(glm_indice_ab))[-1]) # liste des paramètres
    table_finale <- c()
    table_ab <- as.data.frame(coef(summary(glm_indice_ab)))
    for (i in 2:(length(vect_param)+1)){
      table_tempo <- as.data.frame(dummy.coef(glm_indice_ab)[i])
      table_tempo$modalite <- rownames(table_tempo)
      rownames(table_tempo) <- NULL
      table_tempo$variable <- vect_param[i-1]
      colnames(table_tempo) <- c("Estimate", "modalite", "variable")
      table_tempo$ExpEstimate <- table_tempo$Estimate + table_ab[1,1]
      table_tempo$ExpEstimate <- exp(table_tempo$Estimate + 0.5*VAR)
      print(ggplot(table_tempo) + geom_bar(aes(x=modalite, y=ExpEstimate), stat="identity", color = "black", fill = "white") + ylab("Estimateur") + ggtitle(paste(vect_param[i-1], "pour abondance")) + theme(axis.text.x = element_text(angle = 35)))
      table_finale <- rbind(table_finale, table_tempo)
    }

    return(glm_indice_ab)
  }
}

