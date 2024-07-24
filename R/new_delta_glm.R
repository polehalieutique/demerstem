#' Delta coupling of 2 GLMs
#'
#' \code{delta_glm} realize the delta-coupling using the 2 precedents GLMs outputs (filtered with year modality only), extract the year factor, calculate the AI and display the plots.
#' Presence/Absence-GLM modeling process
#'
#' @param   tab_pres        input dataset table used for
#' @param   tab_ia          ia
#' @param   esp             exact name of the studied species
#' @param   title           fraction of the title in the plots
#' @param   list_param      list of the tested parameters
#' @param   var_eff_list    list of the possible fishing effort column
#' @param   espece_id       exact name of the column indicating the species
#' @param   catch_col       exact name of the column indicating the catches
#' @param   limit           percentage representing the limit value under which the modality is removed
#' @param   formula_select_pres  if "auto", the function select which formula as the lowest AIC. Else, run the selected formula.
#' @param   formula_select_ia  if "auto", the function select which formula as the lowest AIC. Else, run the selected formula.
#' @param   repartition
#' @param   data_type
#' @examples
#' data(tableau_sc)
#' glm_pres <- model_pres_abs(tableau_sc, esp="PSEUDOTOLITHUS ELONGATUS", effort="auto", title="SC", list_param=c("annee", "saison", "strate"), var_eff_list=c("surface_chalutee"), espece_id='nom_taxonomique', catch_col='total_capture', interactions = FALSE, limit=0.0001, formula_select = "presence ~ strate + annee + saison")
#' @export

delta_glm <- function(tab_pres, tab_ia, esp, title, list_param, var_eff_list, espece_id, catch_col, limit,
                          formula_select_pres,
                          formula_select_ia, repartition = 0, temporal_factor = 0, data_type) {

  cat("
-----------------------------------------

   --  SOUS-MODELE PRESENCE/ABSENCE --

-----------------------------------------
        ")
  tableau_pres <- table_pres_abs(tab = tab_pres, esp, list_param, var_eff_list, espece_id, catch_col, limit)
  parameters <- list_param

  for (i in 1:length(parameters)){
    tableau_pres[,parameters[i]] <- as.factor(tableau_pres[,parameters[i]])
    tableau_pres[,parameters[i]] <- droplevels(tableau_pres[,parameters[i]])
    contrasts(tableau_pres[,parameters[i]]) <- contr.sum(levels(tableau_pres[,parameters[i]]))
  }

  cat("
-----------------------------------------

   --     SOUS-MODELE ABONDANCE     --

-----------------------------------------
        ")
  tab_ia_pres <- table_pres_abs(tab_ia, esp, list_param,  var_eff_list, espece_id, catch_col, limit)
  tableau_ab <- filter(tab_ia_pres, i_ab>0)

  for (i in 1:length(parameters)){
    tableau_ab[,parameters[i]] <- as.factor(tableau_ab[,parameters[i]])
    tableau_ab[,parameters[i]] <- droplevels(tableau_ab[,parameters[i]])
    contrasts(tableau_ab[,parameters[i]]) <- contr.sum(levels(tableau_ab[,parameters[i]]))
  }

  glm_presabs <- glm_pres_abs(tableau_pres = tableau_pres, parameters, formula_select_pres, summary = F)
  glm_indice_ab <- glm_ai_plus(tableau_ab = tableau_ab, parameters, formula_select_ia, summary = F)

  tableau_ab[,list_param] <- lapply(tableau_ab[,list_param],factor)
  tibble <- tableau_ab[,list_param] %>% sapply(levels)

  Table_Pred <- expand.grid(c(tibble))
  results <- Table_Pred

  # Correction de Laurent au modèle lognormal qui n'est pas réalisée avec une fonction de lien donc pas automatiquement calculée par predict.glm
  Table_Pred$i_ab <- exp(predict.glm(glm_indice_ab[[1]], results , type = "r", se = T)$fit + 0.5*((predict.glm(glm_indice_ab[[1]],results , type = "r", se = T)$se.fit)^2))
  Table_Pred$pres <- predict.glm(glm_presabs[[1]],results , type = "r", se = T)$fit



  # results %>% group_by(annee) %>%  summarise(mean_year = mean(pres)) %>%  ggplot() + geom_bar( aes(x = annee, y = mean_year), stat = 'identity')
  # geom_bar(aes(x=modality, y=corrected_estimates), stat="identity", color = "black", fill = "white")
  #
  # results %>% group_by(annee) %>%  summarise(mean_year = mean(i_ab)) %>%  ggplot() + geom_bar( aes(x = annee, y = mean_year), stat = 'identity')

  Table_Pred$estimation <- Table_Pred$i_ab * Table_Pred$pres
  if (is.data.frame(repartition)==T) {
    Table_Pred <- Table_Pred %>%  full_join(repartition)
    Table_Pred$estimation <- Table_Pred$estimation * Table_Pred$proportion
    Table_Pred$proportion <- NULL
  }
  if (temporal_factor != 0) {
    Table_Pred <- Table_Pred %>% group_by_at(vars(one_of(list_param[!list_param==temporal_factor]))) %>%  summarise(estimation = mean(estimation))
  }
  final_predict <- Table_Pred %>% group_by(annee) %>%  summarise(biomasse = sum(estimation))
  names(final_predict)[names(final_predict) == 'biomasse'] <- data_type
  final_predict <- as.data.frame(final_predict)

  #Plot avec Annee as numeric
  final_predict$annee <- as.numeric(as.character(final_predict$annee))

  EstimateurFinal <- as.name(data_type)
  final_predict$title <- title
  g1 <- ggplot(final_predict) + geom_bar(aes_string(x='annee', y= paste0(data_type)), stat="identity") + labs(x = "Year", y = "Abundance indices") + facet_grid(~title) + theme_nice()
  #g2 <- ggplot(final_predict) + geom_line(size = 1, aes_string(x='annee', y= paste0("EstimateurFinal_",data_type)), stat="identity") + labs(x = "Year", y = "Abundance indices") + scale_y_continuous(limits=c(0, max(final_predict[,2]))) + facet_grid(~title) + theme_nice()#+ ylim(0,max(table_annee_final$EstimateurFinal)))
  g3 <- ggplot(final_predict) + geom_line(aes_string(x = "annee",
                                                     y = paste0(data_type)), stat = "identity") +
    labs(x = "Year", y = "Abundance indices") + scale_y_continuous(limits = c(0,
                                                                              max(final_predict[,2]))) + facet_grid(~title) +
    theme_nice()
  print(g1)
  #print(g2)
  #print(g3)
  # g3 <- ggplot(final_predict) + geom_line(size = 1.1, aes(color = zone, x=annee, y= mean_year), stat="identity") + labs(x = "Year", y = "Abundance indices") + scale_y_continuous(limits=c(0, max(final_predict$mean_year))) + facet_grid(~title) + theme_nice()#+ ylim(0,max(table_annee_final$EstimateurFinal)))
  print(g3)
  list_graph <- list(g1,g3)
  final_predict$title <- NULL
  return (list(final_predict, Table_Pred, list_graph))

}

