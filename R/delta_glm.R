#' Delta coupling of 2 GLMs
#'
#' \code{delta_glm} realize the delta-coupling using the 2 precedents GLMs, extract the year factor, calculate the AI and display the plots.
#'
#'
#' @param   tab_pres        input dataset table used for pres/abs model
#' @param   tab_ia          input dataset table used for ia model
#' @param   esp             exact name of the studied species
#' @param   title           title will be displayed in the plots
#' @param   list_param      list of the tested parameters
#' @param   var_eff_list    list of the possible fishing effort column
#' @param   espece_id       exact name of the column indicating the species
#' @param   catch_col       exact name of the column indicating the catches
#' @param   limit           percentage representing the limit value under which the modality is removed
#' @param   formula_select_pres  specify the final prese/abs model
#' @param   formula_select_ia  specify the final abundance model
#' @param   repartition      Surface by region strata (can be absolute value)
#' @param   temporal_facto   If other than annual and on a smaller time step (seasonal, quarter...). That way biomass mean will be calculated in this time step for each year
#' @param   data_type        Specify the type of data for the combining of different data_type
#' @examples
#'
#' data(tableau_sc)
#' tableau_sc_GIN <- tableau_sc %>% mutate(bathymetrie = case_when(
#'   profond_deb >= 05  & profond_deb <=10 ~ "5-10m",
#'   profond_deb > 10 & profond_deb <=15 ~ "10-15m",
#'  profond_deb > 15 & profond_deb <= 30 ~ "15-30m"
#' ))
#'
#'
#' repartition <- as.data.frame(cbind(bathymetrie = c("5-10m", "10-15m", "15-30m"),
#'                                   proportion = c(661.5, 1009.3, 2872.3)))
#'
#' repartition$proportion <- as.numeric(repartition$proportion)
#'
#' delta_IA <- delta_glm(tab_pres = tableau_sc_GIN, tab_ia = tableau_sc_GIN, esp="PSEUDOTOLITHUS ELONGATUS", title="1st Try",
#'                       list_param=c("annee", "bathymetrie"), var_eff_list=c("surface_chalutee"), espece_id='nom_taxonomique',
#'                       catch_col='total_capture', limit=0.0001,
#'                       formula_select_pres = "presence ~ annee + bathymetrie",
#'                       formula_select_ia = "log(i_ab) ~ annee + bathymetrie",
#'                       repartition = repartition,
#'                       data_type = "SC")
#'
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

  glm_presabs <- glm_pres_abs(tableau_pres = tableau_pres, parameters, formula_select_pres, summary = F, type = 3)
  glm_indice_ab <- glm_ai_plus(tableau_ab = tableau_ab, parameters, formula_select_ia, summary = F, type = 3)

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

