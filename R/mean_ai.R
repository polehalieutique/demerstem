#' Mean of AI series
#'
#' \code{mean_ai} gather the different AI series (/!\ MAX 5 SERIES) in one single table, standardise one mean AI series and plot several graphs
#'
#'
#' @param data_IA         data table containing the year in the first column, and the different AI series on the following ones
#' @param MOY             if TRUE, then mean_3years is run
#' @param vect_year_elim  vector containing the years to delete. Else, empty vector c()
#'
#' @examples
#' data(tableau_sc)
#' data(tableau_pa)
#' data(tableau_pi)
#'
#' glm_abundance_SC <- model_ai_plus(tableau_sc, esp="PSEUDOTOLITHUS ELONGATUS", effort="auto", title="SC", list_param=c("annee", "saison", "strate"), espece_id='nom_taxonomique', var_eff_list=c("surface_chalutee"), catch_col='total_capture', interactions="N", limit=0.001, formula_select = "log(i_ab+0.0001) ~ strate + annee + saison")
#' glm_pres_abs_SC <- model_pres_abs(tableau_sc, esp="PSEUDOTOLITHUS ELONGATUS", effort="auto", title="SC", list_param=c("annee", "saison", "strate"), espece_id='nom_taxonomique', var_eff_list=c("surface_chalutee"), catch_col='total_capture', interactions="N", limit=0.0001, formula_select = "presence ~ strate + annee + saison")
#' AI_SC <- delta_glm(glm_pres_abs_SC, glm_abundance_SC, title="Scientific campaign GIN BOBO", type = "SC")
#'
#' glm_abundance_PA <- model_ai_plus(tableau_pa, esp="BOBO", effort="auto", title="PA", list_param=c("annee", "zone","engin_peche2"), espece_id='espece', var_eff_list=c("nb_jour_peche"), catch_col='captures', interactions="N", limit=0.001, formula_select = "log(i_ab + 1e-04) ~ as.factor(annee) + as.factor(zone) + as.factor(engin_peche2)")
#' glm_pres_abs_PA <- model_pres_abs(tableau_pa, esp="BOBO", effort="auto", title="PA", list_param=c("annee", "zone","engin_peche2"), espece_id='espece', var_eff_list=c("nb_jour_peche"), catch_col='captures', interactions="N", limit=0.0001, formula_select = "presence ~ as.factor(annee) + as.factor(zone) + as.factor(engin_peche2)")
#' AI_PA <- delta_glm(glm_pres_abs_PA, glm_abundance_PA, title="Artisanal Fishery GIN BOBO", type = "PA")
#'
#' glm_abundance_PI <- model_ai_plus(tableau_pi, esp="PSEUDOTOLITHUS ELONGATUS", effort="auto", title="PI", list_param=c("annee", "licence"), espece_id='espece', var_eff_list=c("nombre_operation", "duree_peche"), catch_col='capture_retenue', interactions="N", limit=0.001, formula_select = "log(i_ab + 1e-04) ~ as.factor(annee) + as.factor(licence)")
#' glm_pres_abs_PI <- model_pres_abs(tableau_pi, esp="PSEUDOTOLITHUS ELONGATUS", effort="auto", title="PI", list_param=c("annee", "licence"), espece_id='espece', var_eff_list=c("nombre_operation", "duree_peche"), catch_col='capture_retenue', interactions="N", limit=0.0001, formula_select = "presence ~ as.factor(annee) + as.factor(licence)")
#' AI_PI <- delta_glm(glm_pres_abs_PI, glm_abundance_PI, title="Industrial Fishery GIN BOBO", type = "PI")
#'
#' mean_ai(nb_table=3, MOY="Y", vect_year_elim=c(), AI_SC, AI_PA, AI_PI)
#' @export


mean_ai <- function (data_IA, MOY = TRUE, vect_year_elim, type_ref, type_other,
                     fish_power, title)
{
  list_graph <- NULL
  names(data_IA)[1] <- "year"
  data_IA <- data_IA %>% dplyr::select(year, type_ref, all_of(type_other))
  data_IA <- arrange(data_IA, by_group = year)
  if (length(fish_power) != length(type_other)) {
    fish_power <- rep(fish_power, length(type_other))
  }
  if (length(vect_year_elim) > 0) {
    print(paste0("year(s) ", vect_year_elim, " have been removed"))
    for (i in 1:length(vect_year_elim)) {
      data_IA <- subset(data_IA, !(year == as.numeric(vect_year_elim[i])))
    }
  }

  data_int_0 <- data_IA %>% dplyr::select(year, type_ref)
  data_int <- data_IA %>% dplyr::select(year, type_ref)

  #-----------------------------------------------
  # plot standardized AI without technology creep
  #-----------------------------------------------

  data_IA_fp_0 <- data_IA
  ## on applique la dérive des puissances de pêche à chaque IA identifié comme pêche
  for (k in 1:length(type_other)) {
    Annee.indice <- as.numeric(data_IA_fp_0$year) - min(data_IA_fp_0$year[which(data_IA_fp_0[,2 + k] > 0)]) + 1

    data_IA_fp_0[, 2 + k] <- data_IA_fp_0[, 2 + k] * 1/(1 + 0)^(Annee.indice -1)
  }

  # get year for which value is available for every IA
  data_IA_fp_0$Any_NA <- apply(data_IA_fp_0, 1, function(x) anyNA(x))
  tab_moy <- data_IA_fp_0 %>% filter(Any_NA == FALSE)

  if (is.null(tab_moy)) {
    return("no common years")
  }

  else{
    data_IA_fp_0_st  <- data_IA_fp_0 %>%  dplyr::select(-Any_NA)
    #data_IA_fp_st_transit <- data_IA_fp_0_st
    mean_tempo <- tab_moy %>%
      pivot_longer(cols = c(type_ref,type_other)) %>%
      group_by(name) %>%
      dplyr::summarise(mean = mean(value))

    for (k in 1:length(type_other)) {

      mean_survey <- (as.numeric(mean_tempo
                                 %>% filter(name == type_ref)
                                 %>% dplyr::select(mean)))

      mean_com <- as.numeric(mean_tempo
                             %>% filter(name == type_other[k])
                             %>% dplyr::select(mean))

      data_IA_fp_st_transit <- as.data.frame(data_IA_fp_0_st %>%
                                               pivot_longer(type_other[k]) %>%
                                               dplyr::select(year, value))

      data_IA_fp_st_transit$value <- data_IA_fp_st_transit$value * as.numeric(mean_survey)/as.numeric(mean_com) # on standardise 1 à 1...
      data_int_0 <- full_join(data_int_0, data_IA_fp_st_transit, by = "year")
      names(data_int_0)[names(data_int_0) == "value"] <- type_other[k]

    }
  }

  data_IA_0 <- data_int_0
  #data_IA_0[,c(2:ncol(data_IA_0))] <- data_IA_0[,c(2:ncol(data_IA_0))]/dplyr::first(na.omit(data_IA_0[, type_ref]))[1]
  IA_long_2 <- reshape2::melt(data_IA_0, id.vars = "year")
  IA_long_2$variable <- as.factor(IA_long_2$variable)
  IA_long_2$title <- title

  plot_standard <- ggplot(IA_long_2) +
    geom_line(aes(x = year, y = value, color = variable)) +
    geom_point(aes(x = year, y = value, color = variable)) +
    facet_grid(~title) + labs(x = "Year", y = "Abundance indices", color = "Variable") + theme_nice()

  list_graph[[length(list_graph) + 1]] <- plot_standard


  #-----------------------------------------------
  # plot standardized AI with technology creep
  #-----------------------------------------------


  data_IA_fp <- data_IA
  ## on applique la dérive des puissances de pêche à chaque IA identifié comme pêche
  for (k in 1:length(type_other)) {
    Annee.indice <- as.numeric(data_IA_fp$year) - min(data_IA_fp$year[which(data_IA_fp[,2 + k] > 0)]) + 1

    data_IA_fp[, 2 + k] <- data_IA_fp[, 2 + k] * 1/(1 + fish_power[k])^(Annee.indice -1)
  }

  # get year for which value is available for every IA
  data_IA_fp$Any_NA <- apply(data_IA_fp, 1, function(x) anyNA(x))
  tab_moy <- data_IA_fp %>% filter(Any_NA == FALSE)

  if (is.null(tab_moy)) {
    return("no common years")
  }

  else{
    data_IA_fp_st  <- data_IA_fp %>%  dplyr::select(-Any_NA)
    #data_IA_fp_st_transit <- data_IA_fp_st
    mean_tempo <- tab_moy %>%
      pivot_longer(cols = c(type_ref,type_other)) %>%
      group_by(name) %>%
      dplyr::summarise(mean = mean(value))

    for (k in 1:length(type_other)) {

      mean_survey <- (as.numeric(mean_tempo
                                 %>% filter(name == type_ref)
                                 %>% dplyr::select(mean)))

      mean_com <- as.numeric(mean_tempo
                             %>% filter(name == type_other[k])
                             %>% dplyr::select(mean))

      data_IA_fp_st_transit <- as.data.frame(data_IA_fp_st %>%
                                               pivot_longer(type_other[k]) %>%
                                               dplyr::select(year, value))

      data_IA_fp_st_transit$value <- data_IA_fp_st_transit$value * as.numeric(mean_survey)/as.numeric(mean_com) # on standardise 1 à 1...
      data_int <- full_join(data_int, data_IA_fp_st_transit, by = "year")
      names(data_int)[names(data_int) == "value"] <- type_other[k]

    }
  }

  data_IA <- data_int
  data_IA$AI_standard <- apply(as.data.frame(data_IA[, -1]),
                               1, function(x) mean(x, na.rm = T))
  #data_IA[,c(2:ncol(data_IA))] <- data_IA[,c(2:ncol(data_IA))]/dplyr::first(na.omit(data_IA[, type_ref]))[1]
  IA_long_2 <- reshape2::melt(data_IA, id.vars = "year")
  IA_long_2$variable <- as.factor(IA_long_2$variable)
  IA_long_2$title <- title
  nb_col <- length(unique(as.factor(IA_long_2$variable)))
  palette <- brewer.pal(nb_col, "Set1")
  palette[nb_col] <- "#000000"
  v <- ggplot(IA_long_2) + geom_line(aes(x = year, y = value,
                                         color = variable)) + geom_point(aes(x = year, y = value,
                                                                             color = variable)) + scale_color_manual(values = palette) +
    facet_grid(~title) + labs(x = "Year", y = "Abundance indices",
                              color = "Variable") + theme_nice()
  list_graph[[length(list_graph) + 1]] <- v
  if (MOY == TRUE) {
    data_IA <- mean_3years(data_IA)
    IA_long_3 <- reshape2::melt(data_IA, id.vars = "year")
    IA_long_3$title <- title
    nb_col <- length(unique(as.factor(IA_long_3$variable)))
    palette <- brewer.pal(nb_col, "Set1")
    palette[nb_col] <- "#000000"
    s <- ggplot(IA_long_3) + geom_line(aes(x = year, y = value,
                                           color = variable)) + geom_point(aes(x = year, y = value,
                                                                               color = variable)) + scale_color_manual(values = palette) +
      facet_wrap(~title) + labs(x = "Year", y = "Abundance indices",
                                color = "Variable") + theme_nice()
    list_graph[[length(list_graph) + 1]] <- s
  }
  return(list(data_IA, list_graph))
}
