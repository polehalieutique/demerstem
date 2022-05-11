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

  data_int <- data_IA %>% dplyr::select(year, type_ref)
  for (k in 1:length(type_other)) {
    fish_power_0 <- fish_power * 0
    data_IA_0 <- data_IA
    Annee.indice <- as.numeric(data_IA_0$year) - min(data_IA_0$year[which(data_IA_0[,
                                                                                    2 + k] > 0)]) + 1
    data_IA_0[, 2 + k] <- data_IA_0[, 2 + k] * 1/(1 + fish_power_0[k])^(Annee.indice -
                                                                          1)
    data_IA_0_filter <- reshape2::melt(data_IA_0, id.vars = "year")
    data_IA_0_filter <- data_IA_0_filter %>% filter(variable %in%
                                                      c(type_ref, type_other[k]))
    data_IA_0_filter <- data_IA_0_filter %>% pivot_wider(names_from = variable,
                                                         values_from = value)
    data_IA_0_filter$Any_NA <- apply(data_IA_0_filter, 1, function(x) anyNA(x))
    tab_moy <- data_IA_0_filter %>% filter(Any_NA == FALSE)
    if (is.null(tab_moy)) {
      print("no common years")
    }
    mean_tempo <- tab_moy %>% pivot_longer(cols = c(type_ref,
                                                    type_other[k])) %>% group_by(name) %>% dplyr::summarise(mean = mean(value))
    data_IA_0_transit <- as.data.frame(data_IA_0 %>% pivot_longer(type_other[k]) %>%
                                         dplyr::select(year, value))
    mean_survey <- (as.numeric(mean_tempo %>% filter(name ==
                                                       type_ref) %>% dplyr::select(mean)))
    mean_com <- as.numeric(mean_tempo %>% filter(name ==
                                                   type_other[k]) %>% dplyr::select(mean))
    data_IA_0_transit$value <- data_IA_0_transit$value * as.numeric(mean_survey)/as.numeric(mean_com)
    data_int_0 <- full_join(data_int_0, data_IA_0_transit, by = "year")
    names(data_int_0)[names(data_int) == "value"] <- type_other[k]
  }

  data_IA_0 <- data_int_0
  data_IA_0[,c(2:ncol(data_IA_0))] <- data_IA_0[,c(2:ncol(data_IA_0))]/data_IA_0$SC[1]
  IA_long_2 <- reshape2::melt(data_IA_0, id.vars = "year")
  IA_long_2$variable <- as.factor(IA_long_2$variable)
  IA_long_2$title <- title

  plot_standard <- ggplot(IA_long_2) +
    geom_line(aes(x = year, y = value, color = variable)) +
    geom_point(aes(x = year, y = value, color = variable)) +
    facet_grid(~title) + labs(x = "Year", y = "Abundance indices", color = "Variable") + theme_nice()

  list_graph[[length(list_graph) + 1]] <- list(plot_standard)

  for (k in 1:length(type_other)) {
    Annee.indice <- as.numeric(data_IA$year) - min(data_IA$year[which(data_IA[,
                                                                              2 + k] > 0)]) + 1
    data_IA[, 2 + k] <- data_IA[, 2 + k] * 1/(1 + fish_power[k])^(Annee.indice -
                                                                    1)
    data_IA_filter <- reshape2::melt(data_IA, id.vars = "year")
    data_IA_filter <- data_IA_filter %>% filter(variable %in%
                                                  c(type_ref, type_other[k]))
    data_IA_filter <- data_IA_filter %>% pivot_wider(names_from = variable,
                                                     values_from = value)
    data_IA_filter$Any_NA <- apply(data_IA_filter, 1, function(x) anyNA(x))
    tab_moy <- data_IA_filter %>% filter(Any_NA == FALSE)
    if (is.null(tab_moy)) {
      print("no common years")
    }
    mean_tempo <- tab_moy %>% pivot_longer(cols = c(type_ref,
                                                    type_other[k])) %>% group_by(name) %>% dplyr::summarise(mean = mean(value))
    data_IA_transit <- as.data.frame(data_IA %>% pivot_longer(type_other[k]) %>%
                                       dplyr::select(year, value))
    mean_survey <- (as.numeric(mean_tempo %>% filter(name ==
                                                       type_ref) %>% dplyr::select(mean)))
    mean_com <- as.numeric(mean_tempo %>% filter(name ==
                                                   type_other[k]) %>% dplyr::select(mean))
    data_IA_transit$value <- data_IA_transit$value * as.numeric(mean_survey)/as.numeric(mean_com)
    data_int <- full_join(data_int, data_IA_transit, by = "year")
    names(data_int)[names(data_int) == "value"] <- type_other[k]
  }
  data_IA <- data_int
  data_IA$AI_standard <- apply(as.data.frame(data_IA[, -1]),
                               1, function(x) mean(x, na.rm = T))
  data_IA[,c(2:ncol(data_IA))] <- data_IA[,c(2:ncol(data_IA))]/data_IA$SC[1]
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
  list_graph[[length(list_graph) + 1]] <- list(v)
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
    list_graph[[length(list_graph) + 1]] <- list(s)
  }
  return(list(data_IA, list_graph))
}
