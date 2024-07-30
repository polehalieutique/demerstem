#' Mean of AI series
#'
#' \code{mean_ai} gather the different AI series (/!\ MAX 5 SERIES) in one single table, standardise one mean AI series and plot several graphs
#'
#'
#' @param data_IA         data table containing the year in the first column, and the different AI series on the following ones
#' @param MOY             if TRUE, then mean_3years is run
#' @param vect_year_elim  vector containing the years to delete. Else, empty vector c()
#' @param type_ref        Select AI for standardisation.
#' @param type_other      Select others AI
#' @param fish_power      A vector containing technological creep for each AI in type_other. Not applied if equal zero.
#'
#' @examples
#' data(data_IA_JABBA)
#' data_IA_test <- data_IA_JABBA %>%  dplyr:::select(c(annee, ScientificSurvey_SEN, Artisanal_SEN))
#' mean_ai(data_IA=data_IA_test, MOY=T, vect_year_elim=c(), type_ref = "ScientificSurvey_SEN", type_other = "Artisanal_SEN", fish_power = c(0.05), title = "1st Try")
#'
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

  # get year for which value is available for every IA
  data_IA_fp_0$Any_NA <- apply(data_IA_fp_0, 1, function(x) anyNA(x))
  tab_moy <- data_IA_fp_0 %>% filter(Any_NA == FALSE)

  if (is.null(tab_moy)) {
    return("no common years")
  }
  # standardisation using mean on common year
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

  #--------------
  # plot with tech creep
  #--------------
  # 1. without mean
  data_IA_0 <- data_IA
  #data_IA_0[,c(2:ncol(data_IA_0))] <- data_IA_0[,c(2:ncol(data_IA_0))]/dplyr::first(na.omit(data_IA_0[, type_ref]))[1]
  IA_long_2 <- reshape2::melt(data_IA_0, id.vars = "year")
  IA_long_2$variable <- as.factor(IA_long_2$variable)
  IA_long_2$title <- title

  plot_standard_creep <- ggplot(IA_long_2) +
    geom_line(aes(x = year, y = value, color = variable)) +
    geom_point(aes(x = year, y = value, color = variable)) +
    facet_grid(~title) + labs(x = "Year", y = "Abundance indices", color = "Variable") + theme_nice()

  list_graph[[length(list_graph) + 1]] <- plot_standard_creep


  # 1. with mean
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
