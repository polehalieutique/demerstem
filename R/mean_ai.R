#' Mean of AI series
#'
#' \code{mean_ai} gather the different AI series (/!\ MAX 5 SERIES) in one single table, standardise one mean AI series and plot several graphs
#'
#'
#' @param nb_table        number of tables included in "..."
#' @param MOY             if TRUE, then mean_3years is run
#' @param vect_year_elim  vector containing the years to delete. Else, empty vector c()
#' @param ...             the different AI tables (max 5)
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


mean_ai<-function(nb_table, MOY=TRUE, vect_year_elim, ...){
  list_IA <- list(...)
  list_year <- c()
  for (i in 1:nb_table){
    tempo_table <- as.data.frame(list_IA[i])
    list_year <- c(list_year, tempo_table[,1])
  }

  min_year <- min(unlist(list_year))
  #print(min_year)
  max_year <- max(unlist(list_year))
  #print(max_year)
  vect_year <- c(min_year:max_year)

  data_IA <- as.data.frame(vect_year)
  colnames(data_IA) <- c('Year')
  for (i in 1:length(list_IA)){
    data_tempo <- as.data.frame(list_IA[i]) %>% dplyr::select(annee, EstimateurFinal, type)
    type_tempo <- gsub(" ", "", paste("IA_",data_tempo$type[1]))
    data_tempo <- data_tempo %>% dplyr::select(-type)
    colnames(data_tempo) <- c("Year", type_tempo)
    data_IA <- left_join(data_IA, data_tempo, by='Year')
  }

  if (length(vect_year_elim)>0){
    print("on élimine les années du vect_year_elim")
    print("the years from vect_year_elim has been deleted")
    for (i in 1:length(vect_year_elim)){
      data_IA <- subset(data_IA, !(Year == as.numeric(vect_year_elim[i])))
    }
  }

  IA_long<-reshape2::melt(data_IA,id.vars="Year")
  t <- ggplot(IA_long) + geom_line(aes(x=Year, y=value, color=variable)) + geom_point(aes(x=Year, y=value, color=variable))
  t <- t + ylab("Indice d'Abondance")

  print(t)

  #calcul standardisation sur années communes
  data_IA$Any_NA <- apply(data_IA[, grep("IA", names(data_IA))], 1, function(x) anyNA(x))

  tab_moy <- data_IA %>% filter(Any_NA == FALSE)
  #tab_moy2 <- tab_moy %>% mutate(mean_IA_SC = sum(IA_SC)/nrow(tab_moy), mean_IA_PA = sum(IA_PA)/nrow(tab_moy), mean_IA_PI = sum(IA_PI)/nrow(tab_moy))
  for (i in 2:(ncol(data_IA)-1)){
    mean_tempo <- sum(tab_moy[,i])/nrow(tab_moy)
    data_IA[,i] <- data_IA[,i]/mean_tempo
  }
  data_IA$mean_standard_AI <- apply(data_IA[, grep("IA", names(data_IA))], 1, function(x) mean(x, na.rm = T))
  data_IA <- data_IA %>% dplyr::select(-Any_NA)
  IA_long<-reshape2::melt(data_IA,id.vars="Year")
  nb_col <- length(unique(as.factor(IA_long$variable)))
  palette <- brewer.pal(nb_col,"Set1") #Max = 9!
  palette[nb_col] <- "#000000"
  #Plot des nouveaux IA standardisé à 1 / plot of the new AIs standardised
  v <- ggplot() + geom_line(aes(x=IA_long$Year, y=IA_long$value, color=IA_long$variable)) + geom_point(aes(x=IA_long$Year, y=IA_long$value, color=IA_long$variable))
  v <- v + ylab("Indice d'Abondance") #+ scale_color_brewer(palette="Set1")
  v <- v + scale_color_manual(values = palette)
  #v <- v + geom_line(aes(x=data_IA$Year, y=data_IA$mean_standard_AI), col = "black") + geom_point(aes(x=data_IA$Year, y=data_IA$mean_standard_AI), col = "black")
  print(v)



  if (MOY == TRUE){
    data_IA <- mean_3years(data_IA)
    IA_long<-reshape2::melt(data_IA,id.vars="Year")

    nb_col <- length(unique(as.factor(IA_long$variable)))
    palette <- brewer.pal(nb_col,"Set1") #Max = 9!
    palette[nb_col] <- "#000000"

    #Plot des nouveaux IA standardisé à 1 / plot of the new AIs standardised
    s <- ggplot() + geom_line(aes(x=IA_long$Year, y=IA_long$value, color=IA_long$variable)) + geom_point(aes(x=IA_long$Year, y=IA_long$value, color=IA_long$variable))
    s <- s + ylab("Indice d'Abondance") #+ scale_color_brewer(palette="Set1")
    s <- s + scale_color_manual(values = palette)
    #s <- s + geom_line(aes(x=data_IA$Year, y=data_IA$mean_standard_AI_cor), col = "black") + geom_point(aes(x=data_IA$Year, y=data_IA$mean_standard_AI_cor), col = "black")
    print(s)
  }

  return(data_IA)


}
