#' Cette fonction réunie les séries d'IA (/!\ MAX 5 SERIES) dans une seule table, standardise et plot plusieurs graphs en relation
#'
#'
#' @param nb_table : nb de table inclut dans "..." (=le nb de série d'IA différentes)
#' @param MOY   : si "oui" vient appliquer la fonction Moy_3ans()
#' @param vect_year_elim   : vecteur contenant les différentes années à éliminer. Sinon, vecteur vide c()
#' @param ...  : les différentes tables d'IA (max 5)
#'
#' @examples
#'
#' @export


prep_table<-function(nb_table, MOY, vect_year_elim, ...){
  listIA <- c(...)
  list_year <- c()
  for (i in 1:nb_table){
    list_year <- c(list_year, listIA[1 + 3*(i-1)])
  }

  min_year <- min(unlist(list_year))
  print(min_year)
  max_year <- max(unlist(list_year))
  print(max_year)
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
  data_IA$IA_MOYEN_STANDARD <- apply(data_IA[, grep("IA", names(data_IA))], 1, function(x) mean(x, na.rm = T))
  data_IA <- data_IA %>% dplyr::select(-Any_NA)
  IA_long<-reshape2::melt(data_IA,id.vars="Year")
  #Plot des nouveaux IA standardisé à 1 / plot of the new AIs standardised
  v <- ggplot() + geom_line(aes(x=IA_long$Year, y=IA_long$value, color=IA_long$variable)) + geom_point(aes(x=IA_long$Year, y=IA_long$value, color=IA_long$variable))
  v <- v + ylab("Indice d'Abondance") + scale_color_brewer(palette="Set1")
  v <- v + geom_line(aes(x=data_IA$Year, y=data_IA$IA_MOYEN_STANDARD), col = "black") + geom_point(aes(x=data_IA$Year, y=data_IA$IA_MOYEN_STANDARD), col = "black")
  print(v)


  if (length(vect_year_elim)>0){
    print("on élimine les années du vect_year_elim")
    for (i in 1:length(vect_year_elim)){
      data_IA <- subset(data_IA, !(Year == as.numeric(vect_year_elim[i])))
    }
  }


  if (MOY == "oui"){
    data_IA <- mean_3years(data_IA)


    IA_long<-reshape2::melt(data_IA,id.vars="Year")
    #Plot des nouveaux IA standardisé à 1 / plot of the new AIs standardised
    s <- ggplot() + geom_line(aes(x=IA_long$Year, y=IA_long$value, color=IA_long$variable)) + geom_point(aes(x=IA_long$Year, y=IA_long$value, color=IA_long$variable))
    s <- s + ylab("Indice d'Abondance") + scale_color_brewer(palette="Set1")
    s <- s + geom_line(aes(x=data_IA$Year, y=data_IA$IA_MOYEN_STANDARD_COR), col = "black") + geom_point(aes(x=data_IA$Year, y=data_IA$IA_MOYEN_STANDARD_COR), col = "black")
    print(s)
  }

  return(data_IA)


}
