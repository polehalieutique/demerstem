#' Cette fonction réalise le couplage Delta à partir des GLM pres/abs et abondance, extrait le facteur année et réalise 3 plots de l'IA déduit.
#'
#'
#' @param glm_pres : sorties de la fonction delta_presabs()
#' @param glm_ab   : sorties de la fonction delta_abondance()
#' @param titre    : première partie des titres de graphiques. Ex : "IA de pêches scientifiques"
#'
#' @examples
#' #SC
#' table_annee_final_SC <- extract_glm(glm_pres_sc,glm_ab_sc, titreSC)
#' @export


delta_GLM_IA<-function(glm_pres,glm_ab, titre, type){
  ### Tableau IA par année

  # Pres/Abs
  table_pres <- as.data.frame(coef(summary(glm_pres)))
  vect_param <- c(all.vars(formula(glm_pres))[-1]) # liste des paramètres
  table_finale <- c()
  for (i in 2:(length(vect_param)+1)){
    table_tempo <- as.data.frame(dummy.coef(glm_pres)[i])
    table_tempo$modalite <- rownames(table_tempo)
    rownames(table_tempo) <- NULL
    table_tempo$variable <- vect_param[i-1]
    colnames(table_tempo) <- c("Estimate", "modalite", "variable")
    table_finale <- rbind(table_finale, table_tempo)
  }
  table_finale$ExpEstimate <- table_finale$Estimate + table_pres[1,1]
  table_finale$ExpEstimate <- exp(table_finale$ExpEstimate)/(1+exp(table_finale$ExpEstimate)) #correction de Laurent

  table_finale <- table_finale %>% filter(variable == "annee")


  # Abondance
  VAR <- var(residuals(glm_ab))
  table_ab <- as.data.frame(coef(summary(glm_ab)))

  vect_param <- c(all.vars(formula(glm_ab))[-1]) # liste des paramètres
  table_finale2 <- c()
  for (i in 2:(length(vect_param)+1)){
    table_tempo <- as.data.frame(dummy.coef(glm_ab)[i])
    table_tempo$modalite <- rownames(table_tempo)
    rownames(table_tempo) <- NULL
    table_tempo$variable <- vect_param[i-1]
    colnames(table_tempo) <- c("Estimate", "modalite", "variable")
    table_finale2 <- rbind(table_finale2, table_tempo)
  }
  table_finale2$ExpEstimate <- table_finale2$Estimate + table_ab[1,1]
  table_finale2$ExpEstimate <- exp(table_finale2$Estimate + 0.5*VAR) # correction de lolo


  table_finale2 <- table_finale2 %>% filter(variable == "annee")

  # Couplage
  table_annee_final <- inner_join(table_finale, table_finale2, by='modalite') #Inner Join? Ou Left joint? Avec left on a des NA.
  table_annee_final <- table_annee_final %>% dplyr::select(modalite, ExpEstimate.x, ExpEstimate.y) %>%
    mutate(EstimateurFinal = ExpEstimate.x * ExpEstimate.y) %>% dplyr::select(modalite, EstimateurFinal)
  #table_annee_final <- table_annee_final %>% mutate(annee = as.factor(substr(facteur, start = 6, stop = 9)))
  colnames(table_annee_final) <- c("annee", "EstimateurFinal")

  g1<-ggplot(table_annee_final) + geom_bar(aes(x=annee, y=EstimateurFinal), stat="identity") + ylab("Indice d'Abondance") + ggtitle(paste(titre, "avec années numériques")) + theme(axis.text.x = element_text(angle = 35)) #"IA pêche scientifique Guinée"


  #Plot avec Annee as numeric
  table_annee_final$annee <- as.numeric(as.character(table_annee_final$annee))
  table_annee_final$type <- type

  g2<-ggplot(table_annee_final) + geom_bar(aes(x=annee, y=EstimateurFinal), stat="identity") + ylab("Indice d'Abondance") + ggtitle(paste(titre, "avec annees en facteurs"))
  g3<-ggplot(table_annee_final) + geom_line(aes(x=annee, y=EstimateurFinal), stat="identity") + ylab("Indice d'Abondance") + ggtitle(paste(titre, "avec annees en facteurs"))
  print(g1)
  print(g2)
  print(g3)

  return (table_annee_final)
}


