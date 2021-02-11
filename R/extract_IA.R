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


extract_IA<-function(glm_pres,glm_ab, titre){
  ### Tableau IA par année
  # Pres/Abs
  table_pres <- as.data.frame(coef(summary(glm_pres))) #Table avec estim, std-error et p-values
  table_pres$facteur <- as.character(rownames(table_pres))
  table_pres$ExpEstimate <- exp(table_pres$Estimate)/(1+exp(table_pres$Estimate)) #correction de Laurent
  #table_pres<- table_pres %>% dplyr::select(-"z value")

  table_pres_annee <- table_pres %>% filter(substr(facteur, start = 1, stop = 3) == "ann")

  # Abondance
  VAR <- var(residuals(glm_ab))
  table_ab <- as.data.frame(coef(summary(glm_ab)))
  table_ab$facteur <- as.character(rownames(table_ab))
  table_ab$ExpEstimate <- exp(table_ab$Estimate + 0.5*VAR)
  #table_ab <- table_ab %>% dplyr::select(-"t value")

  table_ab_annee <- table_ab %>% filter(substr(facteur, start = 1, stop = 3) == "ann")

  # Couplage
  table_annee_final <- inner_join(table_ab_annee, table_pres_annee, by='facteur') #Inner Join? Ou Left joint? Avec left on a des NA.
  table_annee_final <- table_annee_final %>% dplyr::select(facteur, ExpEstimate.x, ExpEstimate.y) %>%
    mutate(EstimateurFinal = ExpEstimate.x * ExpEstimate.y) %>% dplyr::select(facteur, EstimateurFinal)
  table_annee_final <- table_annee_final %>% mutate(annee = as.factor(substr(facteur, start = 6, stop = 9)))


  g1<-ggplot(table_annee_final) + geom_bar(aes(x=annee, y=EstimateurFinal), stat="identity") + ylab("Indice d'Abondance") + ggtitle(paste(titre, "avec années numériques")) #"IA pêche scientifique Guinée"


  #Plot avec Annee as numeric
  table_annee_final$annee <- as.numeric(as.character(table_annee_final$annee))

  g2<-ggplot(table_annee_final) + geom_bar(aes(x=annee, y=EstimateurFinal), stat="identity") + ylab("Indice d'Abondance") + ggtitle(paste(titre, "avec annees en facteurs"))
  g3<-ggplot(table_annee_final) + geom_line(aes(x=annee, y=EstimateurFinal), stat="identity") + ylab("Indice d'Abondance") + ggtitle(paste(titre, "avec annees en facteurs"))
  print(g1)
  print(g2)
  print(g3)

  return (table_annee_final)
}

