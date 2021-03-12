#' Delta coupling of 2 GLMs
#'
#' \code{delta_glm} realize the delta-coupling using the 2 precedents GLMs, extract the year factor, calculate the AI and display the plots.
#'
#'
#' @param glm_pres_abs    : outputs from the function model_pres_abs()
#' @param glm_abundance   : outputs from the function model_IAplus()
#' @param title           : first part of the title for the plots
#' @param type            : type of the data series. Ex : "SC", "PA", etc...
#'
#' @examples
#' data(tableau_sc)
#' glm_abundance <- model_ai_plus(tableau_sc, esp = "PSEUDOTOLITHUS ELONGATUS", effort = "auto", title = "SC", list_param = c("annee", "saison", "strate"),  espece_id='nom_taxonomique', var_eff_list=c("surface_chalutee"), catch_col='total_capture', interactions ="N", limit=0.001, formula_select = "log(i_ab+0.0001) ~ strate + annee + saison")
#' glm_pres_abs <- model_pres_abs(tableau_sc, esp="PSEUDOTOLITHUS ELONGATUS", effort="auto", title="SC", list_param=c("annee", "saison", "strate"),  espece_id='nom_taxonomique', var_eff_list=c("surface_chalutee"), catch_col='total_capture', interactions = "N", limit=0.0001, formula_select = "presence ~ strate + annee + saison")
#' delta_glm(glm_pres_abs, glm_abundance, title, type = "SC")
#' @export


delta_glm<-function(glm_pres_abs,glm_abundance, title, type){
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
  VAR <- var(residuals(glm_abundance))
  table_ab <- as.data.frame(coef(summary(glm_abundance)))

  vect_param <- c(all.vars(formula(glm_abundance))[-1]) # liste des paramètres
  table_finale2 <- c()
  for (i in 2:(length(vect_param)+1)){
    table_tempo <- as.data.frame(dummy.coef(glm_abundance)[i])
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

  g1<-ggplot(table_annee_final) + geom_bar(aes(x=annee, y=EstimateurFinal), stat="identity") + ylab("Indice d'Abondance") + ggtitle(paste(title, "avec années numériques")) + theme(axis.text.x = element_text(angle = 35)) #"IA pêche scientifique Guinée"


  #Plot avec Annee as numeric
  table_annee_final$annee <- as.numeric(as.character(table_annee_final$annee))
  table_annee_final$type <- type

  g2<-ggplot(table_annee_final) + geom_bar(aes(x=annee, y=EstimateurFinal), stat="identity") + ylab("Indice d'Abondance") + ggtitle(paste(title, "avec annees en facteurs"))
  g3<-ggplot(table_annee_final) + geom_line(aes(x=annee, y=EstimateurFinal), stat="identity") + ylab("Indice d'Abondance") + ggtitle(paste(title, "avec annees en facteurs"))
  print(g1)
  print(g2)
  print(g3)

  return (table_annee_final)
}


