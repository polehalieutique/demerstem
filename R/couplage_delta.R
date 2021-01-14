#' la fonction couplage_delta s'applique au cas où le sous_modèle glm_positif est gaussien

#' @param glm_posit : Sous-modele abondance
#' @param glm_pres  : Sous-modele presence/absence
#' @param data_posit: donnees d'abondances (présences seulement)
#' @param data_pres : donnees presence_absence
#' @param tab       : les donnees auxquelles on ajuste le modele
#' @param esp       : espece
#' @param facteur_flotille : facteur pour lequel il est interessant de tracer les series temporelles d'indices modelises. (Ex : Trouver une flotille de tuning)

#' @examples
#'  #PA
#'  couplage_delta(glm_ia_pa, glm_pres_pa, tableau_pa, "BOBO", type_donnee="commercial", effort="auto", list_param, espece_id_list, var_eff_list, ope_id, col_capture, facteur_flotille="engin_peche2", seuil=0.05, titre="PA")
#' @export
#'
couplage_delta <- function(glm_posit, glm_pres, tab , esp, type_donnee, effort, list_param, espece_id_list, var_eff_list, ope_id, col_capture, facteur_flotille, seuil, titre) {

  tab_pres <- indice_ab_pres(tab, type_donnee, effort, esp, list_param,  espece_id_list, var_eff_list, ope_id, col_capture, seuil)
  tab_ab <- filter(as.data.frame(tab_pres), presence==1)

  # Sous-modele positifs : 2 cas
  if(log_trans=="yes"){
    ##Cas 1 : les donnees ont ete log_transformees
    # Expected mean of the log-density from the glm model
    E.logdens.pos <- predict.glm(glm_posit, tab_ab, type="r", se=T)$fit

    # Use the the "Laurent correction" to get the expected mean of log-transformed data
    var.logdens.pos <- var(residuals(glm_posit))
    E.dens.pos <- exp(E.logdens.pos + 0.5*var.logdens.pos)
  }else{
    ##Cas 2 : les donnees n'ont pas été log_transformees
    E.dens.pos <- predict.glm(glm_posit, tab_ab, type="r", se=T)$fit
  }


  # Sous-modele presences
  presence.pred <- predict.glm(glm_pres, tab_pres, type="r", se=T)$fit

  # Couplage : modele conditionel
  E.dens <- E.dens.pos * presence.pred


  # Plot observations vs predictions
  plot(tab_pres$i_ab, E.dens , xlim = c(0,1000), ylim = c(0,1000))
  points(tab_pres$i_ab, tab_pres$i_ab,col="red", type='l')

  # Tracer les series temporelles i_ab moyen et par flotilles
  tab_ab2 <- cbind(tab_ab, E.dens.pos) #  plot(tab_ab2$i_ab, tab_ab2$E.dens.pos, xlim=c(0,1000), ylim=c(0,1000))
  tab_pres2 <- cbind(tab_pres, presence.pred)
  tab_ab_pres <- dplyr::inner_join(tab_pres2, tab_ab2) %>% dplyr::mutate(E.dens=E.dens.pos * presence.pred)
  tab_ab_pres[,facteur_flotille] <- as.factor(tab_ab_pres[,facteur_flotille])

  requete <- tab_ab_pres %>% dplyr::group_by(flotille=tab_ab_pres[,facteur_flotille], annee) %>% dplyr::summarise(mean_E.dens=mean(E.dens))
  a <- ggplot()  + geom_line(data=requete, aes(annee, mean_E.dens, group=flotille, color=flotille))+
    stat_summary(data=tab_ab_pres, aes(annee, E.dens, color="INDICE MOYEN"), fun="mean", geom="line", linetype=2)+
    ylab("indice abondance") + ggtitle(paste("indice abondance modelise", titre))

  b <- ggplot()  + stat_summary(data=tab_ab_pres, aes(annee, E.dens), fun="mean", geom="line") + ylab("indice abondance") + ggtitle(paste("indice abondance moyen seul", titre))
  c <- ggarrange(a, b, ncol = 1, nrow = 2)
  print(c)

  indice_ab_model <<- tab_ab_pres
}
