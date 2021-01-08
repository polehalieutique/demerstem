#' la fonction glm_ia établit le modèle d'indices d'abondances : (sous-modèle positif dans un GLM delta). Une selection des parametre est effectuee sur critere statistique (stepAIC)

#' @param tab          : les données auxquelles ajuster le modèle
#' @param parametres   : liste des parametres à tester
#' @param logtrans     : pour automatiser la logtransformation des donnees si elle apporte une amélioration ("auto") ou pour forcer la logtransformation ("force")
#' @param interactions : prendre en compte les interactions. ("auto") ou ("force")

#' @examples
#'  #PA
#'  glm_ia_pa <- glm_ia(tableau_pa_ab, param_pa, logtrans="auto", interactions="auto")

#' #PI
#' glm_ia_pi <- glm_ia(tableau_pi_ab, param_pi, logtrans="auto", interactions="auto") # 60 000 lignes, 9 facteurs = trop lourd pour interactions2

#' #SC
#' glm_ia_sc <- glm_ia(tableau_sc_ab, param_sc, logtrans="auto", interactions="auto")

#' @export



glm_ia <- function(tab, parametres, logtrans, interactions){
  #passer les parametres en facteur
  lapply(tab[,parametres], as.factor)

  #ecriture formule
  a <- paste(parametres[1], "+")
  for (i in 2 : (length(parametres)-1)){
    a <- paste(a, parametres[i], "+")}
  a <- paste(a, parametres[length(parametres)])
  formule <- paste("i_ab ~", a)
  formule_log <- paste("log(i_ab+0.0001) ~", a)
  formule_inter <- paste("i_ab ~ (", a, ")^2") #test des interactions ordre2
  formule_log_inter <- paste("log(i_ab+0.0001) ~ (", a,")^2")

  if(logtrans=="auto"){
    log_trans <<-"no" #par defaut, tant qu'aucune logtransformation n'est effectuée

    #Ecriture du modele de base
    Model <- glm(as.formula(formule) , family=gaussian, data=tab)
    Model_log <- glm(as.formula(formule_log) , family=gaussian, data=tab) # ou get(parametres)

    # Caution !!
    # AIC
    # Comments
    # The AIC criteria uses the Deviance.
    # A gaussian deviance with log-transformed values (what is done in AIC(model))
    # IS NOT equivalent to computing a LogNormal deviance with non-transformed values
    AIC.log <- AIC(Model_log)+2*sum(log(tab$i_ab), na.rm=TRUE)

    if(	AIC.log < (AIC(Model)-10)){
      Model_old <- Model
      Model <- Model_log
      formule <- formule_log
      print(paste("les donnees sont passées au log (log+0.0001), l'AIC est amelioré de", AIC(Model_old)-AIC(Model_log), "points"))
      log_trans <<- "yes"
    }

    if(interactions=="auto"){
      #Ecriture du modele avec intercations sous conditions de nb_lignes^nb_parametres < 100000^5
      if(nrow(tab)^length(parametres) < 10^25){
        Model_inter <- glm(as.formula(formule_inter) , family=gaussian, data=tab)
        Model_log_inter <- glm(as.formula(formule_log_inter) , family=gaussian, data=tab) # ou get(parametres)

        AIC.log.inter <- AIC(Model_log_inter)+2*sum(log(tab$i_ab), na.rm=TRUE)

        if(AIC(Model_inter) <(AIC(Model)-10)){
          Model_old <- Model
          Model <- Model_inter
          formule <- formule_inter
          print(paste("les interactions sont prises en compte, l'AIC est amelioré de", AIC(Model_old)-AIC(Model_inter), "points"))
        }

        if(AIC.log.inter < (AIC(Model)-10)){
          Model_old <- Model
          Model <- Model_log_inter
          formule <- formule_log_inter
          log_trans <<- "yes"
          print(paste("les donnees sont passees au log (log+0.0001) et les interactions ordre 2 sont prises en compte, l'AIC est amelioré de", AIC(Model_old)-AIC(Model_log_inter), "points"))}
      }else{
        print("les interactions ordre 2 ne sont pas testees car le nombre de donnees ou de parametres est trop important")}

    }else if(interactions=="force"){
      Model_inter <- glm(as.formula(formule_inter) , family=gaussian, data=tab)
      print(paste("les interactions sont prises en compte, l'AIC est amelioré de", AIC(Model)-AIC(Model_inter), "points"))
      Model <- Model_inter
      Model_log_inter <- glm(as.formula(formule_log_inter) , family=gaussian, data=tab) # ou get(parametres)
      AIC.log.inter <- AIC(Model_log_inter)+2*sum(log(tab$i_ab))

      if(AIC.log.inter <(AIC(Model)-10)){
        Model <- Model_log_inter
        formule <- formule_log_inter
        print(paste("les donnees sont passees au log (log+0.0001) et les interactions ordre 2 sont prises en compte"))
        log_trans <<- "yes"}
    }
  }else if (logtrans=="force") {
    log_trans <<- "yes"
    print("les donnees sont passées au log (log+0.0001)")
    #Ecriture du modele de base
    Model_log <- glm(as.formula(formule_log) , family=gaussian, data=tab) # ou get(parametres)
    AIC.log <- AIC(Model_log)+2*sum(log(tab$i_ab))
    Model <- Model_log


    if(interactions=="auto"){
      #Ecriture du modele avec intercations sous conditions de nb_lignes^nb_parametres < 100000^5
      if(nrow(tab)^length(parametres) < 100000^5){
        Model_log_inter <- glm(as.formula(formule_log_inter), family=gaussian, data=tab) # ou get(parametres)

        AIC.log.inter <- AIC(Model_log_inter)+2*sum(log(tab$i_ab))
        if(AIC(Model.log.inter)<(AIC(Model)-10)){
          Model_old <- Model
          Model <- Model_log_inter
          formule <- formule_log_inter
          print(paste("les donnees sont passees au log (log+0.0001) et les interactions ordre 2 sont prises en compte, l'AIC est amelioré de", AIC(Model_old)-AIC(Model_log_inter), "points"))}
      }else{
        print("les interactions ordre 2 ne sont pas testees car le nombre de donnees ou de parametres est trop important")}

    }else if(interactions=="force"){
      Model_log_inter <- glm(as.formula(formule_log_inter) , family=gaussian, data=tab) # ou get(parametres)
      AIC.log.inter <- AIC(Model_log_inter)+2*sum(log(tab$i_ab))

      if(AIC(Model.log.inter)<(AIC(Model)-10)){
        Model <- Model_log_inter
        formule <- formule_log_inter
        print(paste("les donnees sont passees au log (log+0.0001) et les interactions ordre 2 sont prises en compte"))}
    }
  }

  #selection AIC
  Model_aic=stepAIC(Model, formule ,trace=TRUE, direction="both")

  if(AIC(Model_aic)<(AIC(Model)-5)){
    Model_old <- Model
    Model <- Model_aic
    print(paste("AIC ameliore de ", AIC(Model_old)-AIC(Model_aic), "points apres StepAIC"))
  }else{
    print("La selection stepAIC n'apporte pas d'e difference d'amelioration nette, elle n'est pas prise en compte")}


  # resume modele et graphiques residus
  summary(Model)
  res1 <- resid(Model)
  fit1 <- fitted(Model)
  par(mfrow = c(2,2))
  hist(res1)
  plot(fit1,res1)
  qqnorm(res1) ; qqline(res1)
  plot(Model, 4)

  log_trans
  return(Model)
}
