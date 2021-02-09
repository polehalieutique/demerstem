#' la fonction glm_pres

#' @param tab          : les données auxquelles ajuster le modèle
#' @param parametres   : liste des parametres à tester
#' @param interactions : prendre en compte les interactions. ("auto") ou ("force")

#' @examples
#'  glm_pres_pa <- glm_pres(tableau_pa_pres, param_pa, interactions="auto")
#'  glm_pres_pi <- glm_pres(tableau_pi_pres, param_pi) #150 000 lignes, 9 facteurs = trop lourd pour les interactions
#' tableau_sc_pres <- pres_abs(tableau_sc, "PSEUDOTOLITHUS ELONGATUS")
#' tab <- tableau_sc_pres
#' tab_old <- tab
#' tab <- na.omit(tab)
#' nrow(tab_old) ; nrow(tab)
#' param_sc <- param_use(tableau_sc_pres, list_param)
#' param_sc <- param_sc[-1] #####type_campagne pose probleme
#' parametres <- param_sc
#' @export


glm_pres <- function(tab, parametres, interactions, formule_select){
  #passer les parametres en facteur
  lapply(tab[,parametres], as.factor)
  # Mise en forme des parametres
  a <- paste(parametres[1], "+")
  for (i in 2 : (length(parametres)-1)){
    a <- paste(a, parametres[i], "+")}
  a <- paste(a, parametres[length(parametres)])
  formule <- paste("presence ~", a)
  formule_inter <- paste("presence ~ (", a, ")^2") #test des interactions ordre2

  #Ecriture du modele de base
  Model<-glm(as.formula(formule), family=binomial, data=tab)
  if (formule_select == "auto"){
    if(interactions=="auto"){
      #Ecriture du modele avec intercations sous conditions de nb_lignes^nb_parametres < 100000^5
      if(nrow(tab)^length(parametres) < 100000^5){
        Model_inter <- glm(as.formula(formule_inter), family=binomial, data=tab)
        if(AIC(Model_inter)<(AIC(Model)-10)){
          Model_old <- Model
          Model <- Model_inter
          formule <- formule_inter
          print(paste("les interactions ordre 2 sont prises en compte, l'AIC est amelioré de", AIC(Model_old)-AIC(Model_inter), "points"))
        }
      } else {
        print("les interactions ordre 2 ne sont pas testees car le nombre de donnees ou de parametres est trop important")
      }

    } else if (intercations=="force"){
      Model_inter <- glm(as.formula(formule_inter), family=binomial, data=tab)
      Model <- Model_inter
      formule <- formule_inter
      print("les interactions ordre 2 sont prises en compte")
    }
  } else {
    formule <- formule_select
    Model <- glm(as.formula(formule), family=binomial, data=tab)
  }


  #selection aic
  Model_aic=stepAIC(Model, formule ,trace=TRUE, direction="both")

  if(AIC(Model_aic)<(AIC(Model)-10)){
    Model_old <- Model
    Model <- Model_aic
    formule <- as.character(Model_aic$formula)[2]
    print(paste("AIC ameliore de ", AIC(Model_old)-AIC(Model_aic), "points apres StepAIC"))
  }else{
    print("La selection stepAIC n'apporte pas d'amelioration nette, elle n'est pas prise en compte")
  }

  # Print du modele sélectionné
  if (formule_select == "auto"){
    print("Le modele sélectionné par optimisation statistique est le suivant :")
  } else {
    print("Le modele que vous avez sélectionné :")
  }
  print(formule)
  print(summary(Model))
  print(anova(Model))

  # resume modele et graphiques residus
  summary(Model)
  res1 <- resid(Model)
  fit1 <- fitted(Model)
  par(mfrow = c(2,2))
  hist(res1)
  plot(fit1,res1)
  qqnorm(res1) ; qqline(res1)
  plot(Model, 4)

  return(Model)
}

