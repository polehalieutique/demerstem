#' la fonction glm_ia établit le modèle d'indices d'abondances : (sous-modèle positif dans un GLM delta). Une selection des parametre est effectuee sur critere statistique (stepAIC) ; ajout de l'argument "formule_select" : soit "auto" et la fonction choisit le modèle par opti stat, soit on lui donne la formule que l'on souhaite tester. Permet de finaliser.

#' @param tab          : les données auxquelles ajuster le modèle
#' @param parametres   : liste des parametres à tester
#' @param interactions : prendre en compte les interactions. ("auto") ou ("force")
#' @param formule_select : permet la selection manuel de la formule à tester dans le glm / "auto" sinon

#' @examples
#'  #PA
#'  glm_ia_pa <- glm_ia(tableau_pa_ab, param_pa, logtrans="auto", interactions="auto")

#' #PI
#' glm_ia_pi <- glm_ia(tableau_pi_ab, param_pi, logtrans="auto", interactions="auto") # 60 000 lignes, 9 facteurs = trop lourd pour interactions2

#' #SC
#' glm_ia_sc <- glm_ia(tableau_sc_ab, param_sc, logtrans="auto", interactions="auto")

#' @export


glm_IAplus <- function(tab, parametres, formule_select){
  #passer les parametres en facteur
  lapply(tab[,parametres], as.factor)
  #ecriture formule
  a <- paste("as.factor(", parametres[1], ") +")
  for (i in 2 : (length(parametres)-1)){
    a <- paste(a, "as.factor(", parametres[i], ") +")}
  a <- paste(a, "as.factor(", parametres[length(parametres)], ")")
  formule_log <- paste("log(i_ab+0.0001) ~", a)
  #formule_ini <- formule_log
  #test des interactions ordre2
  formule_log_inter <- paste("log(i_ab+0.0001) ~ (", a,")^2")


  if (formule_select == "auto"){
    print("selection stepAIC en partant du modele sature :")
    model_sature <- glm(formula = formule_log_inter, family=gaussian, data=tab)
    test_sature <- stepAIC(model_sature, scope=(lower=~1)) #trace=TRUE
    print("selection stepAIC en partant du modele min :")
    model_cst <- glm(formula = log(i_ab + 1e-04) ~ 1, family=gaussian, data=tab)
    test_cst <- stepAIC(model_cst, scope=(upper=paste("~ (", a,")^2"))) #trace=TRUE

    print(anova(test_sature, test_cst))

    if (AIC(test_sature) < AIC(test_cst)){
      Model <- test_sature
      formule <- test_sature$formula
    } else {
      Model <- test_cst
      formule <- test_cst$formula
    }

  } else {
    formule <- formule_select
    Model <- glm(as.formula(formule) , family=gaussian, data=tab)
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

