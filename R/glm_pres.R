#' la fonction glm_pres

#' @param tab          : les données auxquelles ajuster le modèle
#' @param parametres   : liste des parametres à tester
#' @param formule_select : permet la selection manuel de la formule à tester dans le glm / "auto" sinon

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


glm_pres <- function(tab, parametres, formule_select){
  #passer les parametres en facteur
  lapply(tab[,parametres], as.factor)
  # Mise en forme des parametres
  a <- paste("as.factor(", parametres[1], ") +")
  for (i in 2 : (length(parametres)-1)){
    a <- paste(a, "as.factor(", parametres[i], ") +")}
  a <- paste(a, "as.factor(", parametres[length(parametres)], ")")
  formule <- paste("presence ~", a)
  formule_inter <- paste("presence ~ (", a, ")^2") #test des interactions ordre2


  if (formule_select == "auto"){
    print("selection stepAIC en partant du modele sature :")
    model_sature <- glm(formula = formule_inter, family=gaussian, data=tab)
    test_sature <- stepAIC(model_sature, scope=(lower=~1)) #trace=TRUE

    print("selection stepAIC en partant du modele min :")
    model_cst <- glm(formula = presence ~ 1, family=gaussian, data=tab)
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

