#' this function select the best GLM model possible for the presence/absence data, based on an AIC selection process (formula_select = "auto"). It runs the GLMs, plot the residuals analysis graphs and return the GLM outputs.

#' @param tab            : table of presence and absence data, coming as the output of table_pres_abs
#' @param parameters     : list of parameters to test
#' @param formula_select : if "auto", the function select which formula as the lowest AIC. Else, run the selected formula.

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


glm_pres_abs <- function(tab, parameters, formula_select){
  #passer les parametres en facteur
  lapply(tab[,parameters], as.factor)
  # Mise en forme des parametres
  a <- paste(parameters[1], "+")
  for (i in 2 : (length(parameters)-1)){
    a <- paste(a, parameters[i], "+")}
  a <- paste(a, parameters[length(parameters)])
  formula <- paste("presence ~", a)
  formula_inter <- paste("presence ~ (", a, ")^2") #test des interactions ordre2


  if (formula_select == "auto"){
    print("selection stepAIC en partant du modele sature :")
    print("stepAIC selection starting with the full model :")
    model_sature <- glm(formula = formula_inter, family=gaussian, data=tab)
    test_sature <- stepAIC(model_sature, scope=(lower=~1)) #trace=TRUE

    print("selection stepAIC en partant du modele min :")
    print("stepAIC selection starting with the minimal model :")
    model_cst <- glm(formula = presence ~ 1, family=gaussian, data=tab)
    test_cst <- stepAIC(model_cst, scope=(upper=paste("~ (", a,")^2"))) #trace=TRUE

    print(anova(test_sature, test_cst))

    if (AIC(test_sature) < AIC(test_cst)){
      Model <- test_sature
      formula <- test_sature$formula
    } else {
      Model <- test_cst
      formula <- test_cst$formula
    }

  } else {
    formula <- formula_select
    Model <- glm(as.formula(formula) , family=gaussian, data=tab)
  }


  # Print du modele sélectionné
  if (formula_select == "auto"){
    print("Le modele sélectionné par optimisation statistique est le suivant :")
    print("Le modele sélectionné par optimisation statistique est le suivant :")
  } else {
    print("Le modele que vous avez sélectionné :")
    print("The model you selected:")
  }
  print(formula)
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