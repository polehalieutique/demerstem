#' this function select the best GLM model possible for the abundance data, based on an AIC selection process (formula_select = "auto"). It runs the GLMs, plot the residuals analysis graphs and return the GLM outputs.

#' @param tab            : table of abundance data, extracted from the output of table_pres_abs (presence = 1)
#' @param parameters     : list of parameters to test
#' @param formula_select : if "auto", the function select which formula as the lowest AIC. Else, run the selected formula.

#' @examples
#'  #PA
#'  glm_ia_pa <- glm_ia(tableau_pa_ab, param_pa, logtrans="auto", interactions="auto")

#' #PI
#' glm_ia_pi <- glm_ia(tableau_pi_ab, param_pi, logtrans="auto", interactions="auto") # 60 000 lignes, 9 facteurs = trop lourd pour interactions2

#' #SC
#' glm_ia_sc <- glm_ia(tableau_sc_ab, param_sc, logtrans="auto", interactions="auto")

#' @export


glm_ai_plusplus <- function(tab, parameters, formula_select){
  #passer les parametres en facteur
  lapply(tab[,parameters], as.factor)
  #ecriture formule
  a <- paste(parameters[1], "+")
  for (i in 2 : (length(parameters)-1)){
    a <- paste(a, parameters[i], "+")}
  a <- paste(a, parameters[length(parameters)])
  formula_log <- paste("log(i_ab+0.0001) ~", a)
  #test des interactions ordre2
  formula_log_inter <- paste("log(i_ab+0.0001) ~ (", a,")^2")


  if (formula_select == "auto"){
    print("selection stepAIC en partant du modele sature :")
    print("stepAIC selection starting with the full model :")
    model_sature <- glm(formula = formula_log_inter, family=gaussian, data=tab)
    test_sature <- stepAIC(model_sature, scope=(lower=~1)) #trace=TRUE
    print("selection stepAIC en partant du modele min :")
    print("stepAIC selection starting with the minimal model :")
    model_cst <- glm(formula = log(i_ab + 1e-04) ~ 1, family=gaussian, data=tab)
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
    print("The selected model by statistical optimisation is:")
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

