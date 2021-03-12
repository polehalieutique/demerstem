#' Fit a Generalized Linear Models on abundance data
#'
#' \code{glm_ai_plus} returns the best GLM model possible for the abundance data, based on an AIC selection process (formula_select = "auto"). It runs the GLMs, plot the residuals analysis graphs and return the GLM outputs.
#'
#' @param tab            : table of abundance data, extracted from the output of table_pres_abs
#' @param parameters     : list of parameters to test
#' @param formula_select : if "auto", the function select which formula as the lowest AIC. Else, run the selected formula.

#' @return \code{glm_ai_plus} can either return the best GLM model based on AIC comparison, or return the outputs of the GLM based on the formula given in \emph{formula_select}

#' @examples
#' data(tableau_sc)
#' table_ex <- table_pres_abs(tableau_sc, effort="auto", esp="PSEUDOTOLITHUS ELONGATUS", list_param=c("annee", "saison", "strate"), espece_id='nom_taxonomique', var_eff_list=c("surface_chalutee"), catch_col='total_capture', limit=0.0001)
#' table_ex_abundance <- filter(tableau_ex, presence==1)
#' param <- param_use(tableau_ex_abundance, list_param)
#' for (i in 1:length(param)){
#' tableau_ex_abundance[,param[i]] <- as.factor(tableau_ex_abundance[,param[i]])
#' tableau_ex_abundance[,param[i]] <- droplevels(tableau_ex_abundance[,param[i]])
#' contrasts(tableau_ex_abundance[,param[i]]) <- contr.sum(levels(tableau_ex_abundance[,param[i]]))
#' }
#' glm_ai_plus(tableau_ex_abundance, param, formula_select = "auto")
#' @export


glm_ai_plus <- function(tab, parameters, formula_select){
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

