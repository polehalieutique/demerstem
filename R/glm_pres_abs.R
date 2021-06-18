#' Fit a Generalized Linear Models on presence/absence data
#'
#' \code{glm_pres_abs} returns the best GLM model possible for the presence/absence data, based on an AIC selection process (formula_select = "auto"). It runs the GLMs, plot the residuals analysis graphs and return the GLM outputs.
#'
#' @param tab             table of presence and absence data, coming as the output of table_pres_abs
#' @param parameters      list of parameters to test
#' @param formula_select  if "auto", the function select which formula as the lowest AIC. Else, run the selected formula.
#' @param family          "gaussian" by default. You can define it on "binomial" to switch
#'
#' @return \code{glm_pres_abs} can either return the best GLM model based on AIC comparison, or return the outputs of the GLM based on the formula given in \emph{formula_select}
#'
#' @examples
#' data(tableau_sc)
#' list_param <- c("annee", "strate", "saison")
#' table_ex <- table_pres_abs(tableau_sc, effort="auto", esp="PSEUDOTOLITHUS ELONGATUS", list_param=c("annee", "saison", "strate"), espece_id='nom_taxonomique', var_eff_list=c("surface_chalutee"), catch_col='total_capture', limit=0.0001)
#' param <- param_use(table_ex, list_param)
#' for (i in 1:length(param)){
#' table_ex[,param[i]] <- as.factor(table_ex[,param[i]])
#' table_ex[,param[i]] <- droplevels(table_ex[,param[i]])
#' contrasts(table_ex[,param[i]]) <- contr.sum(levels(table_ex[,param[i]]))
#' }
#' glm_pres_abs(table_ex, param, formula_select = "auto")
#'
#' @export


glm_pres_abs <- function(tab, parameters, formula_select, summary = FALSE){
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
  print(anova(Model))


  # resume modele et graphiques residus
  summary(Model)


  if (summary == TRUE){
    print(summary(Model))
    res1 <- resid(Model)
    fit1 <- fitted(Model)
    par(mfrow = c(2,2))
    hist(res1)
    plot(fit1,res1)
    qqnorm(res1) ; qqline(res1)
    plot(Model, 4)
  }

  return(Model)
}
