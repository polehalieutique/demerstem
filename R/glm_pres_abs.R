#' Fit a Generalized Linear Models on presence/absence data
#'
#' \code{glm_pres_abs} returns the best GLM model possible for the presence/absence data, based on an AIC selection process (formula_select = "auto"). It runs the GLMs, plot the residuals analysis graphs and return the GLM outputs.
#'
#' @param tableau_pres             table of presence and absence data, coming as the output of table_pres_abs
#' @param parameters      list of parameters to test
#' @param formula_select  if "auto", the function select which formula as the lowest AIC. Else, run the selected formula.
#' @param force_interaction Factorial plan can be incomplete, thus Anova type III won't work. Allow to coerce an examination of interactions by using Anova type I.

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


glm_pres_abs <- function (tableau_pres, parameters, formula_select, summary = FALSE)
{

  # parameters as factors
  lapply(tableau_pres[,parameters], as.factor)
  # Preparation of factors
  a <- paste(parameters[1], "+")
  for (i in 2 : (length(parameters)-1)){
    a <- paste(a, parameters[i], "+")}
  a <- paste(a, parameters[length(parameters)])
  formula <- paste("presence ~", a)
  formula_inter <- paste("presence ~ (", a, ")^2") #order 2 interactions


  if (formula_select == "auto"){
    print("stepAIC selection starting with the full model :")
    model_sature <- glm(formula = formula_inter, family=binomial, data=tableau_pres)
    test_sature <- stepAIC(model_sature, scope=(lower=~1)) #trace=TRUE

    print("stepAIC selection starting with the minimal model :")
    model_cst <- glm(formula = presence ~ 1, family=binomial, data=tableau_pres)
    test_cst <- stepAIC(model_cst, scope=(upper=paste("~ (", a,")^2"))) #trace=TRUE

    print(anova(test_sature, test_cst))

    if (AIC(test_sature) < AIC(test_cst)){
      Model <- test_sature
      formula <- test_sature$formula
    } else {
      Model <- test_cst
      formula <- test_cst$formula
    }

  }

  else {
    formula <- formula_select
  }

    # Print du modele sélectionné
    if (formula_select == "auto"){
      print("The statiscally selected model is :")

    } else {
      print("The model you have selected :")

    }

  Model <- glm(as.formula(formula), family=binomial, data=tableau_pres)
  print(formula)
  print(paste0( "AIC = ",round(AIC(Model),3)))


  print(summary(aov(Model)))
  print("% of variability explained by each effect : ")
  table_var <- round(summary(aov(Model))[[1]][2]*100/sum(summary(aov(Model))[[1]][2]),1)
  table_var[2] <- round((summary(aov(Model))[[1]][2])*100/(sum(summary(aov(Model))[[1]][2])-summary(aov(Model))[[1]][length(summary(aov(Model))[[1]][,2]),2]))
  table_var[length(summary(aov(Model))[[1]][,2]),2] <- NA
  names(table_var) <- c("% variance",paste0("% variance of explained (", 100 - table_var[length(summary(aov(Model))[[1]][,2]),1],"%)"))
  names(table_var[2]) <- paste0("% variance of explained (",round(100 - table_var[length(parameters),1],1),"%)")
  print.data.frame(table_var)

  table_var <- summary(aov(Model))[[1]][1]
  table_var[1] <- row.names(table_var)
  table_var[2] <- summary(aov(Model))[[1]][1]
  table_var[3] <- round(summary(aov(Model))[[1]][2]*100/sum(summary(aov(Model))[[1]][2]),1)
  table_var[4] <- round((summary(aov(Model))[[1]][2])*100/(sum(summary(aov(Model))[[1]][2])-summary(aov(Model))[[1]][length(summary(aov(Model))[[1]][,2]),2]))
  table_var[length(summary(aov(Model))[[1]][,2]),4] <- NA
  table_var[5] <- summary(aov(Model))[[1]][5]
  table_var[6] <- summary(aov(Model))[[1]][5]
for ( k in 1:(dim(table_var[5])[1]-1)) {

  if(summary(aov(Model))[[1]][k,5]< 1) {table_var[k,5] <- "< 1"
  table_var[k,6] <- " "}

  if(summary(aov(Model))[[1]][k,5]< 0.1) {table_var[k,5] <- "< 0.1"
  table_var[k,6] <- "."}

  if(summary(aov(Model))[[1]][k,5]< 0.05) {table_var[k,5] <- "< 0.05"
  table_var[k,6] <- "*"}

  if(summary(aov(Model))[[1]][k,5]< 0.01) {table_var[k,5] <- "< 0.01"
  table_var[k,6] <- "**"}

  if(summary(aov(Model))[[1]][k,5]< 0.001) {table_var[k,5] <- "< 0.001"
  table_var[k,6] <- "***"}
}


  names(table_var) <- c("Effect", "Df",
                        "% variance",
                        paste0("% of explained (", 100 - table_var[length(summary(aov(Model))[[1]][,2]),3],"%)"),
                        "P-value",
                        "signif.")

  if (summary == TRUE) {
    print(summary(Model))
    res1 <- resid(Model)
    fit1 <- fitted(Model)
    par(mfrow = c(2, 2))
    hist(res1)
    plot(fit1, res1)
    qqnorm(res1)
    qqline(res1)
    plot(Model, 4)
  }
  return(list(Model, summary(aov(Model)), table_var))
}

