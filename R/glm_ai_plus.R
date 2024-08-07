#' Fit a Generalized Linear Models on abundance data
#'
#' \code{glm_ai_plus} returns a GLM model on abundance data. It can run a StepAIC if specified. It runs the GLMs, plot the residuals analysis graphs and return the GLM outputs.
#'
#' @param tableau_ab             table of abundance data, extracted from the output of table_pres_abs
#' @param parameters      list of parameters to test
#' @param formula_select  if "auto", the function select which formula as the lowest AIC. Else, run the selected formula.
#' @param summary         To show residuals plots.
#' @param type            To study marginal effects with Anova type III in case of interaction
#'
#' @return \code{glm_ai_plus} can either return the best GLM model based on AIC comparison, or return the outputs of the GLM based on the formula given in \emph{formula_select}
#'
#' @examples
#' data(tableau_sc)
#' list_param <- c("annee", "saison")
#' table_ex <- table_pres_abs(tableau_sc, esp="PSEUDOTOLITHUS ELONGATUS", list_param=c("annee", "saison", "strate"), espece_id='nom_taxonomique', var_eff_list=c("surface_chalutee"), catch_col='total_capture', limit=0.0001)
#' table_ex_abundance <- filter(table_ex, presence==1)
#' param <- param_use(table_ex_abundance, list_param)
#' for (i in 1:length(param)){
#' table_ex_abundance[,param[i]] <- as.factor(table_ex_abundance[,param[i]])
#' table_ex_abundance[,param[i]] <- droplevels(table_ex_abundance[,param[i]])
#' contrasts(table_ex_abundance[,param[i]]) <- contr.sum(levels(table_ex_abundance[,param[i]]))
#' }
#' glm_ai_plus(table_ex_abundance, param, formula_select = "auto")
#' @export


glm_ai_plus <- function(tableau_ab, parameters, formula_select, summary=FALSE, type = 2){
  #passer les parametres en facteur

  lapply(tableau_ab[,parameters], as.factor)
  #ecriture formule
  a <- paste(parameters[1], "+")
  for (i in 2 : (length(parameters)-1)){
    a <- paste(a, parameters[i], "+")}
  a <- paste(a, parameters[length(parameters)])
  formula_log <- paste("log(i_ab) ~", a)
  #test des interactions ordre2
  formula_log_inter <- paste("log(i_ab) ~ (", a,")^2")

  if (formula_select == "auto"){
    print("stepAIC selection starting with the full model :")
    model_sature <- glm(formula = formula_log_inter, family=gaussian, data=tableau_ab)
    test_sature <- stepAIC(model_sature, scope=(lower=~1)) #trace=TRUE
    print("stepAIC selection starting with the minimal model :")
    model_cst <- glm(formula = log(i_ab) ~ 1, family=gaussian, data=tableau_ab)
    test_cst <- stepAIC(model_cst, scope=(upper=paste("~ (", a,")^2"))) #trace=TRUE

    print(anova(test_sature, test_cst))

    if (AIC(test_sature) < AIC(test_cst)) {
      Model <- test_sature
      formula <- test_sature$formula
    }
    else {
      Model <- test_cst
      formula <- test_cst$formula
    }
  }
  else {
    formula <- formula_select
  }

  # Print du modele sélectionné
  if (formula_select == "auto"){
    print("The selected model by statistical optimisation is:")
  } else {
    print("The model you selected:")
  }
  print(formula)

  # resume modele et graphiques residus
  Model <- glm(as.formula(formula) , family=gaussian, data=tableau_ab)
  print(formula)
  print(paste0( "AIC = ",round(AIC(Model),3)))

  if (type == 3){
    ANOVA <- Anova(Model, type = 3, test = "F")
  }
  else {
    ANOVA <- Anova(Model, type = 2, test = "F")
  }

  print(ANOVA)

  print("% of variability explained by each effect : ")
  table_var <- 100 * round(ANOVA[1]/sum(ANOVA[1]),3)
  table_var[2] <- round(ANOVA[1]*100/(sum(ANOVA[1])-tail(ANOVA[1],1)[[1]]))
  s <- length(table_var[[1]])
  table_var[s,2] <- NA
  names(table_var) <- c("% variance",
                        paste0("% variance of explained (", 100 - table_var[s,1],"%)"))
  print.data.frame(table_var)



  table_var <- ANOVA[1]
  table_var[1] <- row.names(table_var)
  table_var[2] <- ANOVA[2]
  table_var[3] <- 100 * round(ANOVA[1]/sum(ANOVA[1]),3)
  table_var[4] <- round(ANOVA[1]*100/(sum(ANOVA[1])-tail(ANOVA[1],1)[[1]]))

  table_var[s,4] <- NA
  table_var[5] <- ANOVA[4]
  table_var[6] <- ANOVA[4]

  if (formula != "presence ~ 1"){

    for ( k in 1:(dim(table_var[5])[1]-1)) {

      if(ANOVA[k,4]< 1) {table_var[k,5] <- "< 1"
      table_var[k,6] <- " "}

      if(ANOVA[k,4]< 0.1) {table_var[k,5] <- "< 0.1"
      table_var[k,6] <- "."}

      if(ANOVA[k,4]< 0.05) {table_var[k,5] <- "< 0.05"
      table_var[k,6] <- "*"}

      if(ANOVA[k,4]< 0.01) {table_var[k,5] <- "< 0.01"
      table_var[k,6] <- "**"}

      if(ANOVA[k,4]< 0.001) {table_var[k,5] <- "< 0.001"
      table_var[k,6] <- "***"}
    }
  }


  names(table_var) <- c("Effect", "Df",
                        "% variance",
                        paste0("% of explained (", 100 - table_var[length(summary(aov(Model))[[1]][,2]),3],"%)"),
                        "P-value",
                        "signif.")

  if (summary == TRUE) {
    print(summary(Model))
    summary(Model)
    res1 <- resid(Model)
    fit1 <- fitted(Model)
    par(mfrow = c(2, 2))
    hist(res1)
    plot(fit1, res1)
    qqnorm(res1)
    qqline(res1)
    plot(Model, 4)
  }
  return(list(Model, ANOVA, table_var))
}


