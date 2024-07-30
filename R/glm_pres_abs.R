#' Fit a Generalized Linear Models on presence/absence data
#'
#' \code{glm_pres_abs} returns a presence/absence GLM model. It can run a StepAIC if specified. It runs the GLMs, plot the residuals analysis graphs and return the GLM outputs.
#'
#' @param tableau_pres     table of presence and absence data, coming as the output of table_pres_abs
#' @param parameters      list of parameters to test
#' @param formula_select  if "auto", the function select which formula as the lowest AIC. Else, run the selected formula.
#' @param summary         To show residuals plots.
#' @param type            To study marginal effects with Anova type III in case of interaction
#'
#'
#' @return \code{glm_pres_abs} can either return the best GLM model based on AIC comparison, or return the outputs of the GLM based on the formula given in \emph{formula_select}
#'
#' @examples
#' data(tableau_sc)
#' list_param <- c("annee", "saison")
#' table_ex <- table_pres_abs(tableau_sc, esp="PSEUDOTOLITHUS ELONGATUS", list_param=c("annee", "saison"), espece_id='nom_taxonomique', var_eff_list=c("surface_chalutee"), catch_col='total_capture', limit=0.0001)
#' param <- param_use(table_ex, list_param)
#'
#' for (i in 1:length(param)){
#' table_ex[,param[i]] <- as.factor(table_ex[,param[i]])
#' table_ex[,param[i]] <- droplevels(table_ex[,param[i]])
#' contrasts(table_ex[,param[i]]) <- contr.sum(levels(table_ex[,param[i]]))
#' }
#'
#' glm_pres_abs(table_ex, param, formula_select = "auto")
#'
#' @export


glm_pres_abs <- function (tableau_pres, parameters, formula_select, summary = FALSE, type = 2)
{

  # parameters as factors
  lapply(tableau_pres[,parameters], as.factor)
  # Preparation of factors
  a <- paste(parameters[1], "+")
  for (i in 2 : (length(parameters)-1)){
    a <- paste(a, parameters[i], "+")}
  a <- paste(a, parameters[length(parameters)])
  formula <- paste("presence ~", a)
  formula_inter <- paste0("presence ~ (", a, ")^2") #order 2 interactions


  if (formula_select == "auto"){
    cat("
    ---------------------------------------------------------


        stepAIC selection starting with the full model :


    ---------------------------------------------------------

        ")
    model_sature <- glm(formula = formula_inter, family=binomial, data=tableau_pres)
    test_sature <- stepAIC(model_sature, scope=(lower=~1)) #trace=TRUE

    cat("
    ---------------------------------------------------------


        stepAIC selection starting with the minimal model :


    ---------------------------------------------------------

        ")

    model_cst <- glm(formula = presence ~ 1, family=binomial, data=tableau_pres)
    test_cst <- stepAIC(model_cst, scope=(upper=paste("~ (", a,")^2"))) #trace=TRUE

    print(anova(test_sature, test_cst))

    if (AIC(test_sature) < AIC(test_cst)){
      Model <- test_sature
      formula <- test_sature$formula
    } else if (AIC(test_sature) == AIC(test_cst)) {
      Model <- test_cst
      formula <- test_cst$formula
    } else {
      Model <- test_cst
      formula <- test_cst$formula
    }

  }

  else {
    formula <- formula_select
  }


cat("
#------------------------------------------------#



                    OUTPUTS



#------------------------------------------------#

")

    # Print du modele sélectionné
    if (formula_select == "auto"){
      cat("The statiscally selected model is :")

    } else {
      cat("The model you have selected :")

    }
cat("

    ")
  Model <- glm(as.formula(formula), family=binomial, data=tableau_pres)

  print(formula)

  cat("

    ")
  cat(paste0( "AIC = ",round(AIC(Model),3)))

  cat("

    ")

  if (type == 3){
    ANOVA <- Anova(Model, type = 3, test = "F")
    }
  else {
    ANOVA <- Anova(Model, type = 2, test = "F")
  }

  print(ANOVA)

  cat("% of variability explained by each effect : ")
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

  if (formula != "presence ~ 1"){
  names(table_var) <- c("Effect", "Df",
                        "% variance",
                        paste0("% variance of explained (", 100 - table_var[s,3],"%)"),
                        "P-value",
                        "signif.")}

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
  return(list(Model, ANOVA, table_var))
}

