#' Recalculation of year effects in case of interaction
#'
#' \code{delta_glm} realize the delta-coupling using the 2 precedents GLMs outputs, extract the year factor, calculate the AI and display the plots.
#'
#'
#' @param data_table         : outputs table from either model_pres_abs() or model_ai_plus
#' @param proportion_table   : table giving the respective proportion of each modality
#' @param glm_type           : either "pres/abs" or "abundance"
#' @param tab                : initial data used for the glm modeling
#'
#' @examples
#' data(tableau_sc)
#' glm_pres_abs <- model_pres_abs(tableau_sc, esp="PSEUDOTOLITHUS ELONGATUS", effort="auto", title="SC", list_param=c("annee", "saison", "strate"), var_eff_list=c("surface_chalutee"), espece_id='nom_taxonomique', catch_col='total_capture', interactions = TRUE, limit=0.0001, formula_select = "presence ~ strate * annee + saison")
#' prof <- c("0-5m", "5-10m", "10-15m", "15-30m", ">30m")
#' proportion <- c(0.10, 0.15, 0.15, 0.30, 0.30)
#' repartition_prof <- as.data.frame(cbind(prof, proportion))
#' repartition_prof$proportion <- as.numeric(repartition_prof$proportion)
#' rownames(repartition_prof) <- repartition_prof$prof
#'
#' glm_pres_sc_recalcule <- interaction_calculation(data_table=glm_pres_abs, proportion_table=repartition_prof, glm_type="pres/abs", tab=tableau_sc, effort, esp, list_param, var_eff_list, espece_id, catch_col, limit, formula_select="presence ~ strate*annee + saison")
#' delta_glm(glm_pres_abs, glm_abundance, title="Scientific campaign GIN BOBO", type = "SC")
#' @export


interaction_calculation <- function(data_table, proportion_table, glm_type, tab, esp, list_param, var_eff_list, espece_id, catch_col, limit, formula_select){
  #On récupère l'intercept
  tableau_pres <- table_pres_abs(tab, esp, list_param, var_eff_list, espece_id, catch_col, limit)
  tableau_ab <- filter(tableau_pres, i_ab>0)
  param <- param_use(tableau_pres, list_param)

  if (glm_type == "pres/abs"){

    for (i in 1:length(param)){

      tableau_pres[,param[i]] <- as.factor(tableau_pres[,param[i]])
      tableau_pres[,param[i]] <- droplevels(tableau_pres[,param[i]])
      contrasts(tableau_pres[,param[i]]) <- contr.sum(levels(tableau_pres[,param[i]]))
    }

    glm_presabs <- glm_pres_abs(tableau_pres, param, formula_select)
    table_pres <- as.data.frame(coef(summary(glm_presabs)))
    intercept <- table_pres[1, 1]

  }

  if (glm_type == "abundance"){
    for (i in 1:length(param)){
      tableau_ab[,param[i]] <- as.factor(tableau_ab[,param[i]])
      tableau_ab[,param[i]] <- droplevels(tableau_ab[,param[i]])
      contrasts(tableau_ab[,param[i]]) <- contr.sum(levels(tableau_ab[,param[i]]))
    }

    glm_indice_ab <- glm_ai_plus(tableau_ab, param, formula_select)
    VAR <- var(residuals(glm_indice_ab))
    table_ab <- as.data.frame(coef(summary(glm_indice_ab)))
    intercept <- table_ab[1,1]
  }

  #On récupère l'intercept

  rownames(data_table) <- data_table$modality

  data_table$estimates <- data_table$estimates + intercept
  tab_IA <- as.data.frame(c(levels(droplevels(tab$annee))))
  colnames(tab_IA) <- c("annee")

  for (i in 1:nrow(tab_IA)){
    fixed_estim <- data_table[tab_IA[i,1],1] #4
    interact_estim <- 0
    P_sum <- 0
    for (j in 1:nrow(data_table)){
      if(length((strsplit(as.character(data_table[j,2]), ":"))[[1]])>1){
        list1 <- strsplit(as.character(data_table[j,2]), ":")
        for (t in 1:nrow(proportion_table)){
          if (list1[[c(1,1)]]==proportion_table[t,1]){
            k <- 1
          }
          if (list1[[c(1,2)]]==proportion_table[t,1]){
            k <- 2
          }
        }
        if (list1[[c(1,1)]] == tab_IA[i,1] || list1[[c(1,2)]] == tab_IA[i,1]){
          interact_estim <- interact_estim + (data_table[j,1]*proportion_table[list1[[c(1,k)]], 2]) # data_table[j,4]
          P_sum <- P_sum + proportion_table[list1[[c(1,k)]], 2]
        }
      }
    }
    final_estim <- sum(fixed_estim, (interact_estim/P_sum), na.rm=T)
    tab_IA[i,2] <- final_estim
  }

  if (glm_type == "abundance"){
    tab_IA$V2 <- exp(tab_IA$V2 + 0.5*VAR)
  }
  if (glm_type == "pres/abs"){
    tab_IA$V2 <- exp(tab_IA$V2)/(1+exp(tab_IA$V2))}
  #Plot
  colnames(tab_IA) <- c("annee", "estim")
  g1 <- ggplot(tab_IA, aes(x = annee, y=estim)) +
    geom_bar(stat="identity", color = "black", fill = "white") +
    theme(axis.text.x = element_text(angle = 60, size=8), plot.title = element_text(size=10, face="bold"), axis.title.x = element_text(size=8), axis.title.y = element_text(size=8), legend.title = element_text(size=8), legend.text = element_text(size=8))+
    ggtitle(label = "IA recalculé pour données pres/abs") + ylab("Predicted estim.")
  print(g1)
  colnames(tab_IA) <- c("modality", "corrected_estimates")
  return(tab_IA)
}
