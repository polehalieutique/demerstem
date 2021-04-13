#' Abundance-GLM modeling process
#'
#' \code{model_ai_plus} runs the actions for the GLM modeling process on abundance data.
#'
#' @param   tab             input dataset table
#' @param   esp             exact name of the studied species
#' @param   effort          "auto" for an automatic selection of the effort parameter or manual selection ex "duree_peche","nombre_operation","nb_jour_peche", "nb_sorties", "surface_chalutee"
#' @param   title           fraction of the title in the plots
#' @param   list_param      list of the tested parameters
#' @param   var_eff_list    list of the possible fishing effort column
#' @param   espece_id       exact name of the column indicating the species
#' @param   catch_col       exact name of the column indicating the catches
#' @param   interactions    FALSE by default. IF TRUE, print the interactions plots
#' @param   limit           percentage representing the limit value under which the modality is removed
#' @param   formula_select  if "auto", the function select which formula as the lowest AIC. Else, run the selected formula.
#'
#'
#' @examples
#'  data(tableau_sc)
#'  model_ai_plus(tableau_sc, esp = "PSEUDOTOLITHUS ELONGATUS", effort = "auto", title = "SC", list_param = c("annee", "saison", "strate"), var_eff_list=c("surface_chalutee"), espece_id='nom_taxonomique', catch_col='total_capture', interactions =FALSE, limit=0.001, formula_select = "log(i_ab+0.0001) ~ strate + annee + saison")
#'
#' @export

model_ai_plus <- function(tab, esp, effort, title, list_param,  var_eff_list, espece_id, catch_col, interactions=FALSE, limit , formula_select){
  print("SOUS-MODELE ABONDANCE")
  tableau_pres <- table_pres_abs(tab, effort, esp, list_param,  var_eff_list, espece_id, catch_col, limit)
  tableau_ab <- filter(tableau_pres, presence==1)
  print(param_use(tableau_ab, list_param))
  param <- param_use(tableau_ab, list_param)
  print(lapply(param, moda_facto, tab=as.data.frame(tableau_ab), title))

  print(evo_an(tableau_ab, title))
  print(lapply(param, evo_facto, tab=as.data.frame(tableau_ab), title))

  for (i in 1:length(param)){
    tableau_ab[,param[i]] <- as.factor(tableau_ab[,param[i]])
    tableau_ab[,param[i]] <- droplevels(tableau_ab[,param[i]])
    contrasts(tableau_ab[,param[i]]) <- contr.sum(levels(tableau_ab[,param[i]]))
  }

  glm_indice_ab <- glm_ai_plus(tableau_ab, param, formula_select)

  VAR <- var(residuals(glm_indice_ab))
  vect_param <- c(attr(glm_indice_ab$terms, "term.label"))
  table_interact <- c()
  table_ab <- as.data.frame(coef(summary(glm_indice_ab)))
  for (j in 1:length(vect_param)){
    table_tempo <- as.data.frame(dummy.coef(glm_indice_ab)[j+1])
    table_tempo$namemodality <- rownames(table_tempo)
    rownames(table_tempo) <- NULL
    table_tempo$variable <- vect_param[j]
    colnames(table_tempo) <- c("estimates", "modality", "variable")
    if (as.numeric(gregexpr(pattern =':',as.character(attr(glm_indice_ab$term, "term.labels"))[j]))>0){
      table_tempo <- table_tempo %>% filter(estimates != 0)
    }
    table_tempo$corrected_estimates <- table_tempo$estimates + table_ab[1,1]
    table_tempo$corrected_estimates <- exp(table_tempo$corrected_estimates + 0.5*VAR)

    if (as.numeric(gregexpr(pattern =':',as.character(attr(glm_indice_ab$term, "term.labels"))[j]))<0){
      table_tempo$modality <- as.factor(table_tempo$modality)
      levels(table_tempo$modality) <- levels(tableau_ab[,vect_param[j]])
      print(ggplot(table_tempo) + geom_bar(aes(x=modality, y=corrected_estimates), stat="identity", color = "black", fill = "white") + ylab("Estimateur") + ggtitle(paste(vect_param[j], "pour pres/abs")) + theme(axis.text.x = element_text(angle = 35)))
    }

    table_interact <- rbind(table_interact, table_tempo)

    if (interactions == TRUE){
      object <- gregexpr(pattern =':',as.character(attr(glm_indice_ab$term, "term.labels"))[j])
      if (length(object)>0){
        if(as.numeric(object)>0){
          interact_variables <- strsplit(as.character(attr(glm_indice_ab$term, "term.labels"))[j], ":")
          variable1 <- interact_variables[[c(1,1)]]
          variable2 <- interact_variables[[c(1,2)]]

          formula_interact <- paste(variable1,"*",variable2)
          inter_glm <- effect(formula_interact, glm_indice_ab, se=TRUE) #marche pas annee:saison?!
          inter_glm<-as.data.frame(inter_glm)
          inter_glm[,1] <- factor(inter_glm[,1], levels=levels(tableau_ab[,variable1]))
          inter_glm[,2] <- factor(inter_glm[,2], levels=levels(tableau_ab[,variable2]))

          plot_inter_glm<-ggplot(data=inter_glm, aes(x=inter_glm[,1], y=fit, group=inter_glm[,2]))+
            geom_line(size=2, aes(color=inter_glm[,2]))+
            geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill=inter_glm[,2]),alpha=.2)+
            ylab("Predicted Ppres")+
            xlab(variable1)+
            ggtitle(paste("First Interaction Plot for", attr(glm_indice_ab$term, "term.labels"))[j])+
            theme_bw()+
            theme(text = element_text(size=12),
                  legend.text = element_text(size=12),
                  legend.direction = "horizontal",
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  legend.position="top")
          print(plot_inter_glm)

          plot_inter_glm2 <- ggplot(data=inter_glm, aes(x=inter_glm[,1], y=fit, fill=inter_glm[,2])) +
            geom_bar(stat="identity", position=position_dodge()) +
            ylab("Predicted Ppres")+
            xlab(variable1)+
            ggtitle(paste("Second Interaction Plot for", attr(glm_indice_ab$term, "term.labels"))[j])+
            theme_bw()
          print(plot_inter_glm2)
        }
      }
    }
  }
  return(table_interact)
}

