#' Presence/Absence-GLM modeling process
#'
#' \code{model_pres_abs} runs the actions for the GLM modeling process on presence/absence data.
#'
#' @param   tab             input dataset table
#' @param   esp             exact name of the studied species
#' @param   title           fraction of the title in the plots
#' @param   list_param      list of the tested parameters
#' @param   var_eff_list    list of the possible fishing effort column
#' @param   espece_id       exact name of the column indicating the species
#' @param   catch_col       exact name of the column indicating the catches
#' @param   interactions    FALSE by default. IF TRUE, print the interactions plots
#' @param   limit           percentage representing the limit value under which the modality is removed
#' @param   formula_select  if "auto", the function select which formula as the lowest AIC. Else, run the selected formula.
#' @param   plot            FALSE by default. If TRUE, print the data presentation histograms
#' @param   summary         FALSE by default. If TRUE, print the summary of the selected GLM


#' @examples
#' data(tableau_sc)
#' glm_pres <- model_pres_abs(tableau_sc, esp="PSEUDOTOLITHUS ELONGATUS", effort="auto", title="SC", list_param=c("annee", "saison", "strate"), var_eff_list=c("surface_chalutee"), espece_id='nom_taxonomique', catch_col='total_capture', interactions = FALSE, limit=0.0001, formula_select = "presence ~ strate + annee + saison")
#' @export

model_pres_abs <- function(tab, esp, title, list_param,  var_eff_list, espece_id, catch_col, interactions=FALSE, limit, formula_select, plot = FALSE, summary = FALSE){
  print("SOUS-MODELE PRESENCE ABSENCE")
  tableau_pres <- table_pres_abs(tab, esp, list_param, var_eff_list, espece_id, catch_col, limit)
  param <- param_use(tableau_pres, list_param)
  print(param_use(tableau_pres, list_param))
  if (plot == TRUE){
    #print(lapply(param, pres_facto, tab=tableau_pres, title))
    print(ggarrange(plotlist=lapply(param, pres_facto, tab=tableau_pres, title),
                    ncol=2, nrow=2, common.legend = TRUE, legend = "bottom"))
  }
  #ceiling(length(param)/2)

  for (i in 1:length(param)){
    tableau_pres[,param[i]] <- as.factor(tableau_pres[,param[i]])
    tableau_pres[,param[i]] <- droplevels(tableau_pres[,param[i]])
    contrasts(tableau_pres[,param[i]]) <- contr.sum(levels(tableau_pres[,param[i]]))
  }

  glm_presabs <- glm_pres_abs(tableau_pres, param, formula_select, summary)

  vect_param <- c(attr(glm_presabs$terms, "term.label"))
  table_interact <- c()
  table_pres <- as.data.frame(coef(summary(glm_presabs)))
  list_plot <- c()
  count <- 0
  for (j in 1:length(vect_param)){
    table_tempo <- as.data.frame(dummy.coef(glm_presabs)[j+1])
    table_tempo$namemodality <- rownames(table_tempo)
    rownames(table_tempo) <- NULL
    table_tempo$variable <- vect_param[j]
    colnames(table_tempo) <- c("estimates", "modality", "variable")
    if (as.numeric(gregexpr(pattern =':',as.character(attr(glm_presabs$term, "term.labels"))[j]))>0){
      table_tempo <- table_tempo %>% filter(estimates != 0)
    }
    table_tempo$corrected_estimates <- table_tempo$estimates + table_pres[1,1]

    #table_tempo$corrected_estimates <- exp(table_tempo$corrected_estimates)/(1+exp(table_tempo$corrected_estimates))

    if (as.numeric(gregexpr(pattern =':',as.character(attr(glm_presabs$term, "term.labels"))[j]))<0){
      count <- count + 1
      table_tempo$modality <- as.factor(table_tempo$modality)
      table_tempo$modality <- ordered(table_tempo$modality, levels = levels(tableau_pres[,vect_param[j]]))
      #levels(table_tempo$modality) <- levels(tableau_pres[,vect_param[j]])
      #print(ggplot(table_tempo) + geom_bar(aes(x=modality, y=corrected_estimates), stat="identity", color = "black", fill = "white") + ylab("Estimateur") + ggtitle(paste(vect_param[j], "pour pres/abs")) + theme(axis.text.x = element_text(angle = 35)))

      list_plot[[j]] <- local({
        tempo <- j
        x1 <- ggplot(table_tempo) + geom_bar(aes(x=modality, y=corrected_estimates), stat="identity", color = "black", fill = "white") + ylab("Estimateur") + ggtitle(paste(vect_param[tempo], "pour pres/abs")) + theme(axis.text.x = element_text(angle = 60, size=7), plot.title = element_text(size=10, face="bold"), axis.title.x = element_text(size=8), axis.title.y = element_text(size=8), legend.title = element_text(size=8), legend.text = element_text(size=8))
      })
    }

    table_interact <- rbind(table_interact, table_tempo)

    #GRAPH
    if (interactions == TRUE){
      object <- gregexpr(pattern =':',as.character(attr(glm_presabs$term, "term.labels"))[j])
      if (length(object)>0){
        if(as.numeric(object)>0){
          interact_variables <- strsplit(as.character(attr(glm_presabs$term, "term.labels"))[j], ":")
          variable1 <- interact_variables[[c(1,1)]]
          variable2 <- interact_variables[[c(1,2)]]

          formula_interact <- paste(variable1,"*",variable2)
          inter_glm <- effect(formula_interact, glm_presabs, se=TRUE) #marche pas annee:saison?!
          inter_glm<-as.data.frame(inter_glm)
          inter_glm[,1] <- factor(inter_glm[,1], levels=levels(tableau_pres[,variable1]))
          inter_glm[,2] <- factor(inter_glm[,2], levels=levels(tableau_pres[,variable2]))

          plot_inter_glm<-ggplot(data=inter_glm, aes(x=inter_glm[,1], y=fit, group=inter_glm[,2]))+
            geom_line(size=2, aes(color=inter_glm[,2]))+
            geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill=inter_glm[,2]),alpha=.2)+
            ylab("Predicted Ppres")+
            xlab(variable1)+
            ggtitle(paste("First Interaction Plot for", attr(glm_presabs$term, "term.labels"))[j])+
            theme_bw()+
            #theme(text = element_text(size=12),
            #legend.text = element_text(size=12),
            #legend.direction = "horizontal",
            #panel.grid.major = element_blank(),
            #panel.grid.minor = element_blank(),
            #legend.position="top")
            theme(axis.text.x = element_text(angle = 60, size=8), plot.title = element_text(size=10, face="bold"), axis.title.x = element_text(size=8), axis.title.y = element_text(size=8), legend.title = element_text(size=8), legend.text = element_text(size=8))
          print(plot_inter_glm)

          plot_inter_glm2 <- ggplot(data=inter_glm, aes(x=inter_glm[,1], y=fit, fill=inter_glm[,2])) +
            geom_bar(stat="identity", position=position_dodge()) +
            ylab("Predicted Ppres")+
            xlab(variable1)+
            ggtitle(paste("Second Interaction Plot for", attr(glm_presabs$term, "term.labels"))[j])+
            theme_bw() + theme(axis.text.x = element_text(angle = 30, size=8), plot.title = element_text(size=10, face="bold"), axis.title.x = element_text(size=8), axis.title.y = element_text(size=8), legend.title = element_text(size=8), legend.text = element_text(size=8))
          print(plot_inter_glm2)
          #print(ggarrange(plot_inter_glm, plot_inter_glm2, ncol=2, common.legend = TRUE, legend = "bottom"))
        }
      }
    }
  }
  print(ggarrange(plotlist=list_plot, ncol=2, nrow=2, common.legend = TRUE, legend = "bottom"))
  #ceiling(count/2)
  return(table_interact)
}
