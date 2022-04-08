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
#' @param   force_interaction Factorial plan can be incomplete, thus Anova type III won't work. Allow to coerce an examination of interactions by using Anova type I.

#' @examples
#' data(tableau_sc)
#' glm_pres <- model_pres_abs(tableau_sc, esp="PSEUDOTOLITHUS ELONGATUS", effort="auto", title="SC", list_param=c("annee", "saison", "strate"), var_eff_list=c("surface_chalutee"), espece_id='nom_taxonomique', catch_col='total_capture', interactions = FALSE, limit=0.0001, formula_select = "presence ~ strate + annee + saison")
#' @export

model_pres_abs <- function(tab, esp, title, list_param, var_eff_list, espece_id, catch_col, interactions = FALSE, limit, formula_select, plot = FALSE, summary = FALSE) {
  list_graph<- list(NULL)
  print("SOUS-MODELE PRESENCE ABSENCE")
  tableau_pres <- table_pres_abs(tab, esp, list_param, var_eff_list, espece_id, catch_col, limit)
  parameters <- list_param
  facteur <- parameters[1]
  if (plot == TRUE){
    tableau_pres$facteur=factor(tableau_pres[,facteur])
    tableau_pres$title <- paste("presence/absence - ", title, "\n", facteur)
    plot_1 <- ggplot(tableau_pres, aes(facteur, fill=factor(presence)))+geom_bar() + facet_grid(~title) + theme_nice() +
            theme(axis.text.x = element_text(angle = 60, size=9),
                  plot.title = element_text(size=11, face="bold"),
                  axis.title.x = element_blank(),
                  axis.title.y = element_text(size=9),
                  legend.title = element_text(size=10),
                  legend.text = element_text(size=10))
    print(plot_1)
    list_graph[[1]] <- list(plot_1)
    plot_2 <- ggarrange(plotlist=lapply(parameters, pres_facto, tab=tableau_pres, title),
                    ncol=2, nrow=2, common.legend = TRUE, legend = "bottom")
    print(plot_2)
    list_graph[[length(list_graph) + 1]] <- list(plot_2)
  }

  for (i in 1:length(parameters)){
    tableau_pres[,parameters[i]] <- as.factor(tableau_pres[,parameters[i]])
    tableau_pres[,parameters[i]] <- droplevels(tableau_pres[,parameters[i]])
    contrasts(tableau_pres[,parameters[i]]) <- contr.sum(levels(tableau_pres[,parameters[i]]))
  }

  glm_presabs <- glm_pres_abs(tableau_pres = tableau_pres, parameters, formula_select, summary)
  vect_param <- c(attr(glm_presabs[[1]]$terms, "term.label"))
  table_interact <- c()
  table_pres <- as.data.frame(coef(summary(glm_presabs[[1]])))
  list_plot <- c()
  count <- 0
  for (j in 1:length(vect_param)){
    table_tempo <- as.data.frame(dummy.coef(glm_presabs[[1]])[j +1])
    table_tempo$namemodality <- rownames(table_tempo)
    rownames(table_tempo) <- NULL
    table_tempo$variable <- vect_param[j]
    colnames(table_tempo) <- c("estimates", "modality", "variable")
    if (as.numeric(gregexpr(pattern = ":", as.character(attr(glm_presabs[[1]]$term,"term.labels"))[j])) > 0) {
      table_tempo <- table_tempo %>% filter(estimates != 0)
    }
    table_tempo$corrected_estimates <- table_tempo$estimates + table_pres[1, 1]

    table_tempo$corrected_estimates <- exp(table_tempo$corrected_estimates)/(1+exp(table_tempo$corrected_estimates))

    if (as.numeric(gregexpr(pattern =':',as.character(attr(glm_presabs[[1]]$term, "term.labels"))[j]))<0){
      count <- count + 1
      table_tempo$modality <- as.factor(table_tempo$modality)
      table_tempo$modality <- ordered(table_tempo$modality, levels = levels(tableau_pres[,vect_param[j]]))
      table_tempo$title <- paste0("Estimation - pres/abs - ", title, "\n", vect_param[j])

      list_plot[[j]] <- local({
        tempo <- j
        x1 <- ggplot(table_tempo) +
          geom_bar(aes(x=modality, y=corrected_estimates), stat="identity", color = "black", fill = "grey") +
          ylab("% presence") +
          facet_grid(~title) +
          theme_bw() +
          theme(strip.text.x = element_text(face = "bold")) +
          theme(axis.text.x = element_text(angle = 60, size=9),
                plot.title = element_text(size=10, face="bold"),
                axis.title.x = element_blank(),
                axis.title.y = element_text(size=9),
                legend.title = element_text(size=10),
                legend.text = element_text(size=10)) +
          labs(x = vect_param[tempo])

      })
    }
    if(plot==TRUE){
      if(vect_param[j]==facteur){
        plot_3 <- ggplot(table_tempo) +
               geom_bar(aes(x=modality, y=corrected_estimates), stat="identity", color = "black", fill = "grey") +
               ylab("% presence") +
               facet_grid(~title) +
               theme_nice() +
               theme(axis.text.x = element_text(angle = 60, size=9),
                     plot.title = element_text(size=10, face="bold"),
                     axis.title.x = element_blank(),
                     axis.title.y = element_text(size=9),
                     legend.title = element_text(size=10),
                     legend.text = element_text(size=10)) +
               labs(x = vect_param[j])

        list_graph[[length(list_graph) + 1]] <- list(plot_3)
      }
    }
    table_tempo$title <- NULL
    table_interact <- rbind(table_interact, table_tempo)

    #Graph
    if (interactions == TRUE){
      object <- gregexpr(pattern =':',as.character(attr(glm_presabs[[1]]$term, "term.labels"))[j])
      if (length(object)>0){
        if(as.numeric(object)>0){
          interact_variables <- strsplit(as.character(attr(glm_presabs[[1]]$term, "term.labels"))[j], ":")
          variable1 <- interact_variables[[c(1,1)]]
          variable2 <- interact_variables[[c(1,2)]]

          formula_interact <- paste(variable1,"*",variable2)
          inter_glm <- effect(formula_interact, glm_presabs[[1]], se=T) # Look after this sen may be wrong and be an issue /!|!\
          inter_glm<-as.data.frame(inter_glm)
          inter_glm[,1] <- factor(inter_glm[,1], levels=levels(tableau_pres[,variable1]))
          inter_glm[,2] <- factor(inter_glm[,2], levels=levels(tableau_pres[,variable2]))
          inter_glm$title <- paste("Interaction - pres/abs \n ", attr(glm_presabs[[1]]$term, "term.labels")[j])

          plot_4 <- ggplot(data=inter_glm, aes(x=inter_glm[,1], y=fit, group=inter_glm[,2])) +
            geom_line(size=2, aes(color=inter_glm[,2]))+
            geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill=inter_glm[,2]),alpha=.2)+
            facet_grid(~title) +
            labs(x = variable1,
                 y = "Predicted % of presence",
                 color = variable2, fill = variable2) +
            theme_nice()+
            #theme(text = element_text(size=12),
            #legend.text = element_text(size=12),
            #legend.direction = "horizontal",
            #panel.grid.major = element_blank(),
            #panel.grid.minor = element_blank(),
            #legend.position="top")
            theme(axis.text.x = element_text(angle = 60, size=8),
                  plot.title = element_text(size=10, face="bold"),
                  axis.title.x = element_blank(),
                  axis.title.y = element_text(size=8),
                  legend.title = element_text(size=8),
                  legend.text = element_text(size=8))

          print(plot_4)

          list_graph[[length(list_graph) + 1]] <- list(plot_4)

          plot_5 <- ggplot(data=inter_glm, aes(x=inter_glm[,1], y=fit, fill=inter_glm[,2])) +
            geom_bar(stat="identity", position=position_dodge()) +
            labs(x = variable1,
                 y = "Predicted % of presence",
                 color = variable2, fill = variable2) +
            facet_grid(~title) +
            theme_nice() + theme(axis.text.x = element_text(angle = 30, size=8),
                               plot.title = element_text(size=10, face="bold"),
                               axis.title.x = element_blank(),
                               axis.title.y = element_text(size=8),
                               legend.title = element_text(size=8),
                               legend.text = element_text(size=8))
          print(plot_5)
          list_graph[[length(list_graph) + 1]] <- list(plot_5)
        }
      }
    }
  }
  plot_6 <- ggarrange(plotlist=list_plot, ncol=2, nrow=2, common.legend = TRUE, legend = "bottom")
  if (plot ==T) {print(plot_3)}
  print(plot_6)
  list_graph[[length(list_graph) + 1]] <- list(plot_6)
  return(list(table_interact, glm_presabs, list_graph))
}
