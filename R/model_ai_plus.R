#' Abundance-GLM modeling process
#'
#' \code{model_ai_plus} runs the actions for the GLM modeling process on abundance data.
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
#'  data(tableau_sc)
#'  model_ai_plus(tableau_sc, esp = "PSEUDOTOLITHUS ELONGATUS", effort = "auto", title = "SC", list_param = c("annee", "saison", "strate"), var_eff_list=c("surface_chalutee"), espece_id='nom_taxonomique', catch_col='total_capture', interactions =FALSE, limit=0.001, formula_select = "log(i_ab+0.0001) ~ strate + annee + saison")
#'
#' @export

model_ai_plus <- function(tab, esp, title, list_param,  var_eff_list, espece_id, catch_col, interactions=FALSE, limit , formula_select, plot = FALSE, summary = FALSE, force_interaction = FALSE){
  list_graph<- list(NULL)
  print("SOUS-MODELE ABONDANCE")
  tableau_pres <- table_pres_abs(tab, esp, list_param,  var_eff_list, espece_id, catch_col, limit)
  tableau_ab <- filter(tableau_pres, i_ab>0)
  parameters <- list_param
  facteur <- parameters[1]

  if(plot==TRUE){

    tableau_ab$facteur=factor(tableau_ab[,facteur])
    tableau_ab$title <- paste("Observation abundance - ", title, "\n", facteur)
    plot_1 <- ggplot(tableau_ab, aes(facteur))+geom_bar() + facet_grid(~title) + theme_nice() +
      theme(axis.text.x = element_text(angle = 60, size=9),
            plot.title = element_text(size=11, face="bold"),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size=9),
            legend.title = element_text(size=10),
            legend.text = element_text(size=10))
    print(plot_1)
    list_graph[[1]] <- list(plot_1)

    plot_2 <- ggarrange(plotlist=lapply(parameters, moda_facto, tab=tableau_ab, title),
                        ncol=2, nrow=2, common.legend = TRUE, legend = "bottom")
    print(plot_2)
    list_graph[[length(list_graph) + 1]] <- list(plot_2)

    requete <- tableau_ab %>% dplyr::group_by(facteur, annee) %>% dplyr::summarise(mean_ind_ab=mean(i_ab))
    requete$title <- paste("CPUE - ", title, "\n", facteur)

    plot_3 <- ggplot(requete, aes(annee, mean_ind_ab)) +
            geom_point(aes(col=facteur), size=1.5) +
            geom_line(aes(group=facteur, color=facteur), size = 1.2)+
            theme_nice() +
            facet_grid(~title) +
      theme(axis.text.x = element_text(angle = 60, size=9),
            plot.title = element_text(size=11, face="bold"),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size=9),
            legend.title = element_text(size=10),
            legend.text = element_text(size=10)) +
            ylab("mean CPUE") +
            theme(legend.key.size = unit(0.4, "cm"), legend.title = element_text(size=7), legend.text = element_text(size=6))
    list_graph[[length(list_graph) + 1]] <- list(plot_3)
    print(plot_3)
    plot_4 <- ggarrange(plotlist=lapply(parameters, evo_facto, tab=tableau_ab, title),
                    ncol=2, nrow=2)
    list_graph[[length(list_graph) + 1]] <- list(plot_4)
    print(plot_4)
  }

  for (i in 1:length(parameters)){
    tableau_ab[,parameters[i]] <- as.factor(tableau_ab[,parameters[i]])
    tableau_ab[,parameters[i]] <- droplevels(tableau_ab[,parameters[i]])
    contrasts(tableau_ab[,parameters[i]]) <- contr.sum(levels(tableau_ab[,parameters[i]]))
  }

  glm_indice_ab <- glm_ai_plus(tableau_ab = tableau_ab, parameters, formula_select, summary)

  VAR <- var(residuals(glm_indice_ab[[1]]))
  vect_param <- c(attr(glm_indice_ab[[1]]$terms, "term.label"))
  table_interact <- c()
  table_ab <- as.data.frame(coef(summary(glm_indice_ab[[1]])))
  list_plot <- c()
  count <- 0
  for (j in 1:length(vect_param)){
    table_tempo <- as.data.frame(dummy.coef(glm_indice_ab[[1]])[j+1])
    table_tempo$namemodality <- rownames(table_tempo)
    rownames(table_tempo) <- NULL
    table_tempo$variable <- vect_param[j]
    colnames(table_tempo) <- c("estimates", "modality", "variable")
    if (as.numeric(gregexpr(pattern =':',as.character(attr(glm_indice_ab[[1]]$term, "term.labels"))[j]))>0){ # if interaction : other plot
      table_tempo <- table_tempo %>% filter(estimates != 0)
    }
    table_tempo$corrected_estimates <- table_tempo$estimates + table_ab[1,1]
    table_tempo$corrected_estimates <- exp(table_tempo$corrected_estimates + 0.5*VAR)

    if (as.numeric(gregexpr(pattern =':',as.character(attr(glm_indice_ab[[1]]$term, "term.labels"))[j]))<0){ # else : plot the estimation for each modality
      count <- count + 1
      table_tempo$modality <- as.factor(table_tempo$modality)
      table_tempo$modality <- ordered(table_tempo$modality, levels = levels(tableau_ab[,vect_param[j]]))
      table_tempo$title <- paste0("Estimation - abundance - ", title, "\n", vect_param[j])

      list_plot[[j]] <- local({
        tempo <- j
        x1 <- ggplot(table_tempo) +
          geom_bar(aes(x=modality, y=corrected_estimates), stat="identity", color = "black", fill = "grey") +
          ylab("Estimation") +
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
    ## ICI
    if(plot==TRUE){
      if(vect_param[j]==facteur){
        plot_5 <- ggplot(table_tempo) +
          geom_bar(aes(x=modality, y=corrected_estimates), stat="identity", color = "black", fill = "grey") +
          ylab("Estimation") +
          facet_grid(~title) +
          theme_nice() +
          theme(axis.text.x = element_text(angle = 60, size=9),
                plot.title = element_text(size=10, face="bold"),
                axis.title.x = element_blank(),
                axis.title.y = element_text(size=9),
                legend.title = element_text(size=10),
                legend.text = element_text(size=10)) +
          labs(x = vect_param[j])
        list_graph[[length(list_graph) + 1]] <- list(plot_5)
        }
    }

    table_tempo$title <- NULL
    table_interact <- rbind(table_interact, table_tempo)

    if (interactions == TRUE){
      object <- gregexpr(pattern =':',as.character(attr(glm_indice_ab[[1]]$term, "term.labels"))[j])
      if (length(object)>0){
        if(as.numeric(object)>0){
          interact_variables <- strsplit(as.character(attr(glm_indice_ab[[1]]$term, "term.labels"))[j], ":")
          variable1 <- interact_variables[[c(1,1)]]
          variable2 <- interact_variables[[c(1,2)]]

          formula_interact <- paste(variable1,":",variable2)
          inter_glm <- effect(formula_interact, glm_indice_ab[[1]], se=TRUE)
          inter_glm<-as.data.frame(inter_glm)
          inter_glm[,1] <- factor(inter_glm[,1], levels=levels(tableau_ab[,variable1]))
          inter_glm[,2] <- factor(inter_glm[,2], levels=levels(tableau_ab[,variable2]))
          inter_glm$title <- paste("Interaction plot - abundance \n ", attr(glm_presabs[[1]]$term, "term.labels")[j])

          plot_6 <- ggplot(data=inter_glm, aes(x=inter_glm[,1], y=fit, group=inter_glm[,2])) +
            geom_line(size=2, aes(color=inter_glm[,2]))+
            geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill=inter_glm[,2]),alpha=.2)+
            facet_grid(~title) +
            labs(x = variable1,
                 y = "Predicted abundance",
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

          print(plot_6)

          list_graph[[length(list_graph) + 1]] <- list(plot_6)

          plot_7 <- ggplot(data=inter_glm, aes(x=inter_glm[,1], y=fit, fill=inter_glm[,2])) +
            geom_bar(stat="identity", position=position_dodge()) +
            labs(x = variable1,
                 y = "Predicted abundance",
                 color = variable2, fill = variable2) +
            facet_grid(~title) +
            theme_nice() + theme(axis.text.x = element_text(angle = 30, size=8),
                                 plot.title = element_text(size=10, face="bold"),
                                 axis.title.x = element_blank(),
                                 axis.title.y = element_text(size=8),
                                 legend.title = element_text(size=8),
                                 legend.text = element_text(size=8))
          print(plot_7)
          list_graph[[length(list_graph) + 1]] <- list(plot_7)
        }
      }
    }
  }
  if (plot ==T) {print(plot_5)}
  plot_8 <- ggarrange(plotlist=list_plot, ncol=2, nrow=2, common.legend = TRUE, legend = "bottom")
  list_graph[[length(list_graph) + 1]] <- list(plot_8)
  print(plot_8)
  return(list(table_interact, glm_indice_ab, list_graph))
}
