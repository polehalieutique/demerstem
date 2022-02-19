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

  print("SOUS-MODELE ABONDANCE")
  tableau_pres <- table_pres_abs(tab, esp, list_param,  var_eff_list, espece_id, catch_col, limit)
  tableau_ab <- filter(tableau_pres, i_ab>0)
  param <- list_param
  facteur <- param[1]

  if(plot==TRUE){

    tableau_ab$facteur=factor(tableau_ab[,facteur])

    print(ggplot(tableau_ab, aes(facteur))+
            geom_bar() +
            ggtitle(paste(facteur, title)) +
            ylab("nombre de donnees") +
            theme(axis.text.x = element_text(angle = 45)) +
            theme(axis.text.x = element_text(angle = 60, size=8), plot.title = element_text(size=11, face="bold"), axis.title.x = element_text(size=8), axis.title.y = element_text(size=8), legend.title = element_text(size=8), legend.text = element_text(size=8)))

    print(ggarrange(plotlist=lapply(param, moda_facto, tab=tableau_ab, title),
                    ncol=2, nrow=2, common.legend = TRUE, legend = "bottom"))

    requete <- tableau_ab %>% dplyr::group_by(facteur, annee) %>% dplyr::summarise(mean_ind_ab=mean(i_ab))

    print(ggplot(requete, aes(annee, mean_ind_ab)) +
            geom_point(aes(col=facteur), size=1) +
            geom_line(aes(group=facteur, color=facteur))+
            theme(axis.text.x = element_text(angle = 60, size=7), plot.title = element_text(size=8, face="bold"), axis.title.x = element_text(size=7), axis.title.y = element_text(size=7), axis.text.y = element_text(size=7)) +
            ggtitle(paste(facteur, title)) +
            ylab("mean CPUE") +
            theme(legend.key.size = unit(0.4, "cm"), legend.title = element_text(size=7), legend.text = element_text(size=6)))

    print(ggarrange(plotlist=lapply(param, evo_facto, tab=tableau_ab, title),
                    ncol=2, nrow=2))
  }

  for (i in 1:length(param)){
    tableau_ab[,param[i]] <- as.factor(tableau_ab[,param[i]])
    tableau_ab[,param[i]] <- droplevels(tableau_ab[,param[i]])
    contrasts(tableau_ab[,param[i]]) <- contr.sum(levels(tableau_ab[,param[i]]))
  }

  glm_indice_ab <- glm_ai_plus(tableau_ab, param, formula_select, summary)

  VAR <- var(residuals(glm_indice_ab))
  vect_param <- c(attr(glm_indice_ab$terms, "term.label"))
  table_interact <- c()
  table_ab <- as.data.frame(coef(summary(glm_indice_ab)))
  list_plot <- c()
  count <- 0
  for (j in 1:length(vect_param)){
    table_tempo <- as.data.frame(dummy.coef(glm_indice_ab)[j+1])
    table_tempo$namemodality <- rownames(table_tempo)
    rownames(table_tempo) <- NULL
    table_tempo$variable <- vect_param[j]
    colnames(table_tempo) <- c("estimates", "modality", "variable")
    if (as.numeric(gregexpr(pattern =':',as.character(attr(glm_indice_ab$term, "term.labels"))[j]))>0){ # if interaction : other plot
      table_tempo <- table_tempo %>% filter(estimates != 0)
    }
    table_tempo$corrected_estimates <- table_tempo$estimates + table_ab[1,1]
    table_tempo$corrected_estimates <- exp(table_tempo$corrected_estimates + 0.5*VAR)

    if (as.numeric(gregexpr(pattern =':',as.character(attr(glm_indice_ab$term, "term.labels"))[j]))<0){ # else : plot the estimation for each modality
      count <- count + 1
      table_tempo$modality <- as.factor(table_tempo$modality)
      table_tempo$modality <- ordered(table_tempo$modality, levels = levels(tableau_ab[,vect_param[j]]))
      list_plot[[j]] <- local({
        tempo <- j
        x1 <- ggplot(table_tempo) +
          geom_bar(aes(x=modality, y=corrected_estimates), stat="identity", color = "black", fill = "white") +
          ylab("Estimation") +
          ggtitle(paste(vect_param[tempo], "for abundance data")) +
          theme(axis.text.x = element_text(angle = 60, size=8), plot.title = element_text(size=10, face="bold"), axis.title.x = element_text(size=8), axis.title.y = element_text(size=8), legend.title = element_text(size=8), legend.text = element_text(size=8)) +
          labs(x = vect_param[tempo])
        })
    }
    ## ICI
    if(plot==TRUE){
      if(vect_param[j]==facteur){
        plot(ggplot(table_tempo) +
               geom_bar(aes(x=modality, y=corrected_estimates), stat="identity", color = "black", fill = "white") +
               ylab("Estimation") +
               ggtitle(paste(vect_param[j], "for abundance data")) +
               theme(axis.text.x = element_text(angle = 60, size=8), plot.title = element_text(size=10, face="bold"), axis.title.x = element_text(size=8), axis.title.y = element_text(size=8), legend.title = element_text(size=8), legend.text = element_text(size=8)) +
               labs(x = vect_param[j]))
        }
    }


    table_interact <- rbind(table_interact, table_tempo)

    if (interactions == TRUE){
      object <- gregexpr(pattern =':',as.character(attr(glm_indice_ab$term, "term.labels"))[j])
      if (length(object)>0){
        if(as.numeric(object)>0){
          interact_variables <- strsplit(as.character(attr(glm_indice_ab$term, "term.labels"))[j], ":")
          variable1 <- interact_variables[[c(1,1)]]
          variable2 <- interact_variables[[c(1,2)]]

          formula_interact <- paste(variable1,":",variable2)
          inter_glm <- effect(formula_interact, glm_indice_ab, se=TRUE)
          inter_glm<-as.data.frame(inter_glm)
          inter_glm[,1] <- factor(inter_glm[,1], levels=levels(tableau_ab[,variable1]))
          inter_glm[,2] <- factor(inter_glm[,2], levels=levels(tableau_ab[,variable2]))

          plot_inter_glm<-ggplot(data=inter_glm, aes(x=inter_glm[,1], y=exp(fit+0.5*VAR), group=inter_glm[,2]))+
            geom_line(size=2, aes(color=inter_glm[,2]))+
            geom_ribbon(aes(ymin=exp(fit+0.5*VAR) - exp(se + 0.5*VAR), ymax=exp(fit+0.5*VAR) + exp(se + 0.5*VAR),fill=inter_glm[,2]),alpha=.2)+
            labs(x = variable1,
                 y = "Predicted Abundance",
                 color = variable2, fill = variable2) +
            ggtitle(paste("First Interaction Plot for", attr(glm_indice_ab$term, "term.labels"))[j])+
            theme_bw()+
            #theme(text = element_text(size=12),
            #legend.text = element_text(size=12),
            #legend.direction = "horizontal",
            #panel.grid.major = element_blank(),
            #panel.grid.minor = element_blank(),
            #legend.position="top")
            theme(axis.text.x = element_text(angle = 60, size=8), plot.title = element_text(size=10, face="bold"), axis.title.x = element_text(size=8), axis.title.y = element_text(size=8), legend.title = element_text(size=8), legend.text = element_text(size=8))
          print(plot_inter_glm)

          plot_inter_glm2 <- ggplot(data=inter_glm, aes(x=inter_glm[,1], y=exp(fit+0.5*VAR), fill=inter_glm[,2])) +
            geom_bar(stat="identity", position=position_dodge()) +
            labs(x = variable1,
                 y = "Predicted Abundance",
                 color = variable2, fill = variable2) +
            ggtitle(paste("Second Interaction Plot for", attr(glm_indice_ab$term, "term.labels"))[j])+
            theme_bw() + theme(axis.text.x = element_text(angle = 30, size=8), plot.title = element_text(size=10, face="bold"), axis.title.x = element_text(size=8), axis.title.y = element_text(size=8), legend.title = element_text(size=8), legend.text = element_text(size=8))
          print(plot_inter_glm2)
          #print(ggarrange(plot_inter_glm, plot_inter_glm2, ncol=2, common.legend = TRUE, legend = "bottom"))

          #print(plot_inter_glm2)
        }
      }
    }
  }
  print(ggarrange(plotlist=list_plot, ncol=2, nrow=2, common.legend = TRUE, legend = "bottom"))
  #ceiling(count/2)
  return(table_interact)
}
