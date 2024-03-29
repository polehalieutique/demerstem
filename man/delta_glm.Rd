% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/delta_glm.R
\name{delta_glm}
\alias{delta_glm}
\title{Delta coupling of 2 GLMs}
\usage{
delta_glm(glm_pres_abs, glm_abundance, title, type)
}
\arguments{
\item{glm_pres_abs}{: table of effects values for each modality of the year factor taken from model_pres_abs (recalculated if needed)}

\item{glm_abundance}{: table of effects values for each modality of the year factor taken from model_ai_plus (recalculated if needed)}

\item{title}{: first part of the title for the plots}

\item{type}{: type of the data series. Ex : "SC", "PA", etc...}
}
\description{
\code{delta_glm} realize the delta-coupling using the 2 precedents GLMs outputs (filtered with year modality only), extract the year factor, calculate the AI and display the plots.
}
\examples{
data(tableau_sc)
glm_abundance <- model_ai_plus(tableau_sc, esp = "PSEUDOTOLITHUS ELONGATUS", effort = "auto", title = "SC", list_param = c("annee", "saison", "strate"), var_eff_list=c("surface_chalutee"),espece_id='nom_taxonomique', catch_col='total_capture', interactions ="N", limit=0.001, formula_select = "log(i_ab+0.0001) ~ strate + annee + saison")
glm_pres_abs <- model_pres_abs(tableau_sc, esp="PSEUDOTOLITHUS ELONGATUS", effort="auto", title="SC", list_param=c("annee", "saison", "strate"), var_eff_list=c("surface_chalutee"), espece_id='nom_taxonomique', catch_col='total_capture', interactions = "N", limit=0.0001, formula_select = "presence ~ strate + annee + saison")
delta_glm(glm_pres_abs, glm_abundance, title="Scientific campaign GIN BOBO", type = "SC")
}
