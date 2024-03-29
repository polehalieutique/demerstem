% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pope_abundance.R
\name{pope_abundance}
\alias{pope_abundance}
\title{pope_abundance}
\usage{
pope_abundance(age)
}
\arguments{
\item{age}{from VPA_pope}
}
\description{
\code{pope_abundance} estimates N at age
}
\examples{
 data(tableau_sc)
 model_ai_plus(tableau_sc, esp = "PSEUDOTOLITHUS ELONGATUS", effort = "auto", title = "SC", list_param = c("annee", "saison", "strate"), var_eff_list=c("surface_chalutee"), espece_id='nom_taxonomique', catch_col='total_capture', interactions =FALSE, limit=0.001, formula_select = "log(i_ab+0.0001) ~ strate + annee + saison")

}
