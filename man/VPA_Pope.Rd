% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/VPA_pope.R
\name{VPA_Pope}
\alias{VPA_Pope}
\title{VPA_Pope}
\usage{
VPA_Pope(FT, age, Mat_Cage, Mat_M)
}
\arguments{
\item{age}{maximal age}

\item{Mat_Cage}{Matrix with Catch sum over fleets for each age}

\item{Mat_M}{Mortality matrix}

\item{FT_init}{Initial value for pseudo cohort analysis, equivalent to the F Terminal}

\item{Mat_F}{Fishing mortality matrix}
}
\description{
\code{VPA_Pope} first approach to estimate a range for initial recruitment
}
\examples{
 data(tableau_sc)
 model_ai_plus(tableau_sc, esp = "PSEUDOTOLITHUS ELONGATUS", effort = "auto", title = "SC", list_param = c("annee", "saison", "strate"), var_eff_list=c("surface_chalutee"), espece_id='nom_taxonomique', catch_col='total_capture', interactions =FALSE, limit=0.001, formula_select = "log(i_ab+0.0001) ~ strate + annee + saison")

}
