% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/new_delta_glm.R
\name{new_delta_glm}
\alias{new_delta_glm}
\title{Delta coupling of 2 GLMs}
\usage{
new_delta_glm(
  tab_pres,
  tab_ia,
  esp,
  title,
  list_param,
  var_eff_list,
  espece_id,
  catch_col,
  limit,
  formula_select_pres,
  formula_select_ia,
  repartition = 0,
  data_type
)
}
\arguments{
\item{tab_pres}{input dataset table used for}

\item{tab_ia}{ia}

\item{esp}{exact name of the studied species}

\item{title}{fraction of the title in the plots}

\item{list_param}{list of the tested parameters}

\item{var_eff_list}{list of the possible fishing effort column}

\item{espece_id}{exact name of the column indicating the species}

\item{catch_col}{exact name of the column indicating the catches}

\item{limit}{percentage representing the limit value under which the modality is removed}

\item{formula_select_pres}{if "auto", the function select which formula as the lowest AIC. Else, run the selected formula.}

\item{formula_select_ia}{if "auto", the function select which formula as the lowest AIC. Else, run the selected formula.}

\item{repartition}{}

\item{data_type}{}
}
\description{
\code{new_delta_glm} realize the delta-coupling using the 2 precedents GLMs outputs (filtered with year modality only), extract the year factor, calculate the AI and display the plots.
Presence/Absence-GLM modeling process
}
\examples{
data(tableau_sc)
glm_pres <- model_pres_abs(tableau_sc, esp="PSEUDOTOLITHUS ELONGATUS", effort="auto", title="SC", list_param=c("annee", "saison", "strate"), var_eff_list=c("surface_chalutee"), espece_id='nom_taxonomique', catch_col='total_capture', interactions = FALSE, limit=0.0001, formula_select = "presence ~ strate + annee + saison")
}
