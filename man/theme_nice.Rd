% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/theme_nice.R
\name{theme_nice}
\alias{theme_nice}
\title{A nice and smoothie theme}
\usage{
theme_nice()
}
\description{
A nice and smoothie theme
}
\examples{

#Les données d'entrée
data(tableau_pa)

resultats_pa<-delta (tab=tableau_pa, esp="BOBO",list_param=c("mois","zone","type_pirogue","motorisation","zone2","saison","engin_peche2"), type_donnee="commercial", effort="auto", titre="PA", param_test=c("mois","zone","type_pirogue","motorisation","zone2","saison","engin_peche2"), espece_id_list='espece', var_eff_list= c("nb_jour_peche", "nb_sorties"), ope_id=c("presence","code_pays","annee", "mois", "zone", "zone2" , "saison", "type_pirogue" , "motorisation" , "engin_peche", "engin_peche2"), col_capture='captures', logtrans="auto", interactions="auto", facteur_flotille="engin_peche2", seuil=0.05)
pre_global_model_pa<-sqldf('select annee as Year,sum(i_ab) as C,avg("E.dens") as I from resultats_pa group by annee',drv='SQLite')
bgm(pre_global_model_pa)
}
