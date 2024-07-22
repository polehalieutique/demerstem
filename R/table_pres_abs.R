#' Creating the table used for the GLMs
#'
#' \code{table_pres_abs} returns the table used as an argument in the glm modeling.
#' It creates the \emph{i_ab} and the \emph{presence} columns, and apply the modality selection functions.
#'
#' @param tab           Table with the captures, fishing efforts and associated parameters
#' @param esp           exact name of the studied species
#' @param list_param    list of the tested parameters
#' @param var_eff_list  list of the possible fishing effort column
#' @param espece_id     exact name of the column indicating the species
#' @param catch_col     exact name of the column indicating the catches
#' @param limit         percentage representing the limit value under which a modality is removed
#'
#' @return the function return a table from which we can select only the \emph{presence = 1} to get the abundance table
#'
#' @examples
#' data(tableau_sc)
#' table_pres_abs(tableau_sc, effort="auto", esp="PSEUDOTOLITHUS ELONGATUS", list_param=c("annee", "saison", "strate"), var_eff_list=c("surface_chalutee"), espece_id='nom_taxonomique', catch_col='total_capture', limit=0.0001)
#'
#'
#' @export

table_pres_abs <- function(tab, esp, list_param, var_eff_list, espece_id, catch_col, limit) {

  #ope_id
  tab_ope_id <- tab %>% dplyr::select(-all_of(var_eff_list), -all_of(catch_col))
  ope_id <- c(colnames(tab_ope_id), "presence")
  ope_id2 <- colnames(tab_ope_id)


  # Garder uniquement les colonnes du tableau contenues dans col_list
  # liste des colonnes a garder
  col_list = c(list_param,  espece_id, var_eff_list, ope_id, catch_col)
  col.use <- names(tab)[(names(tab) %in% col_list)]
  tab <- tab[, col.use]

  # Tableau avec seulement les presence : établie à partir des noms (si un nom d'espèce est décrit alors = présence), mais il peut y avoir une capture = 0 définie malgré tout
  espece_id <- names(tab)[(names(tab) %in% espece_id)]
  tab_posit <- tab %>% mutate(presence=as.numeric(tab[,espece_id] == esp)) %>% filter(presence==1)


  # Calcul de l indice d_abondance and we remove rows without effort (later, kept for pres/abs analysis).
  col_effort <- var_eff_list
  print(paste("effort = ", col_effort))
  print(paste(sum(is.na(tab_posit[,col_effort]) | tab_posit[,col_effort]==0),"over", nrow(tab_posit),
              "lines with effort = 0 or NA while catches are available, are then lost")) # indicate number of data lost

  #calcul de la cpue (Les Inf seront retirése qd seront utilisées les abondances)
  tab_posit <- tab_posit %>% mutate(i_ab=tab_posit[,catch_col]/tab_posit[,col_effort])
  print(paste("i_ab =", catch_col, "/" ,col_effort))


  # Aggreger par station/operation : Identifier si il y a des réplicats sur une station (qui ne devraient pas exister)
  names.use <- names(tab_posit)[(names(tab_posit) %in% ope_id)]
  tab_posit_old <- tab_posit
  tab_posit <- tab_posit_old %>% dplyr::group_by(across(names.use)) %>% dplyr::summarise(i_ab = sum(i_ab), .groups = 'drop') %>% ungroup() # RQ1
  print(paste("passing from", nrow(tab_posit_old), "lines to", nrow(tab_posit), "lines with presences, aggregating by station or fishing operation"))
  if (nrow(tab_posit) != nrow(tab_posit_old)){
    print(paste("WARNING: you're not suppose to aggregate many lines. Please take the time to check your data"))
    print(paste("aggregation here with / agrégation ici avec i_ab = sum(i_ab)"))
  }
  tab_posit <- tab_posit %>% distinct()
  print(paste(nrow(tab_posit), "stations selected with presence of", esp))

  tableau_ab <- as.data.frame(tab_posit) #tableau pour glm sous-modele positif

  # Rajouter les absences
  # Aggreger le tableau d origine par station/operation mais sans les especes
  names.use <- names(tab)[(names(tab) %in% ope_id)]
  tab_stations <- tab[, names.use]
  # Select unique + jointure tab_posit + ajout des 0 pour les absences
  presabs <- tab_stations %>% distinct() %>% left_join(tab_posit, by = ope_id2) %>% mutate(presence = replace_na(presence, 0))
  # table(presabs$presence)
  print(paste("Total of", nrow(presabs), "stations with presence or absence data"))


  # Enlever les modalites peu representees dans tableau_ab (representant par ex moins de 0.5% des donnees)
  names_facteur <- names(tableau_ab)[(names(tableau_ab) %in% list_param)] #liste param = facteurs utilises pour le GLM
  tableau_ab2 <- moda_selection(tab, names_facteur, limit, ope_id2)
  # On garde uniquement les modalités présentes dans tableau_ab2 (selon valeur limit)
  tableau_pres <- presabs %>% semi_join(tableau_ab2, by = names_facteur)
  nrow(tableau_pres)
  tableau_pres$mois <- as.factor(tableau_pres$mois)

  return(tableau_pres)
}
