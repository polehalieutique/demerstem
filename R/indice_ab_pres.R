#' indice_ab_pres
#' Cette fonction est la reunion de deux anciennes fonctions décrites ci-dessous. Elle créé les tableaux necessaires pour le GLM et delta_GLM.

#' Fonction 1 (indice_ab) créé la variable d'abondance sur laquelle seront ajustés les modèles. Pour les pêches scientifiques, il s'agit des captures par surface chalutee. Pour les pêches commerciales, il s'agit des cpue nominales. Pour les cpue, cette fonction automatise le choix de la variable d'effort nominal selon un critère de disponibilité des données. Le choix de la variable d'effort nominal pourra également être manuel. Elle retourne une ligne par operation ou station.
#' Fonction 2 (pres_abs) créé le tableau qui sélectionne une espèce et donne sa presence/absence par station ou operation en vue d'un delta GLM (sous modèle presence/absence)


#' @param tab               : Tableau avec les captures et paramètres associés
#' @param type_donnee       : "scientifique" ou "commercial"
#' @param effort            : "auto" pour un choix automatique de la variable effort ou choix personnel ex "duree_peche","nombre_operation","nb_jour_peche", "nb_sorties", "surface_chalutee" si pas besoin de recalculer la surface chalutee dans les donnees scientifiques
#' @param esp               : yyy nom_taxonomique ou commun selon tableau, espece dont on veut tester la presence. ex : "BOBO" , "PSEUDOTOLITHUS ELONGATUS"
#' @param list_param liste des param_tres
#' @param espece_id_list liste des espèces
#' @param var_eff_list  liste des données d'éffort
#' @param ope_id columne des opérations
#' @param col_capture       : listes d'identifiants de colonnes (voir prep_GLM_tab.Rmd)
#' @param seuil             : seuil minimum de frequence des modalites a garder par facteur

#' @return La fonction incice_ab_pres() retourne un seul tableau : tableau_pres a patir duquel on peut filtrer les presences pour obtenir tableau_ab

#' @examples
#' #Liste des colonnes  parametres categoriels (facteurs) à explorer
#' list_param <- c("mois","zone","type_pirogue","motorisation","zone2","saison","engin_peche2", #PECHE ARTISANALE
#' "licence" , "tjb2", "puissance2", "longueur2", "profondeur2","nationalite"  ,"sect_cod" , "tsect_cod" ,"intitule",  #PECHE INDUSTRIELLE
#' "strate", "profond_deb2", "code_campagne2" , "code_projet")
#' #Liste des noms de colonnes possible pour l'effort de pêche
#'  var_eff_list <- c("duree_peche", "nombre_operation","nb_jour_peche", "nb_sorties", "larg_ouverture", "total_capture", "duree", "vitesse_chalutage")
#'
#'    # liste des identifiants possible pour la colonne capture
#'  col_capture <- c("captures", "capture_retenue", "total_capture", "nombre")
#'     # liste des identifiants possible une observation
#'   ope_id=c("presence",
#'         "code_pays", "code_projet", "code_campagne2", "code_station ", "date_chalutage",
#'         "longitude_deb", "latitude_deb",  "longitude_fin", "latitude_fin",
#'         "strates", "profond_deb2", "type_campagne", #donnees scientifiques
#'         "code_navire", "date_operatio", "numero_operation","longitude_debut", "latitude_debut",
#'         "profondeur2", "puissance2", "tjb2", "longueur2", "nationalite", "licence", # donnees PI
#'         "annee", "mois", "zone", "zone2" , "saison", "type_pirogue" , "motorisation" , "engin_peche", "engin_peche2") #donnees PA
#' PA #
#' data(tableau_pa)
#'
#'
#' tableau_pa_pres <- indice_ab_pres (tableau_pa, "commercial", "auto", esp="BOBO", list_param, "espece", var_eff_list, ope_id, col_capture,seuil=0.005)
#' tableau_pa_ab <-  filter(tableau_pa_pres, presence==1)
#' kable(tableau_pa_ab[sample(nrow(tableau_pa_ab), 5), ])
#' kable(tableau_pa_pres[sample(nrow(tableau_pa_pres), 5), ])

#' PI #
#'
#' tableau_pi_pres <- indice_ab_pres (tableau_pi, "commercial", "duree_peche",  esp="PSEUDOTOLITHUS ELONGATUS", list_param,  espece_id_list, var_eff_list, ope_id, col_capture, seuil=0.05)
#' tableau_pi_ab <-  filter(tableau_pi_pres, presence==1)

#' SC Kg#
#' tableau_sc_pres <- indice_ab_pres (tableau_sc, "scientifique", "auto",  esp="PSEUDOTOLITHUS ELONGATUS", list_param,  espece_id_list, var_eff_list, ope_id, col_capture="total_capture", seuil=0.05)
#' tableau_sc_ab <-  filter(tableau_sc_pres, presence==1)

#' SC nombre#
#' tableau_sc_pres <- indice_ab_pres (tableau_sc, "scientifique", "auto",  esp="PSEUDOTOLITHUS ELONGATUS", list_param,  espece_id_list, var_eff_list, ope_id, col_capture="nombre", seuil=0.05)
#' tableau_sc_ab <-  filter(tableau_sc_pres, presence==1)
#' table(tableau_sc_pres$presence)

#' @export
#'
indice_ab_pres <- function(tab, type_donnee, effort, esp, list_param,  espece_id_list, var_eff_list, ope_id, col_capture, seuil) {

  # Garder uniquement les colonnes du tableau contenues dans col_list
  # liste des colonnes a garder
  col_list = c(list_param,  espece_id_list, var_eff_list, ope_id, col_capture)
  col.use <- names(tab)[(names(tab) %in% col_list)]
  tab <- tab[, col.use]

  # Tableau avec seulement les presence
  # Rappel : espece_id_list = liste des identifiants possibles pour l'espece
  espece_id <- names(tab)[(names(tab) %in% espece_id_list)]
  tab_posit <- tab %>% mutate(presence=as.numeric(tab[,espece_id] == esp)) %>% filter(presence==1)

  #colonnes capture possibles
  col_capture2 <- names(tab_posit)[(names(tab_posit) %in% col_capture)]
  col_capture2 <- col_capture2[[1]]

  # Calcul de l indice d_abondance
  col_effort <- var_eff_list
  print(paste("effort = ", col_effort))
  print(paste(sum(is.na(tab_posit[,col_effort]) | tab_posit[,col_effort]==0),"sur", nrow(tab_posit),
              "lignes avec effort nul ou NA")) # indiquer le nombre de donnees perdues
  #calcul de la cpue
  tab_posit <- tab_posit %>% mutate(i_ab=tab_posit[,col_capture2]/tab_posit[,col_effort])
  print(paste("i_ab =", col_capture2, "/" ,col_effort))


  # Aggreger par station/operation
  names.use <- names(tab_posit)[(names(tab_posit) %in% ope_id)]
  tab_posit_old <- tab_posit
  tab_posit <- tab_posit_old %>% dplyr::group_by(across(names.use)) %>% dplyr::summarise(i_ab = sum(i_ab)) # RQ1
  print(paste("passage de", nrow(tab_posit_old), "à", nrow(tab_posit), "presences en aggregeant par station ou operation"))
  tab_posit <- tab_posit %>% distinct()
  print(paste(nrow(tab_posit), "stations selectionnees avec presence de", esp))

  tableau_ab <- as.data.frame(tab_posit) #tableau pour glm sous-modele positif

  # Rajouter les absences
  # Aggreger le tableau d origine par station/operation mais sans les especes
  names.use <- names(tab)[(names(tab) %in% ope_id)]
  tab_stations <- tab[, names.use]
  # Select unique + jointure tab_posit + ajout des 0 pour les absences
  presabs <- tab_stations %>% distinct() %>% left_join(tab_posit) %>% mutate(presence = replace_na(presence, 0))
  # table(presabs$presence)
  print(paste("Total de", nrow(presabs), "stations catégorisées presence ou absence"))


  # Enlever les modalites peu representees dans tableau_ab (representant par ex moins de 5% des donnees)
  names_facteur <- names(tableau_ab)[(names(tableau_ab) %in% list_param)] #liste param = facteurs utilises pour le GLM
  tableau_ab2 <- moda_few_rm(tab, names_facteur, seuil)

  # On garde uniquement les modalités présentes dans tableau_ab2 (selon valeur seuil)
  tableau_pres <- presabs %>% semi_join(tableau_ab2, by = names_facteur)
  nrow(tableau_pres)
  tableau_pres$mois <- as.factor(tableau_pres$mois)

  return(tableau_pres)
}
