#' indice_ab_pres
#' Cette fonction est la reunion de deux anciennes fonctions décrites ci-dessous. Elle créé les tableaux necessaires pour le GLM et delta_GLM.

#' Fonction 1 (indice_ab) créé la variable d'abondance sur laquelle seront ajustés les modèles. Pour les pêches scientifiques, il s'agit des captures par surface chalutee. Pour les pêches commerciales, il s'agit des cpue nominales. Pour les cpue, cette fonction automatise le choix de la variable d'effort nominal selon un critère de disponibilité des données. Le choix de la variable d'effort nominal pourra également être manuel. Elle retourne une ligne par operation ou station.
#' Fonction 2 (pres_abs) créé le tableau qui sélectionne une espèce et donne sa presence/absence par station ou operation en vue d'un delta GLM (sous modèle presence/absence)


#' @param tab               : Tableau avec les captures et paramètres associés
#' @param type_donnee       : "scientifique" ou "commercial"
#' @param effort            : "auto" pour un choix automatique de la variable effort ou choix personnel ex "duree_peche","nombre_operation","nb_jour_peche", "nb_sorties", "surface_chalutee" si pas besoin de recalculer la surface chalutee dans les donnees scientifiques
#' @param esp               : nom_taxonomique ou commun selon tableau, espece dont on veut tester la presence. ex : "BOBO" , "PSEUDOTOLITHUS ELONGATUS"
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
  if(type_donnee=="commercial"){
    if(effort=="auto"){
      # choix de la variable d'effort nominal :
      # Rappel : var_eff_list = variables possibles d'effort nominal
      names.use <- names(tab_posit)[(names(tab_posit) %in% var_eff_list)]
      tab_posit_effort <- tab_posit[, names.use]
      a <- tab_posit_effort %>% summarise_all(funs(sum(is.na(.) | .==0)))
      col_effort <- names(which.min(a)) #colonne(s) avec le plus de donnees renseignees
      col_effort <- col_effort[[1]] #si plusieurs colonnes
    }else{col_effort=effort}
    print(paste("effort = ", col_effort))
    print(paste(sum(is.na(tab_posit[,col_effort]) | tab_posit[,col_effort]==0),"sur", nrow(tab_posit),
                "lignes avec effort nul ou NA")) # indiquer le nombre de donnees perdues
    #calcul de la cpue
    tab_posit <- tab_posit %>% mutate(i_ab=tab_posit[,col_capture2]/tab_posit[,col_effort])
    print(paste("i_ab =", col_capture2, "/" ,col_effort))

  }else if(type_donnee=="scientifique"){
    # calcul de la surface chalutee = ouverture chalut*distance
    if(effort=="auto"){
      # Calcul de distance des traits : methode a choisir selon la perte de donnees induite (lors de la preparation du tableau dans prep_GLM_tab)
      # methode 1 : calcul de distance entre 2 points
      # geom_deb <-  st_as_sf(tab_posit, coords = c("longitude_deb", "latitude_deb"),
      #                       crs = 4326, agr = "constant")
      #
      # geom_fin <- st_as_sf(tab_posit, coords = c("longitude_fin", "latitude_fin"),
      #                      crs = 4326, agr = "constant" )
      #
      # tab_posit$distance_chalutee <- st_distance(geom_deb, geom_fin, by_element = TRUE) # lwgeom plus long que pointDistance (raster)
      # methode 2 : calcul de distance avec vitesse et duree (vitesse chalutage en noeuds)
      tab_posit <- tab_posit %>% mutate(distance_chalutee=as.numeric((duree/60) * vitesse_chalutage*1.852*1000)) # distance chalutee en m (homogene avec methode 1)

      tab_posit <- tab_posit %>% mutate(surface_chalutee=as.numeric(distance_chalutee*larg_ouverture/1000000)) #passer de m2 a km2 pour methode1
    }else{
      tab_posit <- tab_posit %>% mutate(surface_chalutee=as.numeric(effort))
    }
    tab_posit_old <- tab_posit

    tab_posit <- tab_posit_old %>% filter(as.numeric(surface_chalutee) > 0)
    tab_posit <- tab_posit %>% mutate(i_ab=as.numeric(tab_posit[,col_capture2])/as.numeric(surface_chalutee)) # calcul de l indice_abondance
    print(paste( "perte de ", nrow(tab_posit_old)-nrow(tab_posit) , "surface chalutees nulles ou NA, sur" , nrow(tab_posit_old), "a l'origine"))
    print(paste("i_ab =", col_capture2, "/surface_chalutee(km2)"))
  }

  # Aggreger par station/operation
  names.use <- names(tab_posit)[(names(tab_posit) %in% ope_id)]
  tab_posit_old <- tab_posit
  tab_posit <- tab_posit_old %>% group_by(across(names.use)) %>% summarise(i_ab = sum(i_ab))
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
  tableau_ab2 <- moda_few_rm(tableau_ab, names_facteur, seuil)

  # Enlever dans tableau pres les modalites qui sont absentes dans tableau_ab
  tableau_pres <- presabs %>% semi_join(tableau_ab, by = names_facteur)
  tableau_pres$mois <- as.factor(tableau_pres$mois)

  return(tableau_pres)
}
