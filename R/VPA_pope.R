#'  VPA_Pope
#'
#' \code{VPA_Pope} estimates R_init
#'
#' @param   FT_init          Initial value for pseudo cohort analysis, equivalent to the F Terminal
#' @param   age              maximal age
#' @param   Mat_Cage         Matrix with Catch sum over fleets for each age
#' @param   Mat_M            Mortality matrix
#' @param   Mat_F            Fishing mortality matrix
#'
#'
#' @examples
#'  data(tableau_sc)
#'  model_ai_plus(tableau_sc, esp = "PSEUDOTOLITHUS ELONGATUS", effort = "auto", title = "SC", list_param = c("annee", "saison", "strate"), var_eff_list=c("surface_chalutee"), espece_id='nom_taxonomique', catch_col='total_capture', interactions =FALSE, limit=0.001, formula_select = "log(i_ab+0.0001) ~ strate + annee + saison")
#'
#' @export

VPA_Pope<-function(FT_init, age, Mat_Cage, Mat_M)
{
  #Affichage de la valeur initiale cad de la mortalite par pêche terminale
  #print(paste("La valeur de F terminal est", $init))
  Mat_F<-rep(NA,age)
  Mat_N<-rep(NA,age)

  print(FT_init)
  # Calculs suivant l'approximation de Pope
  # Valeurs terminales de F et N
  Mat_F[age]<-FT_init
  assign('Mat_F', Mat_F, envir=globalenv())

  Mat_N[age]<-pope_abundance(age)
  assign('Mat_N', Mat_N, envir=globalenv())

  # Calcul des effectifs et mortalites par pêche suivant Pope (pas de solveur)
  for(i in 2:age-1) {
    pope_N(age-i)
    pope_F(age-i)
  }
}
