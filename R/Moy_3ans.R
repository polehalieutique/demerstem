#' Cette fonction sert à "smooth" sur 3 ans (ou 2ans pour première et dernière ligne)
#'
#'
#' @param data : la table avec IA_MOYEN_STANDARD à lisser
#'
#' @examples
#'
#' @export




Moy_3ans <- function(data){
  data$IA_MOYEN_STANDARD_COR[1] <- mean(c(data$IA_MOYEN_STANDARD[1], data$IA_MOYEN_STANDARD[1], data$IA_MOYEN_STANDARD[2]), na.rm=T)
  z <- nrow(data)
  data$IA_MOYEN_STANDARD_COR[z] <- mean(c(data$IA_MOYEN_STANDARD[z], data$IA_MOYEN_STANDARD[z], data$IA_MOYEN_STANDARD[z-1]), na.rm=T)
  for (i in 2:(nrow(data)-1)){
    data$IA_MOYEN_STANDARD_COR[i] <- mean(c(data$IA_MOYEN_STANDARD[i-1], data$IA_MOYEN_STANDARD[i], data$IA_MOYEN_STANDARD[i+1]), na.rm=T)

  }


  return(data)
}

