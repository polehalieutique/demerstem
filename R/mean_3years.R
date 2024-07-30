#' Smooth over 3 years
#'
#' \code{mean_ai} smooth the value over 3 years (or 2 years at the start and the end of the serie)
#'
#' @param data  the table in mean_IA to smooth. The mean AI column should be named "
#'
#' @examples
#' year <- c(1985:1995)
#' AI_standard <- rnorm(10,0.5,0.1)
#' data_ex <- as.data.frame(cbind(year, AI_standard))
#' mean_3years(data_ex)
#' @export




mean_3years <- function(data){
  data$AI_cor[1] <- mean(c(data$AI_standard[1], data$AI_standard[1], data$AI_standard[2]), na.rm=T)
  z <- nrow(data)
  data$AI_cor[z] <- mean(c(data$AI_standard[z], data$AI_standard[z], data$AI_standard[z-1]), na.rm=T)
  for (i in 2:(nrow(data)-1)){
    data$AI_cor[i] <- mean(c(data$AI_standard[i-1], data$AI_standard[i], data$AI_standard[i+1]), na.rm=T)

  }


  return(data)
}

