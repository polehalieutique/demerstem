#' this function calculate the fishing effort E, the corresponding Fox effort Efox ; standardised on the last year (= most recent year : mEfox = 1)
#'
#'
#' @param data          : the table with the mean IA serie
#' @param catch_table : table with catches and years (format :
#' @param k             : K factor of the Fox effort calculation. Either : 1, 3 or 5
#'
#' @examples
#'
#' @export


effort_fox_calculation <- function(data, catch_table, k){
  data <- data[-2:-(ncol(data)-1)]
  colnames(data) <- c('Year', 'IA')

  colnames(catch_table) <- c('Year', 'Capture')
  table_IA <- left_join(data, catch_table, by = "Year")

  #table_IA <- as.data.frame(table_IA)
  table_IA <- mutate(table_IA, E = Capture/IA)

  #Efox
  vecttempo <- c()
  for (i in 1:nrow(table_IA)){
    if(!is.na(table_IA$E[i])){
      vecttempo <- c(vecttempo, i)
    }
  }
  compt <- vecttempo[1]
  max <- max(vecttempo)
  if (k==1){
    table_IA$Efox <- table_IA$E
  } else if (k==3){
    for (i in compt:(max-2)){
      j <- i-compt+1
      table_IA$Efox[max-j+1] <- (3*table_IA$E[max-j+1] + 2*table_IA$E[max-j] + table_IA$E[max-j-1])/6
    }
    table_IA$Efox[compt+1] <- (3*table_IA$E[compt+1] + 2*table_IA$E[compt])/5
    table_IA$Efox[compt] <- NA
  } else {
    for (i in compt:(max-4)){
      j <- i-compt+1
      table_IA$Efox[max-j+1] <- (5*table_IA$E[max-j+1] + 4*table_IA$E[max-j] + 3*table_IA$E[max-j-1] + 2*table_IA$E[max-j-2] + table_IA$E[max-j-3])/15
    }
    table_IA$Efox[compt+3] <- (5*table_IA$E[compt+3] + 4*table_IA$E[compt+2] + 3*table_IA$E[compt+1] + 2*table_IA$E[compt])/14
    table_IA$Efox[compt+2] <- (5*table_IA$E[compt+2] + 4*table_IA$E[compt+1] + 3*table_IA$E[compt])/12
    table_IA$Efox[compt+1] <- (5*table_IA$E[compt+1] + 4*table_IA$E[compt])/9
    table_IA$Efox[compt] <- NA
  }

  # Relatif à la dernière valeur
  factE <- table_IA$E[dim(table_IA)[1]]
  factEfox <- table_IA$Efox[dim(table_IA)[1]]
  table_IA$factEfox <- factEfox

  table_IA <- table_IA %>% mutate(E = E/factE , Efox = Efox/factEfox)

  return(table_IA)

}



