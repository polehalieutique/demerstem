#'  Weight size relationship and plot
#'
#' \code{model_weight} estimates R_init
#'
#' @param   data_weight          data with weight and size vectors
#' @param   a                    initialisation of a (set at 0.05)
#' @param   b                    initialisation of b (set at 2.5)
#'
#' @examples
#'  data(data_weight_size)
#'  data_weight_size <- na.omit(data_weight_size)
#'  model_weight(data_weight_size)
#' @export

model_weight <- function(data_weight, a= 0.05, b=2.5) {
  list <- NULL
  data_weight$weight <- as.numeric(data_weight$weight)
  data_weight$length <- as.numeric(data_weight$length)
  M1 <- nls(formula = weight ~ a*(length ^ b),
            start = list(a= a, b= b), algorithm = "default", trace = T, data = data_weight)

    fun <- function(a,b,L){
          W <- a*L^b
          return(W)
         }

  pred = data.frame(l = seq(0,max(data_weight$length)+1 ,1), w = fun(a = coef(M1)[1], b = coef(M1)[2], seq(0, max(data_weight$length)+1 , 1)))
  data_weight$title <- "Size-Weight relationships"
  plot_1 <- ggplot(data = data_weight) + geom_point(aes(x = length, y = weight), color = "blue") +
    geom_line(data = pred, aes(x=l, y =w), col = 'red', size = 1.2) +
    facet_grid(~title) + theme_nice() +
    theme(axis.text = element_text(size = 11, color = "black"),
          axis.title = element_text(size = 11, face= "bold", color = "black")) +  labs(y = "Weight (g)", x = "Length (cm)")

  a <- coef(M1)[1]
  b <- coef(M1)[2]
  print(paste0("a = ", round(a,4)," b = ",round(b,4)))
  list <- list(c(round(a,4),round(b,4)))
  list[[2]] <- plot_1
  return(list)
}
