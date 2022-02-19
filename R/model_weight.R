#'  Weight size relationship and plot
#'
#' \code{model_weight} estimates R_init
#'
#' @param   data_weight          data with weight and size vectors
#'
#' @examples
#'  data(data_weight_size)
#'  data_weight_size <- na.omit(data_weight_size)
#'  model_weight(data_weight_size)
#' @export

model_weight <- function(data_weight, weigth, length) {
  data_weight$weight <- as.numeric(data_weight$weight)
  data_weight$length <- as.numeric(data_weight$length)
  M1 <- nls(formula = weight ~ a*(length ^ b),
            start = list(a= 0.05, b=2.5), algorithm = "default", trace = T, data = data_weight)
  plot(data_weight$weight ~ data_weight$length, xlim = c(0,max(data_weight$length)), ylim = c(0,max(data_weight$weight)), xlab = "Length (cm)", ylab = "Weight (g)")
  curve(weight_size(x, p=c( a = coef(M1)[1], b = coef(M1)[2])), from = 0, to = 1000, n = 100, col = 'red', lwd = 2, add= TRUE)
  a <- coef(M1)[1]
  b <- coef(M1)[2]
  print(paste0("a = ", round(a,4)," b = ",round(b,4)))
}
