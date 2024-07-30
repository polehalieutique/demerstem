#'  Weight size relationship
#'
#' \code{model_weight} estimates a and b parameter from length and weight observations.
#'
#' @param   data_weight          data with weight and size vectors
#' @param   a                    initialisation of a (set at 0.05)
#' @param   b                    initialisation of b (set at 2.5)
#'
#' @examples
#'  df_wg_size = data_frame(length = seq(from = 0, to = 50, by = 0.5)) %>% mutate(weight = 0.015*(length^3.2))
#'  weight <- NULL
#'  length <- NULL
#'  l <- 0
#'  for (k in df_wg_size$weight) {
#'  weight <- c(weight, rnorm(10, k, k/10))
#'  length <- c(length, rep(l, 10))
#'  l <- l + 0.5}
#'  df_size <- data_frame(length, weight)
#'  plot(df_size)
#'  model_weight(df_size)
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
