#' Bayesian function (not used for the moment)
#' \code{bgm_output_5} fonction graphique
#' @param mcmc mcmc liste venant de la fonction bgm

#' @examples
#'

#' @export


bgm_output_1<- function (mcmc,data,n_proj=0,multiple=FALSE){

  # --------------------------------------------
# Work with mcmc.list
# --------------------------------------------

# "mcmc" is an object of the class "mcmc.list" (see package library(coda)
# to explore, plot ... mcmc objects

is(mcmc)

# Names of the variables stored in the mcmc list

varnames(mcmc)


# Extract the variable of interest from the mcmc list
# var.mcmc is still an mcmc list but contains only the variable of interest
# Works only for vectorial variable

var = "B"
var.mcmc = mcmc[,which(substr(varnames(mcmc),1,nchar(var)+1)==paste(var,"[",sep=""))]
varnames(var.mcmc)

# One dimensional variable can be extracted directly

r.mcmc <- mcmc[,c("r")]
B1.mcmc <- mcmc[,c("B[1]")]



# ---------------------------------------------------
# Convergence diagnostics and measure of performance
# ---------------------------------------------------

# Gelman-Rubin convergence diagnostics
# Point est. should be near 1

gelman.diag(mcmc[,c("B[1]")], confidence = 0.95, transform=TRUE, autoburnin=TRUE)
if (multiple)
{
  gelman.diag(mcmc[,c("q1")], confidence = 0.95, transform=TRUE, autoburnin=TRUE)
  gelman.diag(mcmc[,c("q2")], confidence = 0.95, transform=TRUE, autoburnin=TRUE)
  gelman.diag(mcmc[,c("q3")], confidence = 0.95, transform=TRUE, autoburnin=TRUE)

}
else
{
gelman.diag(mcmc[,c("q")], confidence = 0.95, transform=TRUE, autoburnin=TRUE)
}

# Effective size
# An estimate of the number of "independent" samples

effectiveSize(var.mcmc)



# ---------------------------------------------------
# Traceplot (see also ?traceplot - coda package)
# ---------------------------------------------------

# Plot trace of ALL variables in var.mcmc-
# windows()
# plot(var.mcmc, trace = TRUE, density = FALSE)
op <- par(oma=c(1,1,1,1))
par(mfrow=c(1,3))
traceplot(mcmc[,'K'],ylab="K")
traceplot(mcmc[,'r'],ylab="r")
traceplot(mcmc[,'C_MSY'],ylab="C_MSY")
if (multiple)
{
traceplot(mcmc[,'q1'],ylab="q1")
  traceplot(mcmc[,'q2'],ylab="q2")
  traceplot(mcmc[,'q3'],ylab="q3")
}
else
{
  traceplot(mcmc[,'q'],ylab="q")

}
  traceplot(mcmc[,'sigma2p'],ylab="sigma2p")


#windows()
par(mfrow=c(2,3))
traceplot(mcmc[,'B[1]'],ylab="B[1]")
traceplot(mcmc[,'B[5]'],ylab="B[5]")
traceplot(mcmc[,'B[10]'],ylab="B[10]")
traceplot(mcmc[,'B[15]'],ylab="B[15]")
traceplot(mcmc[,'B[20]'],ylab="B[20]")
#traceplot(mcmc[,'B[25]'],ylab="B[25]")


# ---------------------------------------------------
# Density plot (see also ?densplot - coda package)
# ---------------------------------------------------

# Plot density of all variables in var.mcmc
# windows()
# plot(var.mcmc, trace = FALSE, density = TRUE)

#windows()
par(mfrow=c(2,3))
densplot(mcmc[,'K'],ylab="K")
densplot(mcmc[,'r'],ylab="r")
densplot(mcmc[,'C_MSY'],ylab="C_MSY")
densplot(mcmc[,'B_MSY'],ylab="B_MSY")
if (multiple)
{
densplot(mcmc[,'q1'],ylab="q1")
  densplot(mcmc[,'q2'],ylab="q2")
  densplot(mcmc[,'q3'],ylab="q3")
}
else
{
  densplot(mcmc[,'q'],ylab="q")


}
densplot(mcmc[,'sigma2p'],ylab="sigma2p")




# --------------------------------------------
# Work with mcmc samples stored in TABLES
# --------------------------------------------


# Extract MCMC chains and store in a TABLE
# sometimes easier to manipulate than mcmc.list object
# each column = all mcmc samples for each variable

# as.matrix() does not work if mcmc contains a multidimensional variable
# but works after window is used

mcmc <- window(mcmc)
mcmc.table <- as.data.frame(as.matrix(mcmc))
head(mcmc.table)
dim(mcmc.table)


# Plot density from the mcmc.table

#windows()
# par(mfrow = c(2,3))
# plot(density(mcmc.table$'K'))
# plot(density(mcmc.table$'r'))
# plot(density(mcmc.table$'C_MSY'))
# if (multiple)
# {
# plot(density(mcmc.table$'q1'))
#   plot(density(mcmc.table$'q2'))
#   plot(density(mcmc.table$'q3'))
# }
# else
# {
#   plot(density(mcmc.table$'q'))
#
# }
# plot(density(mcmc.table$'sigma2p'))

# ------------------------------------------------------------------------------------
# Estimation of Risk
# ------------------------------------------------------------------------------------
par(mfrow = c(2,2))
var = "risk"
var.mcmc = mcmc[,which(substr(varnames(mcmc),1,nchar(var)+1)==paste(var,"[",sep=""))]
varnames(var.mcmc)
risk <- summary(var.mcmc)$statistics[,1]
Year <- data$Year[seq(1,length(risk))]
risk <- data.frame(Year = Year, risk = risk)
#windows()
risk$title <- "risk"
plot(ggplot(data = risk) + geom_line(data = risk, aes(x= Year, y = risk, group = 1), col = 'red') +
  theme_nice() + facet_grid(.~title))

# --------------------------------------------------------------------------
# Basic Boxplot of time series
# --------------------------------------------------------------------------
# Extract all variable "a" from the mcmc table --> put in a new table x

#windows() ;
par(mfrow = c(2,2))


todo<-data.frame(x=c('B','C','h','D'),
                 title=c("Biomass","Catches","Harvest rates","Depletion"))

i<-1
g<-list()

mcmc <- window(mcmc)
mcmc.table <- as.data.frame(as.matrix(mcmc))
g<-list()
for (i in 1:(length(todo$x)))
{

print(i)
variable<-todo[i,1]
title = todo[i,2]


x<-mcmc.table[,which(substr(colnames(mcmc.table),1,nchar(variable)+1)==paste(variable,"[",sep=""))]

names(x)<-data$Year[seq(1,length(names(x)))]


x %>% pivot_longer(cols=names(x), names_to = "Year") %>% mutate(title = title) %>%
  ggplot() + geom_boxplot(aes(x=as.numeric(Year),y=value, group = Year),outlier.size=0.5,outlier.colour = 'grey') +
    #geom_line(data = risk, aes(x= Year, y = risk, group = 1), col = 'red') +
  labs(x = "Year") +theme_nice() + facet_grid(.~title) ->g[[i]]

}

print(ggarrange(plotlist=g, widths = c(2,2)))


}
