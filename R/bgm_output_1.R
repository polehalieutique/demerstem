#'  Bayesian global model, on encapsule un modèle global générique dans la fonction
#' @param mcmc mcmc liste venant de la fonction bgm

#' @examples
#'

#' @export

bgm_output_1<- function (mcmc){

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
gelman.diag(mcmc[,c("q")], confidence = 0.95, transform=TRUE, autoburnin=TRUE)


# Effective size
# An estimate of the number of "independent" samples

effectiveSize(var.mcmc)



# ---------------------------------------------------
# Traceplot (see also ?traceplot - coda package)
# ---------------------------------------------------

# Plot trace of ALL variables in var.mcmc-
# windows()
# plot(var.mcmc, trace = TRUE, density = FALSE)

#windows()
par(mfrow=c(3,3))
traceplot(mcmc[,'K'],ylab="K")
traceplot(mcmc[,'r'],ylab="r")
traceplot(mcmc[,'C_MSY'],ylab="C_MSY")
traceplot(mcmc[,'q'],ylab="q")
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
par(mfrow=c(3,3))
densplot(mcmc[,'K'],ylab="K")
densplot(mcmc[,'r'],ylab="r")
densplot(mcmc[,'C_MSY'],ylab="C_MSY")
densplot(mcmc[,'q'],ylab="q")
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
par(mfrow = c(2,3))
plot(density(mcmc.table$'K'))
plot(density(mcmc.table$'r'))
plot(density(mcmc.table$'C_MSY'))
plot(density(mcmc.table$'q'))
plot(density(mcmc.table$'sigma2p'))



# --------------------------------------------------------------------------
# Basic Boxplot of time series
# --------------------------------------------------------------------------
# Extract all variable "a" from the mcmc table --> put in a new table x

#windows() ;
par(mfrow = c(2,2))

x = "B"
title = "Biomass"

mcmc <- window(mcmc)
mcmc.table <- as.data.frame(as.matrix(mcmc))
x = mcmc.table[,which(substr(colnames(mcmc.table),1,nchar(x)+1)==paste(x,"[",sep=""))]

boxplot(x, outline = F, main = title)


x = "C"
title = "Catches"

mcmc <- window(mcmc)
mcmc.table <- as.data.frame(as.matrix(mcmc))
x = mcmc.table[,which(substr(colnames(mcmc.table),1,nchar(x)+1)==paste(x,"[",sep=""))]

boxplot(x, outline = F, main = title)


x = "h"
title = "Harvest rates"

mcmc <- window(mcmc)
mcmc.table <- as.data.frame(as.matrix(mcmc))
x = mcmc.table[,which(substr(colnames(mcmc.table),1,nchar(x)+1)==paste(x,"[",sep=""))]

boxplot(x, outline = F, main = title)


x = "D"
title = "Depletion"

mcmc <- window(mcmc)
mcmc.table <- as.data.frame(as.matrix(mcmc))
x = mcmc.table[,which(substr(colnames(mcmc.table),1,nchar(x)+1)==paste(x,"[",sep=""))]

boxplot(x, outline = F, main = title)


# ------------------------------------------------------------------------------------
# Estimation of Risk
# ------------------------------------------------------------------------------------

var = "risk"
var.mcmc = mcmc[,which(substr(varnames(mcmc),1,nchar(var)+1)==paste(var,"[",sep=""))]
varnames(var.mcmc)
risk <- summary(var.mcmc)$statistics[,1]
#windows()
plot(1:length(risk),risk, type = "l", col = "red")

}
