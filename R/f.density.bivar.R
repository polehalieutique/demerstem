#' Bayesian function (not used for the moment)
#' \code{f.density.bivar} fonction graphique
#' @param x : A faire
#' @param y  : A faire
#' @param nlevels: A faire
#' @param nb.points : A faire
#' @examples
#'
#'  f.density.bivar ()
#' @export
#'
f.density.bivar <- function(x,y, nlevels,nb.points)
{
x_name <- x$var_name
y_name <- y$var_name
x <- x[[1]]
y <- y[[1]]
indice <- which(x<=quantile(x, prob = 0.995))
x <- x[indice]
y <- y[indice]

xrange <- range(x) ; nbreaks.x=100
yrange <- range(y) ; nbreaks.y=100

xhist <- hist(x, breaks=seq(xrange[1],xrange[2],length.out=nbreaks.x), plot=FALSE)
yhist <- hist(y, breaks=seq(yrange[1],yrange[2],length.out=nbreaks.y), plot=FALSE)

nf <- layout(matrix(c(2,0,1,3),2,2,byrow=TRUE), c(3,1), c(1,3), TRUE)
#layout.show(nf)

par(mar=c(5,5,1,1))
plot(x[1:nb.points], y[1:nb.points], xlim=xrange, ylim=yrange,
	xlab="", ylab="", pch=".", cex.lab = 1.5)
dens2d <- kde2d(x=x, y=y, n = 100)
contour(dens2d , nlevels = nlevels, drawlabels=F, col = "red", lw = 2, add = T)
mtext(text=x_name, side=1, line=3, cex=1.3)
mtext(text=y_name, side=2, line=3, cex=1.3)

par(mar=c(0,5,3,1))
barplot(xhist$density, axes=FALSE, space=0, horiz=FALSE, col = "lightblue")
mtext(text=paste("Marginal pdf for ",x_name), side=3, line=1, cex=1.3)

par(mar=c(5,0,1,3))
barplot(yhist$density, axes=FALSE, space=0, horiz=TRUE, col = "lightblue")
mtext(text=paste("Marginal pdf for ",y_name), side=4, line = 1, las = 0, cex=1.3)
}
