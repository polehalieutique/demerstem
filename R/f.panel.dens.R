#' Bayesian function (not used for the moment)
#' \code{panel.dens} fonction graphique
#' @param x : A faire
#' @examples
#'
#'  panel.dens()
#' @export
#'

panel.dens <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- density(x,adjust=1)
    h$y<-h$y/max(h$y)
    xlim=range(h$x)
    lines(h, col="black", ...)
}
