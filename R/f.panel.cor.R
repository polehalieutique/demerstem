#' Bayesian function (not used for the moment)
#' \code{panel.cor} fonction graphique
#' @param x : A faire
#' @param y  : A faire
#' @param digit: A faire
#' @param prefix : A faire
#' @param  cex.cor: A faire
#' @examples
#'
#'  f.density.bivar ()
#' @export
#'

panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- (cor(x, y))
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
    cex=2
    # text(0.5, 0.5, txt, cex = cex * r)
	text(0.5, 0.5, txt, cex = cex)
}
