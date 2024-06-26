% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LBB.R
\name{LBB}
\alias{LBB}
\title{Length-based Bayesian biomass estimator (LBB)}
\usage{
LBB(
  lfq,
  startYear = NA,
  endYear = NA,
  years = NA,
  binSize = NA,
  LinfUser = NA,
  LcutUser = NA,
  LcUser = NA,
  LstartUser = NA,
  MKUser = NA,
  mmUser = FALSE,
  GausSel = FALSE,
  MergeLF = FALSE,
  n.chains = 3,
  n.cluster = 3,
  plot = FALSE,
  mfrow = NA
)
}
\arguments{
\item{lfq}{A list of the class "lfq" consisting of following parameters:
\itemize{
  \item \strong{species} species name,
  \item \strong{stock} stock ID or name,
  \item \strong{midLengths} midpoints of the length classes,
  \item \strong{dates} dates of sampling times (class Date),
  \item \strong{catch} matrix with catches/counts per length class (row)
     and sampling date (column),
  \item \strong{comments} comments;
}}

\item{startYear}{Start year of assessment. If NA (default), the first year in the
\code{lfq$dates} is used.}

\item{endYear}{Final year of assessment. If NA (default), the last year in the
\code{lfq$dates} is used.}

\item{years}{Manual selection of years for assessment. If NA (default), all years
in the \code{lfq$dates} are used.}

\item{binSize}{Optional; determines bin size (class width) for length-frequency data.
If NA (default) bin size remains unchanged (as in \code{lfq$midLengths}).}

\item{LinfUser}{Optional; user defined asymptotic length. Any length observation larger than
this value will be removed from the data. If NA (default), Linf is estimated by
the model.}

\item{LcutUser}{Optional, user defined minimum length. Any length observation smaller than
this value will be removed form the data. By default all length obervations are used
(\code{LcutUser} = NA).}

\item{LcUser}{Optional, user defined selectivity parameter. If NA (default) L10 and L90 are
used to estimate a proxy for Lc.}

\item{LstartUser}{Optional, user defined length at which selectivity is 0.95. If NA (default)
Lstart (L95) is estimated by the model.}

\item{MKUser}{Optional; user defined MK ratio. If NA (default) MK ratio is set to 1.5.}

\item{mmUser}{Logical; indicating the unit of length measurements, where TRUE
indicates that lengths are in mm and FALSE (default) indicate that lengths are in cm.}

\item{GausSel}{Logical; indicating the selectivity pattern. If FALSE (default) trawl-like,
if TRUE gaussian selectivity is assumed.}

\item{MergeLF}{Logical; indicating if the data of subsequent years should be merged
with data of preceeding years. (Default: FALSE).}

\item{n.chains}{Number of Markov chains (default: 3).}

\item{n.cluster}{Number of clusters to use to run parallel chains (default: 3).}

\item{plot}{Logical; should the individual year plot be displayed? (Default: FALSE).}

\item{mfrow}{A vector of the form 'c(nr, nc)'.  Subsequent figures will be drawn in an
'nr'-by-'nc' array on the device by _rows_ ('mfrow'). If NA (default), a panel with
3 columns and several rows (dependent on number of years) is used.}
}
\value{
A list with the input parameters and following list objects:
\itemize{
  \item \strong{GausSel}: indicating if gaussian Selection was used,
  \item \strong{priors}: priors,
  \item \strong{refLev}: matrix with all reference levels for all years,
  \item \strong{medianRefLev}: median reference levels (plus 95\% confidence intervals),
  \item \strong{lastRefLev}: reference levels in the last year,
  \item \strong{LFall}: matrix with lengths and relative frequencies.
}
}
\description{
This function is copied from TropFishR 1.6.0. It has been slightly modified in lines 150, 1175 and 1350 by examining if lfq$catch class was a matrix (using `any(class(lfq$catch)) == "matrix"`)
}
\details{
LBB

\code{LBB}

Requires the Gibbs sampler JAGS to be installed on your
    computer, available for your Operating System from the
    following web site:
    \href{http://sourceforge.net/projects/mcmc-jags/files/JAGS/4.x/}{http://sourceforge.net/projects/mcmc-jags/files/JAGS/4.x/}.
    LBB is a new method for the analysis of length frequency data
    from the commercial fishery. It works for species that grow
    throughout their lives, such as most commercial fish and
    invertebrates, and requires no input in addition to length
    frequency data. It estimates asymptotic length (Linf), length
    at first capture (Lc), relative natural mortality (M/K) and
    relative fishing mortality (F/M) as means over the age range
    represented in the length-frequency sample. With these
    parameters as input, standard fisheries equations can be used
    to estimate depletion or current exploited biomass relative to
    unexploited biomass (B/B0). In addition, these parameters allow
    the estimation of the length at first capture that would
    maximize catch and biomass for the given fishing effort
    (Lc_opt), and estimation of a proxy for the relative biomass
    capable of producing maximum sustainable yields
    (Bmsy/B0). Relative biomass estimates of LBB were not
    significantly different from the "true" values in simulated
    data and similar to independent estimates from full stock
    assessments.
}
\examples{
\donttest{
## load data
data(synLFQ8)

## arrange lfq data
lfq <- lfqModify(synLFQ8, aggregate = "year")

## plot lfq data traditionally
plot(lfq)

## plot data in LBB manner
plotLBB.data(lfq)

## add length at maturity to lfq data
lfq$Lm50 <- 36

## run LBB model
res <- LBB(lfq, plot = TRUE)

## plot results
plotLBB(res)

## plot time series
plotLBB.ts(res)
}

# IMPORT

import R2jags
import rjags
}
\references{
R. Froese, H. Winker, G. Coro, N. Demirel, A.C. Tsikliras, D. Dimarchopoulou,
G. Scarcella, W.N. Probst, M. Dureuil, and D. Pauly (2018) A new approach
for estimating stock status from length frequency data. ICES Journal of Marine Science. DOI: 10.1093/icesjms/fsy078
}
\author{
Rainer Froese, (\email{rfroese@geomar.de})
}
