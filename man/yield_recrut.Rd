% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yield_recrut.R
\name{yield_recrut}
\alias{yield_recrut}
\title{Diagnosis on stock from F estimate}
\usage{
yield_recrut(
  a,
  b,
  Linf,
  K,
  t0,
  list_age,
  Mat_F,
  Mat_M,
  mf,
  F0.1 = F,
  Mat_W = NA
)
}
\arguments{
\item{a}{Weight-size relationship parameter}

\item{b}{Weight-size relationship parameter}

\item{Linf}{Asymptotic length}

\item{K}{Growth parameter}

\item{t0}{Growth parameter}

\item{Mat_F}{Vector of F}

\item{mf}{F multiplicator range}

\item{Mat_W}{Can be used instead to growth paramters}
}
\description{
\code{yield_recrut} function to be optimized by stats::optimize, hence gives an estimation by iteration of F minimum value
}
\examples{

a <- 0.006
b <- 3.08

Linf <- 44.4
K <- 0.35
t0 <- -0.24

list_age <- seq(1, 10, by = 1)

Mat_F <- c(40.102259e-05, 4.102259e-05, 5.102259e-05, 4.818126e-04 ,1.173966e-03, 20.173184e-02, 9.366615e-02, 2.059620e-01, 3.454309e-01, 2.906350e-01, 4.469356e-01, 3.430000e-01)
mf <- seq(0, 2, by=0.05)

yield_recrut(a, b, Linf, K, t0, list_age, Mat_F, Mat_M, mf)

}
