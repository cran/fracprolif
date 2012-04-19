\name{split}
\alias{split_tot}
\alias{split_d}
\alias{split_q}
\alias{split_fq}
\alias{split_fd}


\title{
Piecewise curves for rates which change in the middle of an experiment
}
\description{
This helper functions provide the same functionality as the \code{\link{prolif_tot}} set of functions, but with the additional assumption that rates change at some breakpoint during the dataset. 
}
\usage{
split_tot(t, x0, q0, d1, d2, q1, q2, ad1, aq1, ad2, aq2, br=20)
split_d(  t, x0, q0, d1, d2, q1, q2, ad1, aq1, ad2, aq2, br=20)
split_q(  t, x0, q0, d1, d2, q1, q2, ad1, aq1, ad2, aq2, br=20)
split_fd( t, x0, q0, d1, d2, q1, q2, ad1, aq1, ad2, aq2, br=20)
split_fq( t, x0, q0, d1, d2, q1, q2, ad1, aq1, ad2, aq2, br=20)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{t}{A vector time points to return expected population size.}
\item{x0}{The size of the initial propagating (or dividing) population at t=0.}
\item{q0}{The size of the quiescent or non-propagating population at t=0.}
\item{d1}{The rate of growth of the propagating population in the first section.}
\item{d2}{The rate of growth of the propagating population in the second section.}
\item{q1}{The rate at which propagating population members join the quiescent population in the first section.}
\item{q2}{The rate at which propagating population members join the quiescent population  in the second section.}
\item{ad1}{The rate of death from the dividing or propagating population in the first section.}
\item{aq1}{The rate of death from the quiescent population in the first section.}
\item{ad2}{The rate of death from the dividing or propagating population  in the second section.}
\item{aq2}{The rate of death from the quiescent population  in the second section.}
\item{br}{The time breakpoint for the change in rate}
}

\value{
A numerical vector of expected (sub)population totals (or fraction)
}
\references{
The method of determining growth rate is taken from \cite{Powell E.0. (1956). Growth Rate and Generation Time of Bacteria, with Special Reference to Continuous Culture. J.Gen.Microbial V15,492-511}. This makes a robust estimator in the presence of skewed distributions.
}
\author{
  Darren Tyson, Shawn Garbett
}


\seealso{
\code{\link{prolif_tot}}
\code{\link{prolif_d}}
\code{\link{prolif_q}}
\code{\link{prolif_fd}}
\code{\link{prolif_fq}}
}
