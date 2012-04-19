\name{prolif_d}
\alias{prolif_d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Expected Propagating Subpopulation
}
\description{
Computes the expected count of a subpopulation that is propagating inside population undergoing quiescent growth.
}
\usage{prolif_d(t, x0, q0=0, d, q, ad, aq, tol=1e-6)}

\arguments{
  \item{t}{A vector time points to return expected population size.}
  \item{x0}{The size of the initial propagating (or dividing) population at t=0.}
  \item{q0}{The size of the quiescent or non-propagating population at t=0.}
  \item{d}{The rate of growth of the propagating population.}
  \item{q}{The rate at which propagating population members join the quiescent population.}
  \item{ad}{The rate of death from the dividing or propagating population.}
  \item{aq}{The rate of death from the quiescent population.}
  \item{tol}{The tolerance between parameters before switching to limit models. The solution to the quiescent growth model has some boundary cases that would result in a division by zero. These happen when the rate of growth (d) is equal to rate of quiescence (q). When the absolute difference between these parameters is less than the tolerance, the model switches to the limit cases. The default value is good for most common cases. Preferably a very small number, that must be greater than zero.
  }
}
\value{
A numerical vector of expected dividing subpopulation totals.
}

\author{
  Shawn Garbett
}



\seealso{
  \code{\link{prolif_q}}
  \code{\link{prolif_tot}}
  \code{\link{prolif_fq}}
  \code{\link{prolif_fd}}
  \code{\link{q.rates}}
}

\examples{
prolif_d(1:100, 0.5, 0.5, 0.04, 0.03, 0.001, 0.001)
}
