\name{q.rates}
\alias{q.rates}

\title{
Estimate Quiescence-Growth Model Rates
}
\description{
Given an mle estimate of the distribution parameters transform into rates of growth and quiescence. 
}
\usage{
q.rates(dist, est)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dist}{This distribution to use for estimation, e.g. "emg" or "norm"}
  \item{est}{An object of class \code{\link[stats4:mle-class]{mle-class}} containing distribution estimates.}
}

\value{
A vector containing (d) the growth rate and (q) the quiescence rate.
}
\references{
The method of determining growth rate is taken from \cite{POWELL E.0. (1956). Growth Rate and Generation Time of Bacteria, with Special Reference to Continuous Culture. J.Gen.Microbial V15,492-511}. This makes a robust estimator in the presence of skewed distributions.
}
\author{
  Darren Tyson, Shawn Garbett
}


\seealso{
\code{\link[stats4:mle-class]{mle-class}}
\code{\link{q.mle.norm.estimate}}
\code{\link{q.mle.emg.estimate}}
\code{\link{qsurvival.nllik}}
}

\examples{
  data(ca1d.erlotinib)
  mitotic.lifespans <-
    subset(ca1d.erlotinib, !End.of.Expt & 
                           !Death        & 
                           !is.na(Lifespan))$Lifespan
  censored.lifespans <- 
    subset(ca1d.erlotinib, End.of.Expt  & 
                           !Death        & 
                           !is.na(Lifespan))$Lifespan
  q.rates("emg", q.mle.emg.estimate(mitotic.lifespans, censored.lifespans))
}
