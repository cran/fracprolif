\name{qsurvival.nllik}
\alias{qsurvival.nllik}

\title{Negative Log-Likelihood of a Survival Function
}
\description{
Using a survival function on a specified distribution with a given set of parameters, computes the negative log-likelihood
}
\usage{
qsurvival.nllik(dist, complete.lifespans, censored.lifespans, Q, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dist}{A text description of the distribution to use, e.g. emg or norm}
  \item{complete.lifespans}{A vector of observations of complete lifespans}
  \item{censored.lifespans}{A vector of incomplete observations of lifespans}
  \item{Q}{The percent of population in quiescence or non-dividing}
  \item{...}{Specific parameters to the \code{dist} specified}
}

\value{
A numeric value of the negative log-likelihood for the given parameters.
}
\author{
Darren Tyson, Shawn Garbett
}

\seealso{
\code{\link[stats4:mle-class]{mle-class}}
\code{\link{q.mle.norm.estimate}}
\code{\link{q.mle.emg.estimate}}
\code{\link{q.rates}}
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
                           
  mle(function(mean, sd, Q){
       qsurvival.nllik("norm", mitotic.lifespans, censored.lifespans, Q, mean, sd)
     },
     method='L-BFGS-B',
     lower=list(mean=8,  sd=0.1, Q=0.01),
     upper=list(mean=30, sd=20,  Q=0.9),
     start=list(mean=mean(mitotic.lifespans), 
                sd=sd(mitotic.lifespans),
                Q = 0.5))
}