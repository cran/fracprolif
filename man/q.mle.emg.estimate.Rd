\name{q.mle.emg.estimate}
\alias{q.mle.emg.estimate}

\title{
Estimate Distribution Parameters based on EMG survival
}
\description{
Estimation of the emg distribution parameters from data using a survival function and maximum likelihood.
}
\usage{
q.mle.emg.estimate(complete.lifespans, censored.lifespans)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{complete.lifespans}{
Vector of time observations of complete lifespans that span the entire life of an entity.
}
  \item{censored.lifespans}{
Vector of time observations that are incomplete about the life of an entity.
}

}

\value{
An object of \code{\link[stats4]{mle-class}} containing the parameter estimates.
}

\author{
  Shawn Garbett
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[stats4:mle-class]{mle-class}}
\code{\link{q.mle.norm.estimate}}
\code{\link{q.rates}}
\code{\link{qsurvival.nllik}}
\code{\link[emg:emg-package]{emg}}
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
  q.mle.emg.estimate(mitotic.lifespans, censored.lifespans)
}


