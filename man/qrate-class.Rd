\name{qrate-class}
\Rdversion{1.1}
\docType{class}
\alias{qrate-class}
\alias{show,qrate-method}
\alias{summary,qrate-method}

\title{Class \code{"qrate"}}
\description{
A class to return from rate estimation.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("qrate", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{This contains two entries, the division rate and the quiescence rate.}
    \item{\code{stderr}:}{Standard error of the rate estimates}
    \item{\code{n}:}{Number of data points used in estimate}
    \item{\code{df}:}{degrees of freedom left in estimate}
  }
}
\section{Extends}{
Class \code{"\linkS4class{numeric}"}, from data part.
Class \code{"\linkS4class{vector}"}, by class "numeric", distance 2.
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "qrate")}: ... }
	 }
}
\references{
Standard errors are derived by the delta method. See: Casella, G. and Berger, R. L. (2002), Statistical Inference, 2nd ed.
}
\author{
Shawn Garbett
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{q.rates}}
}
\keyword{classes}
