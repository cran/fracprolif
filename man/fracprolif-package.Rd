\name{fracprolif-package}
\alias{fracprolif-package}
\alias{fracprolif}
\docType{package}
\title{
Fractional Proliferation.
}
\description{
This package contains all the functions necessary for analyzing population size based on the behavior of individual members within the population using a methodology termed Fractional Proliferation. The methodology was specifically designed to analyze the behavior of a population of cultured cells and requires fitting data to a newly developed model of proliferation, the Quiescence-Growth Model (see below). This package contains functions to determine rates of division (d) quiescence (q) and apoptosis (a) as well as predict the change in the fraction of cells within the dividing and nondividing compartments over time.
}

\section{Quiescence-Growth Model}{
The Quiescence-Growth Model is based on an exponentially dividing population that transition into a nondividing population at a specified rate. This model is different from Gompertz and logisitic models of proliferation that assume a limiting resource or factor that prevents proliferation above a fixed amount, the carrying capacity. In contrast, the Quiescence-Growth Model assumes that members of the population may stop dividing, e.g. in response to perturbations. The resulting proliferation curves from these models may appear very similar, but the underlying assumptions are different.

The Quiescence-Growth Model assumes that there are two populations, a proliferating population (x) and a non-proliferating population (y). The proliferating population has a rate of division (d), analogous to the exponential growth rate of a population. There is also a rate of quiescence (q), the rate at which members of the dividing population become non-dividing. Each subpopulation can have a different death rate (a_d and a_q). Such a model is formulated as follows using coupled ordinary differential equations:

\deqn{x' = (d-q-a_d)x}{x' = (d - q - a_d) x}

\deqn{y' = qx - a_q y}{y' = q x - a_q y}

Where d is the division rate, q is the quiescence rate and a_d and a_q are the respective death rates. This admits the following solution,

\deqn{x=x_0 e^{(d - q - a_d) t}}{x=x_0 exp((d - q - a_d) t) }

\deqn{
  y=\frac{1}{d-q+a_q-a_d}e^{-a_q t}
    \left( 
      q(x_0(e^{(d-q+a_q-a_d)t}-1)-y_0) + 
      (d + a_q - a_d) y_0
    \right)
}{
  y=exp(-a_q t) ( q(x_0*(exp((d-q+a_q-a_d)t)-1)-y_0) + 
  (d + a_q - a_d) y_0 ) / (d-q+a_q-a_d)
}

Fitting such a model with solid statistics depends greatly upon how the data are measured. Differing rates of death and quiescence can create similar curvatures in the resulting population model on shorter time scales. Because of this, accurate estimation of the rates requires more than a curve fit to the population counts using \code{\link[stats:nls]{nls}}. Direct measurement of individual behavior within the population provides the necessary information to accurately quantify these rates and predict the change in the size of each compartment (x and y) over time. The Fractional Proliferation method utilizes obtainable information at the individual and population levels to make these predictions. We include two example data sets to illustrate the approach: \code{\link{ca1d.erlotinib}} and \code{\link{ca1d.erlotinib.totals}}. These data sets were obtained by automated microscopy of a cancer cell line (CA1d) in the presence of a targeted therapeutic drug (erlotinib). Normally these cells exhibit minimal entry into quiescence, but in the presence of erlotinib the population of quiescent cells increases substantially.
}

\section{Example - Single-Cell Data}{

To begin with an example, load the observed single-cell data by copying and pasting the following code into the R console:

\preformatted{data(ca1d.erlotinib)}

To view the scatterplot of birthtime versus lifespan of the cell behaviors in 16microM erlotinib, copy and paste the example code below into the R console. (Note: All later examples are executable in the same manner.)

\preformatted{
d <- subset(ca1d.erlotinib, !Death & !is.na(Lifespan))
plot(d$Birthtime,
     d$Lifespan,
     main="CA1d 16micromolar erlotinib",
     xlab="Birth time (h)",
     ylab="Lifespan (h)",
     ylim=c(5,90), xlim=c(0,25))
n <- length(d$Lifespan)
text(20,80,substitute("n" == n,list(n=n)))
p <- 100*length(subset(d, End.of.Expt)$Lifespan) / n
text(15,85,substitute(p * "\% EoE", list(p=round(p))))
}

This plot shows the raw data with the birth time of a cell in the experiment plotted along the x-axis and the lifespan plotted along the y-axis. Note that the data include cells that die during the experiment because they also have an observed lifespan, as well as cells that are observed until the end of the experiment without having divided, which are seen along the downward sloped line at the top of the plot. The end of the experiment creates a censoring effect on the data in this regard, and we do not know if these cells eventually divide or not.
}

\section{Example - Intermitotic Time Distribution}{

To view the intermitotic time distribution (i.e. the distribution of cell cycle times) the data \code{\link{ca1d.erlotinib}} must first be split into \code{mitotic.lifespans}, which include cells that divide before the end of the experiment, and \code{censored.lifespans} that reach the end of the experiment without division, excluding death events. 

\preformatted{
mitotic.lifespans <- subset(ca1d.erlotinib,  !End.of.Expt &
                                             !Death       &
                                             !is.na(Lifespan))$Lifespan

censored.lifespans <- subset(ca1d.erlotinib, End.of.Expt  &
                                             !Death       &
                                             !is.na(Lifespan))$Lifespan
}

An exponentially growing population of CA1d cells have intermitotic times that can be described by an Exponentially Modified Gaussian (EMG) distribution (Nature Methods submitted and reference therein), a Gaussian with an exponential right tail.  This can be seen in the following plot:

\preformatted{

hist(mitotic.lifespans,
     main='CA1d 16micromolar erlotinib',
     sub="Observed Intermitotic Times (h)",
     breaks=20)
}

}

\section{Example - Quiescence Likelihood}{

A survival model with knowledge of the distribution, can estimate the distribution parameters and quiescent percentage. This uses the negative log likelihood of a survival model. This package provides a function, \code{\link{qsurvival.nllik}}, that will work with most of the core distributions provided by R. The following call demonstrates estimation with the sample data. The survival model assumes that the censored observations may or may not divide based on the observed divisions.

\preformatted{
est <- q.mle.emg.estimate(mitotic.lifespans, censored.lifespans)

summary(est)
}

Using this estimate we can visualize the fit with the following:
\preformatted{
hist(mitotic.lifespans,
     main='CA1d 16micromolar erlotinib',
     xlab="Observed Intermitotic Times (h)",
     breaks=20,
     freq=FALSE)
curve(demg(x, coef(est)['mu'], coef(est)['sigma'], coef(est)['lambda']), add=TRUE, col='red', lwd=2)
text(40, 0.07,  pos=4, labels=substitute("n"==n, list(n=length(mitotic.lifespans))))
text(40, 0.065, pos=4, labels=substitute(mu==m, list(m=round(coef(est)['mu'],2))))
text(40, 0.06,  pos=4, labels=substitute(sigma==s, list(s=round(coef(est)['sigma'],2))))
text(40, 0.055, pos=4, labels=substitute(kappa==l, list(l=round(1/coef(est)['lambda'],2))))
}

To test for lack of fit of the data to an EMG distribution, the following snippet performs the Kolmogorov-Smirnov test and adds the results to the previous plot:

\preformatted{
ks <- ks.test(mitotic.lifespans, "pemg", 
              mu=coef(est)['mu'],
              sigma=coef(est)['sigma'],
              lambda=coef(est)['lambda'])
        
text(40, 0.05, pos=4, labels=substitute("ks p-value"==p, list(p=round(ks$p.value,2))))
}


The \code{\link{q.mle.emg.estimate}} function call assumes an EMG distribution, but a normal function \code{\link{q.mle.norm.estimate}} is also provided, and the following snippet shows the internals of this function, which can be changed to accommodate any distribution dependence since the inner function \code{\link{qsurvival.nllik}} makes no assumptions about distribution.

\preformatted{
mle(function(mean, sd, Q){
      qsurvival.nllik("norm", mitotic.lifespans, censored.lifespans, Q, mean, sd)},
    method='L-BFGS-B',
    lower=list(mean=8,  sd=0.1, Q=0.01),
    upper=list(mean=30, sd=20,  Q=0.9),
    start=list(mean=mean(mitotic.lifespans), 
               sd=sd(mitotic.lifespans),
               Q = 0.5))
}


}
\section{Example - Division and Quiescence Rate Estimation}{

To estimate the rates of division (d) and quiescence (q), the distribution of the divided cell population is used, and, in this example, we will continue with the emg fit since the distribution's parameters have been estimated (see above). To transform the emg fit into a rate of division, we use the method of Powell (1956) and then subsequently derive the quiescence rate. Note this function is independent of the distribution used but must match the one used in the \code{mle} call (see above).

\preformatted{
r <- q.rates("emg", est)
r
}

}
\section{Example - Death Rate Estimation}{

Now we calculate the death rates from the compartments. In our example we first allow this parameter to float to optimally fit the model to the population counts using the previous estimates of division rate and quiescence rate. First, we load a set of population counts (in a log base 2 scale over time) of ca1d cells in the presence of various concentrations of erlotinib.

\preformatted{
data(ca1d.erlotinib.totals)
}

The single-cell data used to plot the intermitotic time distribution and calculate the rates of division and quiescence were obtained in the presence of 16microM erlotinib and the goal is to determine how well the estimates of the rates obtained from the single-cell data predict the population-level cell counts over time. However, the drug is known to affect cells within a particular phase of the cell cycle and does not immediately effect all cells in a population. Thus, population-level behavior switches from the rates of untreated cells to the treated cells after a delay of approximately half of the average cell cycle time. To estimate the rate of division of the untreated cells we assume that all cells are dividing and the rate of growth of the population is equal to the division rate. 

\preformatted{
m <- lm(DMSO ~ Time_h, ca1d.erlotinib.totals)
untreated.division.rate <- coef(m)['Time_h']*log(2)
}

The estimated time before the drug takes effect is approximately half of the average intermitotic time (cell lifespan). 

\preformatted{
half.imt <- log(2)/untreated.division.rate/2
}

The death rate is initially fit using the Quiescence-Growth Model. When fitting the model, we do not know which compartment the cells die from, so we assign each compartment the same variable. Also the initial rates are those from the control population, i.e. zero for the quiescence and death rates.

\preformatted{
# y is in terms of doublings, i.e. base log(2)
y <- ca1d.erlotinib.totals$e_16000 - ca1d.erlotinib.totals$e_16000[1]
t <- ca1d.erlotinib.totals$Time_h

# Let the Death rates float
fit <- nls(y ~ log(split_tot(t, 1, 0, 
                             untreated.division.rate, r['d'],
                             0.0, r['q'],
                             0.0, 0.0, a, a,
                             half.imt), 2),
           start=list(a=0.003),
           lower=list(a=0),
           algorithm="port")
a <- coef(fit)
summary(fit)
}


}
\section{Example - Final Fractional Proliferation Graph}{

Now with the death rate parameters estimated, we can plot the resulting model as follows:

\preformatted{
plot (t, y, 
      main="CA1d 16micromolar erlotinib",
      xlab="Time (h)",
      ylab="Population Doublings",
      ylim=c(0,1.5))
      
model.total <- log(split_tot(t, 1, 0, untreated.division.rate, r['d'], 0.0, r['q'], 0, 0, a['a'], a['a'], half.imt), 2)
lines(t,model.total,col="green")

lines(t,model.total*split_fq(t, 1, 0, untreated.division.rate, r['d'], 0.0, r['q'], 0, 0, a['a'], a['a'], half.imt), col="red")
lines(t,model.total*split_fd(t, 1, 0, untreated.division.rate, r['d'], 0.0, r['q'], 0, 0, a['a'], a['a'], half.imt), col="blue")

text(2, 1.1, pos=4, substitute("d" == d, list(d=round(r['d'], 4))))
text(2, 1.0, pos=4, substitute("q" == q, list(q=round(r['q'], 4))))
text(2, 0.9, pos=4, substitute("a" == a, list(a=round(a['a'], 4))))
}

Plotted on a log(2) scale, which shows doublings, the circles are the experimental data. The lines are the time-dependent estimates of total size of the population (green line) and the dividing (blue line) and quiescent (red line) fractions of this population based on the Quiescence-Growth Model. Drug is introduced at time 0, and we assume that the population initially has no quiescence.

}
\author{
Darren Tyson, Shawn Garbett
Vanderbilt University

Maintainer: Shawn Garbett <Shawn@Garbett.org>
}

\references{
Darren R. Tyson, Shawn P. Garbett, Peter L. Frick and Vito Quaranta (2012) Fractional Proliferation: A method to deconvolve cell population dynamics from single-cell data. Nature Methods (In Press)

The method of determining growth rate is taken from \cite{Powell E.0. (1956). Growth Rate and Generation Time of Bacteria, with Special Reference to Continuous Culture. J.Gen.Microbial V15,492-511}. This makes a robust estimator in the presence of skewed distributions.

Ulm (1990). Simple method to calculate the confidence interval of a standardized mortality ratio (SMR). Am. J. Epidemiol. V131 (2): 373-375.
}

\keyword{ package, quiescence, growth }
\seealso{
\code{\link[emg:emg-package]{emg}}
\code{\link[stats:nls]{nls}}
}


