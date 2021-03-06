% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fluserf.R
\name{fluserf}
\alias{fluserf}
\title{fluserf: Fit a Serfling Model on Time Series Data}
\usage{
fluserf(data=NULL, outc=NULL, epi, time=NULL, 
              period=52, echo=F, alpha=.05)
}
\arguments{
\item{data}{A dataframe class object, must contain time variable,
epidemic indicator, and measure of influenza morbidity}

\item{outc}{an unquoted name of a column in data which corresponds to
the outcome variable of interest}

\item{epi}{an unquoted name of a column in data object (e.g. epi)
or if null will default to Sept - May.}

\item{time}{an unquoted name of a column in data object, must be
a numeric/integer class variable in dataframe which corresponds
to a unit of time, must be unique (i.e. non-repeating)}

\item{period}{a numeric vector indicating period length, i.e. 52 weeks in year}

\item{echo}{A logical parameter, if T. Will print variables used in model.}

\item{alpha}{Specify level for one-sided test to compute upper limit interval.
default is 0.05}
}
\value{
an object of class data.frame, input + y0 (fitted values), y0_ul
the upper serfling threshold
}
\description{
Performs a cyclical linear regression model
}
\examples{
require(flumodelr)
fludta <- flumodelr::fludta
flu_fit <- fluserf(fludta, outc = fludeaths, time = yrweek_dt)  
              
head(flu_fit)

}
\references{
Serfling RE. Methods for current statistical analysis of
excess pneumonia-influenza deaths. Public Health Rep. 1963 Jun;
78(6): 494 - 506.
/url{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1915276/}
}
