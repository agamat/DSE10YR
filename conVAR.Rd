% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/DSE10YR.R
\name{conVAR}
\alias{conVAR}
\title{Continuously compounded Value at Risk of company}
\usage{
conVAR(return5, W, P, mu2, sd2)
}
\arguments{
\item{return5}{The vector containing the daily returns of the company in the year(s)}

\item{W}{The amount invested}

\item{P}{The confidence interval}

\item{mu2}{The mean return on the company's stock}

\item{sd2}{The standard deviation of the company's stock}
}
\value{
The total amount risked at that confidence interval on the specified sum invested
}
\description{
This function returns the continuously compounded Value at Risk of the company.
}
\details{
This function takes the daily returns, mean and standard deviation and returns the total monetary unit amount at risk
of loss on the specified sum invested.
}
\author{
Syed M. Fuad
}
\seealso{
\code{qnorm}, \code{exp}
}

