#' Loading dataset for any year between 2005 and 2015
#' 
#' This function returns the stock price of all companies enlisted and traded in Dhaka Stock Exchange for the year.
#' 
#' @param id1 The year for which the dataset is required
#' @return Data matrix containing date, high, low, average and closing price for all companies traded that year
#' @author Syed M. Fuad
#' @details This function takes in a year between 2005 to 2015 and returns a data matrix containing the date,
#' high, low, average, closing price, trade volume and turnover for each company traded in that year.
#' @seealso \code{list.files}, \code{read.csv}
#' @export

load <- function(id1, summarize = FALSE) {
  
  filename <- list.files(pattern="*.csv")
  
  data <- read.csv( paste(id1,".csv",sep="") )
  return (data) 
}

#' Loading dataset for a company in any year between 2005 and 2015
#' 
#' This function returns stock price for any specified company enlisted and traded in Dhaka Stock Exchange.
#'
#'  @param year The years for which the data is required
#'  @param coname The company name inside double inverted commas as it exactly appears in the .csv file
#'  @return Data matrix containing date, high, low, average and closing price for all companies traded that year
#'  @author Syed M. Fuad
#'  @details This function takes in a year between 2005 to 2015 and the company name and returns a data matrix containing 
#'  the date, high, low, average, closing price, trade volume and turnover for the company traded in that year.
#'  @seealso \code{subset}
#'  @export

call <- function(year, coname){
  period <- load(year)
  com <- subset(period, period$COMPANYNAME == coname)
  return(com)
}

#' Daily returns for a company enlisted in the Dhaka Stock Exchange
#' 
#' This function returns the daily return for any company enlisted in the Dhaka Stock Exchange in any year between 2005 and 2015.
#' 
#' @param id The vector containing closing price for any company
#' @return Vector containing the daily returns of the company
#' @author Syed M. Fuad
#' @details This function takes a vector containing the closing price and returns the daily returns.
#' @seealso \code{length}
#' @export
 
dailyret <- function(id, summarize = FALSE){
  n <- length(id)
  ret <- (id[2:n] - id[1:(n-1)])/(id[1:(n-1)])
  return(ret)
}

#' Continuously compunded returns for a company in Dhaka Stock Exchange
#' 
#' This function returns continuously compunded returns for a company enlisted and traded in the Dhaka Stock Exchange between 2005 
#' and 2015.
#' 
#' @param id2 The vector containing closing price for any company
#' @return Vector containing the continuously compunded returns of the company
#' @author Syed M. Fuad
#' @details This function takes a vector containing the closing price and returns the continuously compunded returns.
#' @seealso \code{log}, \code{length}
#' @export

continret <- function(id2, summarize = FALSE){
  n <- length(id2)
  ret1 <- log(id2[2:n]) - log(id2[1:(n-1)])
  return(ret1)
}

#' Growth of $1 invested in the company in the year/years specified
#' 
#' This function returns the value of $1 invested in the company in the time period stated.
#' 
#' @param x A vector containing the daily returns of a company in a particular year(s)
#' @return A numeric of the value of $1 invested in the company in the year(s)
#' @author Syed M. Fuad
#' @details This function takes a vector containing the the daily returns and returns the value of $1 invested in the company.
#' @seealso \code{cumprod}
#' @export

growth <- function(x){
  gross <- x+1
  dollar <- cumprod(gross)
}

#' Value at Risk of company
#' 
#' This function returns the Value at Risk of the company.
#' 
#' @param return5 The vector containing the daily returns of the company in the year(s)
#' @param W The amount invested
#' @param P The confidence interval
#' @param mu1 The mean return on the company's stock
#' @param sd1 The standard deviation of the company's stock
#' @return The total amount risked at that confidence interval on the specified sum invested
#' @author Syed M. Fuad
#' @details This function takes the daily returns, mean and standard deviation and returns the total monetary unit amount at risk
#' of loss on the specified sum invested.
#' @seealso \code{qnorm}
#' @export
#' 

VAR <- function(return5, W, P, mu1, sd1) {
  var <- W*qnorm(P, mean=mu1, sd=sd1)
  return(var)
}

#' Continuously compounded Value at Risk of company
#' 
#' This function returns the continuously compounded Value at Risk of the company.
#' 
#' @param return5 The vector containing the daily returns of the company in the year(s)
#' @param W The amount invested
#' @param P The confidence interval
#' @param mu2 The mean return on the company's stock
#' @param sd2 The standard deviation of the company's stock
#' @return The total amount risked at that confidence interval on the specified sum invested
#' @author Syed M. Fuad
#' @details This function takes the daily returns, mean and standard deviation and returns the total monetary unit amount at risk
#' of loss on the specified sum invested.
#' @seealso \code{qnorm}, \code{exp}
#' @export

conVAR <- function(return5, W, P, mu2, sd2) {
  var1 <- W*(exp(qnorm(P, mean=mu2, sd=sd2))-1)
  return(var1)
}
