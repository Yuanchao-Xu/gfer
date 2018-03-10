
#' get a company's historical market cap, data comes from NetEase
#'
#' @param ticker ticker/sympol of a company, MUST BE A CHARACTER, '006027' INSTEAD OF '6027'
#' @param date1 starting date, in the following format "20160101", means Jan 1st of 2016
#' @param date2 ending date, in the following format "20160101", if you only want one day's data, just set starting date and ending date the same day
#' @details
#' Data comes from www.money.163.com
#' @references
#' www.money.163.com
#' @return A data table with companies total capitalization and market capitalization
#' @importFrom utils read.csv

getHisMktCap_unit <- function(ticker, date1, date2) {


  url1 <- 'http://quotes.money.163.com/service/chddata.html?'

  # Cuz in Neteast, 0 represents SH exchange, 1 represents SZ exchange
  if (substr(ticker, 1, 1) == 6|substr(ticker, 1, 1) == 9) {
    url2 <- paste('code=', '0', ticker[[1]], sep = '')
  } else {
    url2 <- paste('code=', '1', ticker[[1]], sep = '')
  }


  url3 <- paste('&start=', date1, '&end=', date2, sep = '')
  url4 <- '&fields=TCAP;MCAP'

  url <- paste(url1, url2, url3, url4, sep = '')

  res <- read.csv(url)
  if (nrow(res) == 0) {
    newRes <- data.frame('no_data', as.character(ticker), 'no_data', 'no_data', 'no_data')
    names(newRes) <- names(res)
    res <- newRes
  }
  return(res)
}




#' get a company's historical market cap, data comes from NetEase
#'
#' @param tickers ticker/sympol of a company, TICKERS MUST BE CHARACTERs, '006027' INSTEAD OF '6027'
#' @param date1 starting date, in the following format "20160101", means Jan 1st of 2016
#' @param date2 ending date, in the following format "20160101", if you only want one day's data, just set starting date and ending date the same day
#' @details
#' The input date interval should have at least one work day
#' Data comes from www.money.163.com
#' @references
#' www.money.163.com
#' @export
#' @return A data table with companies total capitalization and market capitalization
#' @examples
#' \dontrun{
#' getHisMktCap(601857, '20161202', '20161203')
#' }
#'
#'
getHisMktCap <- function(tickers, date1, date2) {
  tickers <- tickers[[1]]
  if (length(tickers) == 1) {
    res <- getHisMktCap_unit(tickers, date1, date2)
  } else if (length(tickers) > 1) {

    # if the result comes from getTickers, it's factor, need to be converted
    # if (is.factor(tickers) == TRUE) {
    #   tickers <- as.character(levels(tickers))[tickers]
    # }
    #

    for (i in 1:length(tickers)) {
      res1 <- getHisMktCap_unit(tickers[i], date1, date2)
      if (i == 1) {
        res <- res1
      } else {
        res <- rbindlist(list(res, res1))
      }
      message(i)
    }

  } else {
    stop('Please input a column of tickers, column, not row')
  }
  return(res)
}
