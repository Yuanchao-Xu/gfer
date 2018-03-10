
#' get a company's English name
#'
#' @param ticker ticker/sympol of a company, MUST BE A CHARACTER, '006027' INSTEAD OF '6027'
#' @details
#' Data comes from hexun.com
#' @references
#' http://hexun.com
#' @return A data table with companies' EN names
#' @importFrom xml2 read_html
#'

getENNames_unit <- function(ticker) {


  url1 <- 'http://stockdata.stock.hexun.com/gszl/s'
  url2 <- '.shtml'


  url <- paste(url1, ticker, url2, sep = '')


  res <- tryCatch({

    url %>% read_html() %>% html_table()

  },error = function(cond) {
    return(cond)
  })

  if (grepl('input error', res)) return(NA)

  ENName <- res[[1]][4, 2]

  return(ENName)
}




#' get a company's EN names
#'
#' @param tickers ticker/sympol of a company, TICKERS MUST BE CHARACTERs, '006027' INSTEAD OF '6027'
#' @details
#' Data comes from hexun.com
#' @references
#' http://hexun.com
#' @export
#' @return A data table with companies' EN names
#' @examples
#' \dontrun{
#' getENNames(601857)
#' }
#'
#'
getENNames <- function(tickers) {
  tickers <- tickers[[1]]
  if (length(tickers) == 1) {
    res <- getENNames_unit(tickers)
  } else if (length(tickers) > 1) {

    # if the result comes from getTickers, it's factor, need to be converted
    # if (is.factor(tickers) == TRUE) {
    #   tickers <- as.character(levels(tickers))[tickers]
    # }
    #

    for (i in 1:length(tickers)) {
      res1 <- getENNames_unit(tickers[i])
      if (i == 1) {
        res <- res1
      } else {
        res <- c(res, res1)
      }
      message(i)
    }

  } else {
    stop('Please input a column of tickers, column, not row')
  }
  return(res)
}
