

#' Check if a company is listed in Chinese stock market
#'
#'
#' @param corpList company list you want to check if listed, should be a dataframe
#' @param stockList Result from \code{\link{getStockList}}
#'
#'
#' @importFrom data.table data.table
#' @export
#' @references
#' http://info.cmbchina.com/Stock/Single/

is.listed <- function(corpList, stockList) {

  res <- lapply(corpList[[1]], function(x) {
    a <- with(stockList, {stockList[FullName == x]})
    if (nrow(a) == 0) {
      a <- data.table(Ticker = 'noinfo', StockName = 'noinfo', Fullname = 'noinfo', Abbreviation = 'noinfo',
                      Exchange = 'noinfo')
    } else if (nrow(a) > 1)
      warning(paste(x, 'has more than one result, only first will be shown'))

    return(a[1,])
  })

  resC <- rbindlist(res)
  # attach corpList to compare and double check
  resC$input <- corpList
  return(resC)
}

#' Get information from Shanghai Exchange and Shenzhen Exchange.
#' This will only get stock information in Shanghai Exchange and Shenzhen Exchange
#' Including stocker ticker, stock name and company full name. Data comes from China Merchants Bank
#'
#' @export
#' @importFrom httr GET

#' @references
#' http://info.cmbchina.com/Stock/Single/
getStockList <- function() {
  exchangeInfo <- data.frame(name = c('szA', 'szB', 'shA', 'shB'))
  exchangeInfo$Feature <- c('sza', 'szb', 'sha', 'shb')
  #total page number for stock exchanges c(27, 3, 74, 3)
  exchangeInfo$PageNO <- c(2, 3, 2, 3)

  finalList <- mapply(getStockList_unit, exchangeInfo$Feature, exchangeInfo$PageNO, exchangeInfo$name,
                      SIMPLIFY = FALSE)
  res <- rbindlist(finalList)
  return(res)
}


#' @importFrom rvest html_table %>%
#' @importFrom xml2 read_html
#' @importFrom data.table rbindlist
getStockList_unit <- function(feature, tPage, exName) {
  url1 <- 'http://info.cmbchina.com/Stock/Single/?channel=Single&'
  res <- lapply(1:tPage, function(i) {
    url <- paste(url1, 't=', feature, '&page=', i, sep = '')
    a <- read_html(url) %>%
      html_table()
    a <- a[[1]]
    a <- a[2:nrow(a), ]
    message(paste('page', i))
    return(a)
  })

  resC <- rbindlist(res)
  colnames(resC) <- c('Ticker', 'StockName', "FullName", 'Abbreviation')
  resC$Exchange <- rep(exName, nrow(resC))

  message(paste(feature, 'Done'))


  return(resC)
}



# ##########################






#  following is to scrape all the company information, but since it takes too much time, temporarily suspended.
#
#
# is.listed <- function(){
#   url <- 'http://www.cninfo.com.cn/cninfo-new/information/companylist'
#   a <- GET(url)
#   b <- content(a, as = 'text')
#
#
#   exchangeinfo <- data.table(name = c('szMain', 'szMiddleSmall', 'startUp', 'shMain', 'hkMain', 'hkStartUp'))
#   exchangeinfo$Feature <- c('<div id="con-a-1" class="hover">', '<div id="con-a-2" style="display:none">',
#                             '<div id="con-a-3" style="display:none">', '<div id="con-a-4" style="display:none">',
#                             '<div id="con-a-5" style="display:none">', '<div id="con-a-6" style="display:none">')
#
#   exchangeinfo$start <- sapply(exchangeinfo$Feature, function(x) regexpr(x, b))
#
#   exchangeinfo$end <- c(exchangeinfo$start[2:nrow(exchangeinfo)], nchar(b))
#
#   exchangeList <- mapply(function(x, y) substr(b, x, y),exchangeinfo$start, exchangeinfo$end)
#
#   message('Note: since it will load the full list from internet, it will take about 20 minutes...')
#
#   finalList <- mapply(function(x, y) getStockFullName(x, y), exchangeList, exchangeinfo$name)
#
#   res <- rbindlist(finalList)
#
#
#   return(res)
#
# }
#
# getStockFullName <- function(exchangeList, exch) {
#   a <- strsplit(exchangeList, 'href=\"')[[1]]
#   a <- a[2:length(a)]
#
#   tickers <- unlist(lapply(a, function(x) {
#     n <- regexpr('">', x)
#     return(substr(x, n+2, n+7))
#   }))
#
#   names <- unlist(lapply(a, function(x) {
#     n1 <- regexpr('">', x)
#     n2 <- regexpr('</a>', x)
#     return(substr(x, n1+9, n2-1))
#   }))
#
#   links <- unlist(lapply(a, function(x) {
#     n <- regexpr('">', x)
#     return(substr(x, 1, n-1))
#   }))
#
#   # next from the links get the full name
#   fullnames <- unlist(lapply(links, function(x) getStockFullName_unit(x)))
#
#   exchange <- rep(exch, length(tickers))
#
#
#   res <- data.table(ticker = tickers, name = names, fullname = fullnames, exchange = exchange)
#   print(paste(exch, 'Done!'))
#   return(res)
# }
#
#
# getStockFullName_unit <- function(link) {
#   url1 <- 'http://www.cninfo.com.cn/information/brief/'
#   url2 <- unlist(strsplit(link, 'fulltext?', fixed = T))[2]
#   url3 <- '.html'
#   url <- paste(url1, url2, url3, sep = '')
#
#   fullname <- tryCatch({
#     html_table(read_html(url))[[2]][1, 2]
#   }, error = function(cond) {
#     message(paste(url2, 'has connection issues'))
#     return('error')
#   })
#
#   print(fullname)
#   return(fullname)
# }
