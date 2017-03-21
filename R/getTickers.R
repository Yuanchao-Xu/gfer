
#' get ticker by input a company's full name
#'
#' It can also be a way to test if a company is listed
#'
#' @param corpName Full name of a company
#' @details
#' Data comes from www.cninfo.com.cn/
#' @return A data table with companies stock name and stock ticker
#' @importFrom httr GET content



getTickers_unit <- function(corpName) {
  url <- paste('http://www.cninfo.com.cn/cninfo-new/fulltextSearch/full?searchkey=', corpName, '&sdate=&edate=&isfulltext=false&sortName=nothing&sortType=desc&pageNum=1', sep = '')
  a <- GET(url)
  b <- content(a)

  # check if something get back from the server
  # and also has to check if there are at least 3 results, cus later on, it
  # will pick up the 3rd result
  if (length(b$announcements) == 0 | length(b$announcements) < 3) {

    warning (paste(corpName, 'could be a wrong name, recheck please'))
    res <- data.frame(secName = 'wrong_name',secCode = 'wrong_name', doubleCheck = corpName)
  } else if (is.null(b$announcements[[3]]$secName)) {
    warning (paste(corpName, 'could be a wrong name, recheck please'))
    res <- data.frame(secName = 'wrong_name',secCode = 'wrong_name', doubleCheck = corpName)
  } else {

    # list number can be 1-10, but usually the 1st will be the full name,
    # then comes the security name, so set 3, safer
    secNameSplit <- unlist(strsplit(b$announcements[[3]]$secName, split = ','))

    # if contains '\u503a', probably a bond
    index <- which(grepl("[[:digit:]]|\u503a", secNameSplit) == FALSE)


    if (length(index) == 0) {
      warning(paste(corpName, 'has no information in www.cninfo.com'))
      res <- data.frame(secName = 'no_info',secCode = 'no_info', doubleCheck = corpName)
    } else {
      # check if only one index is back
      if (length(index) != 1) warning(paste(corpName, 'has more than two tickers, please double check.'))


      secName <- secNameSplit[index]

      secCode <- unlist(strsplit(b$announcements[[3]]$secCode, split = ','))[index]

      # through code to filter bonds
      if (substr(secCode, 1, 2) == '12') warning(paste(corpName, 'could be a bond, double check'))
      doubleCheck <- b$announcements[[3]]$announcementTitle

      res <- data.frame(secName, secCode, doubleCheck)
    }
  }

  return(res)
}




#' get ticker by input a company's full name or a list of companies' full name
#'
#' It can also be a way to test if a company is listed
#' NOTE: If a company is listed in multiple exchange, then it needs double check,
#' the programe only chooses ticker from random exchange
#'
#' @param corpNames Full name of a company, should be full name
#' @details
#' Data comes from www.cninfo.com.cn/
#' @references
#' www.cninfo.com.cn
#' @return A data table with companies stock name and stock ticker
#' @importFrom data.table rbindlist
#'
#'
#'
#'
#' @export



getTickers <- function(corpNames) {
  # here must be a column of company names
  if (is.null(nrow(corpNames))) {
    res <- getTickers_unit(corpNames)
  } else if (nrow(corpNames) > 1) {
    # since listed companies are limited, no need to use data.table

    for (i in 1:nrow(corpNames)) {
      res1 <- getTickers_unit(corpNames[i,])
      if (i == 1) {
        res <- res1
      } else {
        res <- rbindlist(list(res, res1))
      }
      message(i)
    }
  } else {
    break("Please input full name(s) of a company or a column of companies, must be a column, not a row")
  }

  return(res)

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
