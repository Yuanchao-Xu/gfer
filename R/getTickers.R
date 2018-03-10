
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
    stop("Please input full name(s) of a company or a column of companies, must be a column, not a row")
  }

  return(res)

}


