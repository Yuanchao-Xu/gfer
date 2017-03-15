#' get a company's listed location
#'
#' @param tickers ticker/sympol of a company, TICKERS MUST BE CHARACTERs, '006027' INSTEAD OF '6027'
#' @details
#' Data comes from www.finance.sina.com.cn
#' @references
#' www.finance.sina.com.cn
#' @export
#' @return A data table with a listed companies' ticker, security name and listed exchange location
#' @examples
#' \dontrun{
#' getExchange('600601')
#' getExchange(c('00005', '00001'))
#' }


getExchange <- function(tickers) {

  # this is the way to get data from hexun

  # url1 <- 'http://so.hexun.com/ajax.do?key='
  # url2 <- '&type=all?math=0.6109926008061326'
  # url <- paste(url1, stockName, url2, sep = '')
  #
  # #  a <- content(GET(url))
  #
  # res <- content(GET(url), as = 'text') %>%
  #   stri_replace_first_fixed("hxSuggest_JsonData=", "\"list\":")
  #
  # res1 <- paste('{', res, '}', sep = '')
  #
  # resC <- fromJSON(res1)


  # using sina finance is easier

  url1 <- 'http://suggest3.sinajs.cn/suggest/type=&key='
  url2 <- '&name=suggestdata_1489564633542'


  # counter
  n <- 1

  for (i in tickers) {
    url <- paste(url1, i, url2, sep = '')
    a <- GET(url)
    b <- content(a, as = 'text')
    c <- strsplit(b, ',')

    index <- which (c[[1]] == i)
    if (length(index) == 0) {
      warning(paste('found nothing for', i))
      res <- c(i, 'noInfo', 'noInfo')
    } else {
      name1 <- c[[1]][index[1] + 1]
      name2 <- c[[1]][index[1]]

      # get difference
      loc <- Reduce(setdiff, strsplit(c(name1, name2), split = ""))
      # paste them together
      loc <- paste(loc, sep = '', collapse = '')

      if (loc == '' & nchar(c[[1]][index[1] + 1]) == 5) {
        loc <- 'hk'
      }

      info <- c(i, c[[1]][index[1] + 2], loc)
    }
    if (n == 1) {
      res <- info
    } else {
      res <- rbind(res, info)
    }

    n <- n + 1
  }

  # if the res only has one row, dimension has be to assigned
  if (is.null(dim(res))) dim(res) <- c(1, 3)
  res <- data.table(res)
  colnames(res) <- c('Tickers', 'Name', 'location')
  return(res)

}


