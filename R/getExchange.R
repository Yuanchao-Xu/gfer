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


  for (i in tickers) {
    url <- paste(url1, i, url2, sep = '')
    a <- GET(url)
    b <- content(a, as = 'text')

  }



}


