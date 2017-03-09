getExchange_unit <- function(stockName) {
  engine <- v8()
  url1 <- 'http://so.hexun.com/ajax.do?key='
  url2 <- '&type=all?math=0.6109926008061326'
  url <- paste(url1, stockName, url2, sep = '')

  #  a <- content(GET(url))

  res <- content(GET(url), as = 'text') %>%
    stri_replace_first_fixed("hxSuggest_JsonData=", "\"list\":")

  res1 <- paste('{', res, '}', sep = '')

  resC <- fromJSON(res1)


}
