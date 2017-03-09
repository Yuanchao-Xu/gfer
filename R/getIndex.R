#' get a company's market cap, data comes from NetEase
#'
#' @param indexPool a pool of different index, special format for gfer
#' @return A data table with companies total capitalization and market capitalization
#'
#'

getIndexConstnt <- function(indexPool) {
  for (i in 1:nrow(indexPool)) {
    url <- as.character(indexPool[i, 2])
    # index information comes from 2 different source by now, so it needs to
    # detect wether it's from sina or etnet
    if (grepl('sina.com', url)) {
      listNum <- 4
    } else if (grepl('etnet.com', url)){
      listNum <- 1
    } else break ('Wrong url in indexPool')

    data <- html_table(read_html(url), fill = TRUE)[[listNum]][, 1:2]
    data <- data[2:nrow(data), ]
    data[,3] <- rep(indexPool[i,1], nrow(data))
    colnames(data) <- c('ticker', 'name', 'index')
    rownames(data) <- NULL

    if (i == 1) {
      tData <- data
    } else {
      tData <- rbindlist(list(tData, data))
    }
  }

  return(tData)
}


#' get a company's market cap, data comes from NetEase
#'
#' @param ticker ticker/sympol of a company
#' @return A data table with companies and which index they are included
#' @examples
#' getIndex(600601)
#'
#'


getIndex <-function(tickers) {

  #creat an index pool
  indexPool <- data.frame(index = c('CSI_100', 'SSE_50', 'CSI_300', 'SSE_Central_SOEs_50',
                                    'HSI', 'HSCEI'),
                          url = c('http://vip.stock.finance.sina.com.cn/corp/go.php/vII_NewestComponent/indexid/000903.phtml',
                                  'http://vip.stock.finance.sina.com.cn/corp/go.php/vII_NewestComponent/indexid/000016.phtml',
                                  'http://vip.stock.finance.sina.com.cn/corp/go.php/vII_NewestComponent/indexid/000300.phtml',
                                  'http://vip.stock.finance.sina.com.cn/corp/go.php/vII_NewestComponent/indexid/000042.phtml',
                                  'https://www.etnet.com.hk/www/tc/stocks/indexes_detail.php?subtype=HSI',
                                  'https://www.etnet.com.hk/www/tc/stocks/indexes_detail.php?subtype=CEI'
                          )
  )

  data <- getIndexConstnt(indexPool)

  indexTable <- data.table(tickers)

  emptyCols <- data.frame(matrix(0, nrow = nrow(tickers), ncol =nrow(indexPool)))

  indexTable[, c(as.character(indexPool$index)) := emptyCols]

  for (i in tickers) {

  }

}
