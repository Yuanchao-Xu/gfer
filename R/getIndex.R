#' get a company's market cap, data comes from NetEase
#'
#' @param indexPool a pool of different index, special format for gfer
#' @importFrom data.table rbindlist
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
#' @param tickers ticker/sympol of a company, MUST BE A CHARACTER, e.g., input "006600" instead of 006600
#' The tickers have to be FULL AND EXACT, e.g., for Shanghai exchange and Shenzhen exchange, the input must have 6 digits, and for HK exchange, it must
#' have 5 digits. the '0' in the beginning cannot be left out.
#' @param indexData the index information, before running getIndex, indexData needs to be loaded using \code{\link{getIndexData}}
#' @details Data comes from www.finance.sina.com.cn and www.etnet.com.hk
#' @importFrom data.table data.table
#' @references
#' www.finance.sina.com.cn
#' www.etnet.com.hk
#'
#' @return A data table with companies and which index they are included
#' @examples
#' \dontrun{
#' indexData <- getIndexData()
#' getIndex(600601, indexData)
#' }
#' @export
#'


getIndex <-function(tickers, indexData) {

  if (!(is.data.frame(tickers)|is.numeric(tickers)|is.character(tickers))) {
    warning('Your input better be a data.frame...')
  }



  n <- 1
  for (i in tickers[[1]]) {

    ####################### assign to indexArray
    indexArray <- rep(0, (length(unique(indexData$index)) + 1))
    names(indexArray) <-  c('ticker', as.character(unique(indexData$index)))

    subData <- with(indexData, {
      indexData[ticker == i]
    })

    indexArray[as.character(subData$index)] <- 1
    indexArray[1] <- i
    #######################
    if (n == 1) {
      res <- indexArray
    } else {
      # since indexArray is not a dataframe, use rbind instead of rbindlist
      res <- rbind(res, indexArray)
    }
    message(n)
    n <- n + 1
  }
  rownames(res) <- NULL
  return(res)
}



#' get index information
#' Currently include CSI 100, SSE 50, CSI 300, SSE Central SOEs 50, HSI, HSCEI
#' @export
#' @return a data table containing index information
getIndexData <- function(){
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

  indexData <- getIndexConstnt(indexPool)
  return(indexData)
}
