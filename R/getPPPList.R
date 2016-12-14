
#' get PPP list from a single page
#'
#' @param url A url, usually it's the offical ppp website of Ministry of Finance of China
#' @param page The page number
#' @param proxy if you wnat to use a proxy to avoid blocking, you can input a proxy, otherwise leave
#' it blank.
#' @return A table of PPP projects collected from your input page
#' @importFrom httr POST use_proxy timeout content
#' @importFrom jsonlite fromJSON
# @examples
# add(1, 1)
# add(10, 1)


getPPPList_unit <- function(url, page, proxy = NULL){

  res <- POST(url,
              encode="form",
              body=list(queryPage=page,
                        distStr="",
                        induStr="",
                        investStr="",
                        projName="",
                        sortby="",
                        orderby="",
                        stageArr=""), use_proxy(proxy[1, 1], proxy[1, 2]), timeout(15))
  resC <- content(res, as = 'text', encoding = 'utf-8')
  # But RJSONIO only gives list not dataframe
  # jsonlite will have encoding problems here, need to change sys encoding

  info <- fromJSON(resC, flatten = TRUE)$list
  return(info)
}




#' get PPP list from an official website
#'
#' @param endPage On which page you want to stop scrapping
#' @param startPage on Which page you want to start, default is 1
#' @return A table of PPP projects collected from your input page
# @examples
# add(1, 1)
# add(10, 1)

getPPPList <- function(startPage = 1, endPage) {
  # get proxy from special website, and every time scrapes, it will have 300 proxies, so
  # the limit of the random number is 300. Need to check this every some time
  page <- startPage # set up initial value
  times <- 0
  url <- 'http://www.cpppc.org:8082/efmisweb/ppp/projectLivrary/getPPPList.do?tokenid=null'

  # first generate proxy for scapring
  proxyPool <- getProxy()[,1:2]
  startTime <- Sys.time() # Get the start time, if it exceeds 1 hour, load proxy again.



  # Since for this case we got 301 proxies, so map the proxy table
  proxyIndex <- 1 #proxyIndex starts from1
  message('There might by error messages when you choose to use proxy, just ignore them.
          When it stayed for a long time, just click "stop", to start another round')



  repeat {

    PPPList <- tryCatch({

      getPPPList_unit(url = url, page, proxy = proxyPool[proxyIndex,])

    },error = function(cond) {
      message(paste('\n', Sys.time(), " Proxy doestn't work or ...\n"))
      message(cond)
      return(1)
    })

    if (length(PPPList) == 1 | PPPList == 1) {      times <- 0
    proxyIndex <- proxyIndex + 1# if proxy does't work or 30 pages are scraped, change proxy

    if (proxyIndex == 301) {
      message('\nrefreshe proxy pool...')
      proxyPool <- getProxy()[,1:2]
      proxyIndex <- 1
    }
    } else {


      if (times == 0) {
        totalPPPList <- PPPList
      } else {
        # bind the new list to the total list
        totalPPPList <- rbind(totalPPPList, PPPList)
      }

      times <- times + 1
      page <- page + 1
      # here one more control, when page reaches 499, change proxy
      # But if using proxy, usually no need to worry about the ip block
      if (times == 1000000) {
        # Just in case, usually it will not reach that many times
        stop("1000000 times, change proxy...\n")
        # randomNum <- round(runif(1, 1, 300))
      } else {
        message(paste("page", page - 1))
      }
    }
    if (page > endPage) break
    endTime <- Sys.time()
    if (is.integer((endTime - startTime)/5400)) {
      message("\nRefresh proxy pool...")
      proxyPool <- getProxy()[,1:2]
    }
  }

  return(totalPPPList)
}
