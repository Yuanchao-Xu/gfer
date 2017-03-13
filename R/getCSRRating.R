# get CSR rating for unit page
#' get CSR rating from a website for a unit page
#' @param page on Which page you want to scrap
#' @param proxy whether use the proxy, default is FALSE
#' @param date represents the date is until which date, usually it's the last day of a year
#' e.g., "2015-12-31" for the date of year 2015, "2014-12-31" for the date of year 2014
#' @details
#' Get CSR ratings and reports of different companies from http://stockdata.stock.hexun.com/zrbg/
#' @return A table of CSR ratings collected from your input page
#' @importFrom V8 v8
#' @importFrom rvest html_table %>%
#' @importFrom httr content use_proxy
#' @importFrom stringi stri_replace_last_fixed stri_replace_first_fixed
#' @importFrom jsonlite fromJSON
#' @references
#' www.hexun.com
#' @export
getCSRRating_unit <- function(page, date, proxy = NULL) {
  engine <- v8()
  url <- 'http://stockdata.stock.hexun.com/zrbg/data/zrbList.aspx?'

  res <- GET(url, query = list(date = date,
                               count = 20,
                               pname = 20,
                               titType = 'null',
                               page = page), use_proxy(proxy[1, 1], proxy[1, 2]), timeout(15))

  resC <- content(res) %>%
    stri_replace_first_fixed("hxbase_json1(", "var dat=") %>%
    stri_replace_last_fixed(")", "") %>%
    engine$eval()


  resC <- engine$get('dat')$list
  return(resC)
}





# get CSR rating
#' get CSR rating from a website
#' @param startPage on Which page you want to start, default is 1
#' @param endPage On which page you want to stop scrapping
#' @param year In which year you want the rank
#' @param proxy whether use the proxy, default is FALSE
#' @details
#' Get CSR ratings and reports of different companies from http://stockdata.stock.hexun.com/zrbg/
#' @return A table of CSR ratings collected from your input page
#' @importFrom V8 v8
#' @importFrom httr content use_proxy
#' @importFrom stringi stri_replace_last_fixed stri_replace_first_fixed
#' @importFrom jsonlite fromJSON
#' @importFrom data.table rbindlist
#' @references
#' www.hexun.com
#' @examples
#' \dontrun{
#' # get first two pages of CSR ratings in 2015
#' getCSRRating(1,3)
#' }
#' @export



getCSRRating <- function(startPage, endPage, year = 2015, proxy = FALSE) {
  date <- paste(year, '12', '31', sep = '-')
  page <- startPage # set up initial value
  times <- 0
  if (proxy == TRUE) {
    proxyPool <- getProxy()[,1:2]
    message('There might by error messages when you choose to use proxy, just ignore them.
          When it stayed for a long time, just click "stop", to start another round')
  } else if (proxy == FALSE) {
    proxyPool <- NULL
  } else {
    message("Wrong input, it's TRUE or FALSE")
  }
  proxyIndex <- 1 #proxyIndex starts from1
  startTime <- Sys.time() # Get the start time, if it exceeds 1 hour, load proxy again.

  repeat {

    unitList <- tryCatch({

      getCSRRating_unit(page, date = date, proxy = proxyPool[proxyIndex,])

    },error = function(cond) {
      message(paste('\n', Sys.time(), " Proxy doestn't work or ...\n"))
      message(cond)
      return(1)
    })

    if (length(unitList) == 1) {
      times <- 0
      proxyIndex <- proxyIndex + 1# if proxy does't work or 30 pages are scraped, change proxy

      if (proxyIndex == 301) {
        message('\nrefreshe proxy pool...')

        if (proxy == TRUE) {
          proxyPool <- getProxy()[,1:2]
        } else if (proxy == FALSE) {
          proxyPool <- NULL
        } else {
          message("Wrong input, it's TRUE or FALSE")
        }

        proxyIndex <- 1
      }

    } else {


      if (times == 0) {
        totalList <- unitList
      } else {
        # bind the new list to the total list
        totalList <- rbindlist(totalList, unitList)
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
      if (proxy == TRUE) {
        proxyPool <- getProxy()[,1:2]
      } else if (proxy == FALSE) {
        proxyPool <- NULL
      } else {
        message("Wrong input, it's TRUE or FALSE")
      }
    }
  }

  return(totalList)
}
