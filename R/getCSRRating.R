# get CSR rating
#' get CSR rating from a website
#' @param startPage on Which page you want to start, default is 1
#' @param endPage On which page you want to stop scrapping
#' @param date represents the date is until which date, usually it's the last day of a year
#' e.g., "2015-12-31" for the date of year 2015, "2014-12-31" for the date of year 2014
#' @param proxy the proxy, default is NULL
#' @details
#' Get CSR ratings and reports of different companies from http://stockdata.stock.hexun.com/zrbg/
#' @return A table of CSR ratings collected from your input page
#' @importFrom V8 v8
#' @importFrom httr content use_proxy
#' @importFrom stringi stri_replace_last_fixed stri_replace_first_fixed
#' @importFrom jsonlite fromJSON
#' @examples
#' \dontrun{
#' # get first two pages of CSR ratings in 2015
#' getCSRRating(1,3)
#' }
#' @export



getCSRRating <- function(startPage, endPage, date = '2015-12-31', proxy = NULL) {
  page <- startPage
  engine <- v8()
  url <- 'http://stockdata.stock.hexun.com/zrbg/data/zrbList.aspx?'


  repeat {
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

    if (page == 1) {
      totalList <- resC
    } else {
      # bind the new list to the total list
      totalList <- rbind(totalList, resC)
    }

    page <- page + 1

    if (page > endPage) break

  }

  return(totalList)
}
