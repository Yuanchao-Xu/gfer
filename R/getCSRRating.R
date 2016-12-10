# get CSR rating
#' get CSR rating from a website
#' @param startPage on Which page you want to start, default is 1
#' @param endPage On which page you want to stop scrapping
#' @return A table of CSR ratings collected from your input page
#' @importFrom V8 v8
#' @importFrom httr content
#' @importFrom stringi stri_replace_last_fixed
#' @importFrom jsonlite fromJSON
# @examples
# add(1, 1)
# add(10, 1)

doCSRCorp <- function(startPage, endPage) {
  page <- startPage
  engine <- v8()


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
