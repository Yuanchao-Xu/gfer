#' Get proxy pool from free proxy provider
#' @details
#' Extract proxies from http://www.free-proxy-list.net/, in case of the risk of being blocked by the scrapped website
#' @references
#' www.free-proxy-list.net
#' @return The sum of \code{x} and \code{y}.
#' @importFrom rvest html_table %>%
#' @importFrom xml2 read_html


getProxy <- function() {
  # get free proxy from http://www.free-proxy-list.net/
  url <- 'http://www.free-proxy-list.net/'   #xpath = //*[@id="proxylisttable"]
  # url <- 'http://www.kuaidaili.com/proxylist/3/'
  table <- url %>%
    read_html() %>%
    #    html_nodes(xpath = '//*[@id="index_free_list"]/table/tbody') %>%
    html_table()

  return(table[[1]])
}
