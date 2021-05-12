

# Get water quality data


#' get PPP list from a single page
#'
#' @param year In which year you would like to scrape
#' @param week In which week you would like to scrape
#' @param station1 the start station index on the page
#' @param station2 the end station index on the page
#' @param proxy if you wnat to use a proxy to avoid blocking, you can input a proxy, otherwise leave
#' it blank.
#' @return A table of PPP projects collected from your input page
#' @importFrom httr GET timeout content
#' @importFrom rvest html_table %>%
#' @importFrom xml2 read_html
#' @references
#' http://datacenter.mee.gov.cn/report/getCountGraph.do?type=runQianWater
#' @export
# @examples
# add(1, 1)
# add(10, 1)
getWaterQ_MEP_all_unit <- function(year, week, station1, station2, proxy = NULL) {
  #url <- 'http://datacenter.mee.gov.cn/report/water/water.jsp?year=2016&wissue=45&x=29&y=6'
  url <- 'http://datacenter.mee.gov.cn/report/getCountGraph.do?type=runQianWater'
  res <- GET(url,
             query = list(year = year,
                          wissue = week), use_proxy(proxy[1, 1], proxy[1, 2]))
  resC <- content(res, as = 'text', encoding = 'utf-8')%>%
    read_html()%>%
    html_table(fill = TRUE)

  table <- resC[[1]]
  startIndex <- which(table[,1] == station1)
  endIndex <- which(table[,1] == station2)

  table <- table[startIndex:endIndex, 1:14]
  return(table)
}






#' get PPP list from a single page
#'
#' @param year In which year you would like to scrape
#' @param week In which week you would like to scrape, can be an array, like 3:5
#' @param station1 the start station index on the page
#' @param station2 the end station index on the page
#' @param proxy Whether to use proxy, default is FALSE
#' @details
#' Get monitoring data of different stations from Minitsry of Environmental Protection of China (http://datacenter.mep.gov.cn/report/getCountGraph.do?type=runQianWater). Using this function
#' you will get data of all the stations. Since the number of stations vary with time, using this function, you have
#' to make sure that within the period you are scrapping, the number of stations keep consistant.
#' @references
#' http://datacenter.mee.gov.cn/report/getCountGraph.do?type=runQianWater
#' @export
#' @importFrom data.table rbindlist
#' @examples
#'
#' \dontrun{
#' # get data from 1st station to 5th station of the 3rd week of 2016
#' a <- getWaterQ_MEP_all(2016, 3, 1, 5)
#'
#' }
#'

getWaterQ_MEP_all <- function(year, week, station1, station2, proxy = FALSE){
  message('Since the number of monitoring stations changes with time, so make sure in your
          scraping period, the number of monitoring stations is consistent.')

  if (length(year) != 1) message('Caution!!! the result can be wrong if you input more than 1 year, since the number
                                 of stations change with time.')
  times <- 0
  startTime <- Sys.time() # Get the start time, if it exceeds 1 hour, load proxy again.



  # deal with proxy
  proxyIndex <- 1
  if (proxy == TRUE) {
    proxyPool <- getProxy()[,1:2]
  } else if (proxy == FALSE) {
    proxyPool <- NULL
  } else {
    message("Wrong input, it's TRUE or FALSE")
  }
  page <- min(week)

  repeat {

    table <- tryCatch({

      getWaterQ_MEP_all_unit(year = year, week = page, station1 = station1,
                             station2 = station2, proxy = proxyPool[proxyIndex, ])
    },error = function(cond) {
      message(paste('\n', Sys.time(), " Proxy doestn't work or ...\n"))
      message(cond)
      return(1)
    })

    if (length(table) == 1) {
      times <- 0
      proxyIndex <- proxyIndex + 1

      if (proxyIndex == 300) { # there are 300 proxies at a time, if reaches 300, need to refresh
        message('\nrefreshe proxy pool...')
        proxyPool <- getProxy()[,1:2]
        proxyIndex <- 1
      }

    } else {

      if (times == 0) {
        totalTable <- table
      } else {
        # bind the new list to the total list
        totalTable <- rbindlist(totalTable, table)
      }

      times <- times + 1
      page <- page + 1
      # here one more control, when page reaches 499, change proxy
      # But if using proxy, usually no need to worry about the ip block
      if (times == 1000000) {
        # Just in case usually it will not reach this time
        stop("1000000 times, change proxy...\n")
        # randomNum <- round(runif(1, 1, 300))
      } else {
        message(paste("\nweek", page - 1))
      }

    }
    if (page > max(week)) break
    endTime <- Sys.time()
    if (is.integer((endTime - startTime)/5400)) {
      message("\nRefresh proxy pool...")
      proxyPool <- getProxy()[,1:2]
    }
  }

  return(totalTable)
}
