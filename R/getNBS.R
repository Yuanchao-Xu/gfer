#' getNBS
#'
#' get National Bureau of Statistics data
#'
#' @param start starting year of data wanted
#' @param end end year of data wanted, make sure your input end year exists in the NBS website
#' @param indicator of which data is fetched, indicator includes 'GDP', 'water resources', 'water use' and 'wastewater', etc.
#' @importFrom data.table rbindlist setcolorder
#' @references
#' Xuehui YANG (2016). rstatscn: R Interface for China National Data. R
#' package version 1.1.1. https://CRAN.R-project.org/package=rstatscn
#' @return no return
#' @export

getNBS <- function(indicator, start, end) {
  index <- switch(indicator,
                  'GDP' = 'A0201',
                  'water resources' = 'A0C03',
                  'water use' = 'A0C04',
                  'wastewater' = 'A0C05')
  GDP <- lapply(start:end, function(x) {
    a <- statscnQueryData(index, dbcode='fsnd', rowcode='reg', colcode='zb', moreWd=list(name='sj', value=x))
    a$Year <- x
    a$Province <- rownames(a)
    return(a)
  })

  GDP_total <- rbindlist(GDP)

  # re-order columns
  n1 <- ncol(GDP_total)
  newOrder <- c(n1, n1-1, 1:(n1-2))
  setcolorder(GDP_total, newOrder)

  # change province names

  col <- sapply(GDP_total$Province, function(x) strsplit(x, '\\.')[[1]][2])
  names(col) <- NULL
  GDP_total$Province <- col
  return(GDP_total)
}


#' updateNBS
#'
#' update/create the database in your google sheet. You have to sign in mannually for your google sheet
#' Once finished, there will be a google sheet called NBS_data created in your google drive as database.
#' @param start starting year of data wanted
#' @param end end year of data wanted, make sure your input end year exists in the NBS website
#' @import googlesheets
#' @export
updateNBS <- function(start, end) {
  message('Loading from NBS')
  GDP <- getNBS('GDP', start, end)
  wateruse <- getNBS('water use', start, end)
  wastewater <- getNBS('wastewater', start, end)

  #check if there are existing data base in the drive
  message('Uploading to Google Sheet. It may take minites depending on data size.')
  if (!'NBS_data' %in% gs_ls()[['sheet_title']]) {
    gs_new('NBS_data', ws_title = 'GDP', input = GDP, trim = TRUE, verbose = TRUE)

    gs_ws_new(gs_title('NBS_data'), ws_title = 'Water_use', trim = TRUE, verbose = TRUE)
    gs_ws_new(gs_title('NBS_data'), ws_title = 'Wastewater')

  } else {
    gs_edit_cells(gs_title('NBS_data'), ws = 'GDP', input = GDP, trim = TRUE, verbose = TRUE)
  }
  sheet <- gs_title('NBS_data')

  message('GDP upload finished.')
  gs_edit_cells(sheet, ws = 'Water_use', input = wateruse, trim = TRUE, verbose = TRUE)
  message('water use upload finished.')
  gs_edit_cells(sheet, ws = 'Wastewater', input = wastewater, trim = TRUE, verbose = TRUE)
  message('Wastewater upload finished.')
  message('All finished')
}

#' getWaternomicsData_NBS
#'
#' Get NBS data from NBS website.
#'
#' @param start starting year of data wanted
#' @param end end year of data wanted, make sure your input end year exists in the NBS website
#' @import data.table
#' @export

getWaternomicsData_NBS <- function(start, end) {
  message('Loading from NBS')
  GDP <- getNBS('GDP', start, end)
  wateruse <- getNBS('water use', start, end)
  wastewater <- getNBS('wastewater', start, end)
  #closeAllConnections()
  on.exit()


  selected <- c('Province', 'Year','Gross Regional Product', 'Value-added of the Primary Industry', 'Value-added of the Secondary Industry',
                'Value-added of the Tertiary Industry')
  GDP <- GDP[, selected, with = F]

  selected <- c('Province', 'Year', 'Total Use of Water ')
  wateruse <- wateruse[, selected, with = F]

  selected <- c('Province', 'Year', 'Total Waste Water Discharged')
  wastewater <- wastewater[, selected, with = F]

  res <- cbind(GDP, wateruse[, 3], wastewater[,3])

  # calculate x and y coordinates

  res$x <- res[, 8]/res[, 3]
  res$y <- res[, 7]/res[, 3]*10000
  res$r <- res[, 3]


  colnames(res)[1] <- 'label'



  # need further development
  return(res)
}




#' getWaternomicsData_goog
#'
#' Get NBS data from google sheet by shared link. Default link is provided by gfer, you can also create your
#' own google sheet of GDP.
#' NOTE: The 'link sharing on' of the sheet must be ticked in order to read
#' @importFrom gsheet gsheet2tbl
#' @export
getWaternomicsData_goog <- function() {
  message('Loading from database...')
  GDP <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1hkmciju6L3DQjoziFfhDMv3wS7Gpx1rJLW-Fdcykhck/edit#gid=1261170248')
  wateruse <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1hkmciju6L3DQjoziFfhDMv3wS7Gpx1rJLW-Fdcykhck/edit#gid=616646461')
  wastewater <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1hkmciju6L3DQjoziFfhDMv3wS7Gpx1rJLW-Fdcykhck/edit#gid=2060805027')
  #closeAllConnections()
  on.exit()


  selected <- c('Province', 'Year','Gross Regional Product', 'Value-added of the Primary Industry', 'Value-added of the Secondary Industry',
                'Value-added of the Tertiary Industry')
  GDP <- GDP[, selected]

  selected <- c('Province', 'Year', 'Total Use of Water')
  wateruse <- wateruse[, selected]

  selected <- c('Province', 'Year', 'Total Waste Water Discharged')
  wastewater <- wastewater[, selected]

  res <- cbind(GDP, wateruse[, 3], wastewater[,3])

  # calculate x and y coordinates

  res$x <- res[, 8]/res[, 3]
  res$y <- res[, 7]/res[, 3]*10000
  res$r <- res[, 3]


  colnames(res)[1] <- 'label'



  # need further development
  return(res)
}





# @param province choose one or some provinces you are interested. Be careful you
# have to type in the right name as below, e.g., it's 'Inner Mongolia', not 'Neimenggu':
#
# province: 'Beijing', 'Tianjin', 'Hebei', 'Shanxi', 'Inner Mongolia', 'Liaoning',
# 'Jilin', 'Heilongjiang', 'Shanghai', 'Jiangsu', 'Zhejiang', 'Anhui', 'Fujian', 'Jiangxi',
# 'Shandong', 'Henan', 'Hubei', 'Hunan', 'Guangdong', 'Guangxi', 'Hainan', 'Chongqing',
# 'Sichuan', 'Guizhou', 'Yunnan', 'Tibet', 'Shaanxi', 'Gansu', 'Qinghai', 'Ningxia', 'Xinjiang'











#library(jsonlite)
#library(httr)

# the code below sources from http://www.bagualu.net/wordpress/rstatscn-the-r-interface-for-china-national-data,
# thanks to the author Jinag Hang
# Because the above package only provides Chinese version, so I made a small change to the statscnbase link,
# changed it to EN version.
# The original and modified code below are under lisence Apache License 2.0 https://www.apache.org/licenses/LICENSE-2.0



statscnbase <- 'http://data.stats.gov.cn/english/easyquery.htm'
rstatscnEnv <- new.env()
# prefix also can be NULL, if it is a Chinese version.
# change it to nrow mainly because in English version there could be duplicated row names
#
assign('prefix', 'nrow', envir=rstatscnEnv)


#' private function for sec
#'
#' @return milsec
milSec <- function()
{
  tt <- Sys.time()
  ts <- format(tt,"%s")
  ts <- paste(ts,"000",sep = "")
  ts
}

#' the available dbs
#'
#' the available dbs in the national db
#' @return a data frame with 2 columns , one is the dbcode, another is the db description
#' @export
#' @references
#' Xuehui YANG (2016). rstatscn: R Interface for China National Data. R
#' package version 1.1.1. https://CRAN.R-project.org/package=rstatscn
#' @examples
#'  \dontrun{
#'  statscnDbs()
#'  }
#'
statscnDbs <- function()
{
  dbs <- c("hgnd","hgjd","hgyd","fsnd","fsjd","fsyd","csnd","csyd","gjnd","gjyd","gjydsdj")
  dbnames <- c("national data, yearly","national data,  quaterly","national data, monthly",
               "province data, yearly","province data, quaterly","province data, monthly",
               "city data, yearly","city data, monthly", "international data, yearly",
               "international data, monthly","3 main countries data, monthly")
  ret <- data.frame(dbcode = dbs, description = dbnames)
  return(ret)
}

#' private function for constructing the query parameter for dfwds
#'
#' @param wdcode string value , one of c("zb","sj","reg")
#' @param valuecode string value ,  following is the table for available valuecode
#'    zb:   the valudecode can be gotten by statscnQueryZb() function
#'    sj:   the valudecode can be "2014" for nd db,  "2014C" for jd db.
#'    reg:  the valudecode is the region code fetched by statscnRegions(dbcode) function
#' @references
#' Xuehui YANG (2016). rstatscn: R Interface for China National Data. R
#' package version 1.1.1. https://CRAN.R-project.org/package=rstatscn
#' @return return the queyr string for the http request
genDfwds <- function(wdcode,valuecode)
{
  if( is.na(valuecode) ){
    return("[]")
  }else{
    paste('[{"wdcode":"',wdcode,'","valuecode":"',valuecode,'"}]',sep = "")
  }
}
#' private function for check the http status
#'
#' @param ret the response obj returned by httr package
#' @importFrom httr http_status
#' @references
#' Xuehui YANG (2016). rstatscn: R Interface for China National Data. R
#' package version 1.1.1. https://CRAN.R-project.org/package=rstatscn
#' @return return nothing , but if it finds some error , it stop the script
checkHttpStatus <- function(ret)
{
  if (http_status(ret)$category != "Success") {
    stop(sprintf("Bad response from %s", statscnbase))
  }
}
#' private function to convert the returned jason data to a dataframe
#'
#' @param rawObj the fromJSON output
#' @param rowcode rowcode in the data frame
#' @param colcode colcode in the data frame
#' @references
#' Xuehui YANG (2016). rstatscn: R Interface for China National Data. R
#' package version 1.1.1. https://CRAN.R-project.org/package=rstatscn
#' @return the contructed data frame
dataJson2df <- function(rawObj,rowcode,colcode)
{
  ret <- rawObj
  if(ret[[1]] != 200) {
    stop("Bad response from the statscn server")
  }
  #dataStructure
  #jj is a list
  #jj[[1]] = 200 #return code
  #jj[[2]] is datanode
  #jj[[2]][[1]] is data
  #jj[[2]][[2]] is description
  #jj[[2]][[2]][,"nodes"][[1]] is row description , it is a dataframe
  #jj[[2]][[2]][,"nodes"][[2]] is col description , it is a dataframe
  desList <- ret[[2]][[2]][,'nodes']
  rowWdIdx <-  which(ret[[2]][[2]]$wdcode == rowcode)
  colWdIdx <- which(ret[[2]][[2]]$wdcode == colcode)
  rowDes <- desList[[rowWdIdx]]
  colDes <- desList[[colWdIdx]]

  rowNum <- nrow(rowDes)
  colNum <- nrow(colDes)
  rowNames <- rowDes[,1]
  colNames <- colDes[,1]

  units <- rowDes[,'unit']
  units <- ifelse(units == "", "", paste("(", units, ")", sep = ""))
  rowNames <- paste(rowNames, units, sep = "")

  prefix <- get('prefix', envir = rstatscnEnv)
  if(! is.null(prefix) ){
    mrows <- 1:length(rowNames)
    rowNames  <-  paste(mrows,rowNames,sep = ".")
  }

  rowCodes <- rowDes[,2]
  colCodes <- colDes[,2]

  #the rowCode and colCode are in the ret[[2]][[1]][,'wds']
  #it is a list , the list length is the same as the data fetched. list[[1]] is for the first data
  #list[[1]] is a dataframe ,  df[1,'valuecode'] is the rowcode , df[2,'valuecode'] is the colcode
  #now we create a dataframe
  myret <- as.data.frame(matrix(rep(NA,rowNum*colNum),nrow = rowNum))
  rownames(myret) <- rowCodes
  colnames(myret) <- colCodes
  dfdata <- ret[[2]][[1]]
  for (k in seq(1,nrow(dfdata))) {
    wddf <- dfdata[k,"wds"][[1]]
    myret[wddf[rowWdIdx,'valuecode'],wddf[colWdIdx,'valuecode']] <- dfdata[k,'data'][1,'data']
  }
  rownames(myret) <- rowNames
  colnames(myret) <- colNames
  return(myret)
}

#' the data categories
#'
#' the sub data categories for the zbid category, dbcode need to be specified, where the dbcode can be fetched by function
#' statscnDbs(). In the returned data frame, the column 'isParent' shows if each sub category is leap category or not
#' @param zbid the father zb/category id , the root id is 'zb'
#' @param dbcode which db will be queried
#' @return the data frame with the sub zbs/categories , if the given zbid is not a Parent zb/category, null list is returned
#' @importFrom httr POST
#' @importFrom jsonlite fromJSON
#' @export
#' @references
#' Xuehui YANG (2016). rstatscn: R Interface for China National Data. R
#' package version 1.1.1. https://CRAN.R-project.org/package=rstatscn
#' @examples
#' \dontrun{
#' statscnQueryZb()
#' statscnQueryZb('A01',dbcode="hgnd")
#' }
#'
#'
statscnQueryZb<-function(zbid = "zb",dbcode = "hgnd")
{
  curQuery <- list(id = zbid, dbcode = dbcode, wdcode = "zb", m = "getTree")
  yy <- POST(statscnbase, body = curQuery, encode = "form")
  assign('lastQuery', curQuery, envir = rstatscnEnv)
  checkHttpStatus(yy)
  jj <- fromJSON(content(yy, "text", encoding = "utf-8"))
  return(jj)
}
#' the regions in db
#'
#' the available regions in the specified db, it is used for query the province, city and country code generally
#' @param dbcode the dbcode should be some province db(fs*) , city db(cs*) or internaltional db(gj*)
#' @return the data frame with all the available region codes and names in the db
#' @importFrom jsonlite fromJSON
#' @export
#' @references
#' Xuehui YANG (2016). rstatscn: R Interface for China National Data. R
#' package version 1.1.1. https://CRAN.R-project.org/package=rstatscn
#' @examples
#' \dontrun{
#' statscnRegions('fsnd')
#' statscnRegions('csnd')
#' statscnRegions('gjnd')
#' }
#'
#'
#'
statscnRegions<-function(dbcode='fsnd')
{
  curQuery<-list(
    m = "getOtherWds",
    dbcode = dbcode,
    rowcode = "zb",
    colcode = "sj",
    wds = "[]",
    #dfwds="[]",
    k1 = milSec()
  )
  yy <- GET(statscnbase, query = curQuery)
  assign('lastQuery', curQuery, envir = rstatscnEnv)
  checkHttpStatus(yy)
  ret <- fromJSON(content(yy,"text",encoding = "utf-8"))
  regIndex <- which(ret[[2]]$wdcode == 'reg')
  df <- ret[[2]][,'nodes'][[regIndex]]
  df$sort <- NULL
  colnames(df) <- c("regCode","name")
  return(df)
}
#' query data in the statscn db
#'
#' the main function for querying the statscn database, it will retrieve the data from specified db and orginize the data in a data frame.
#' @param zb the zb/category code to be queried
#' @param dbcode the db code for querying
#' @param rowcode rowcode in the returned data frame
#' @param colcode colcode in the returned data frame
#' @param moreWd more constraint on the data
#'        where the name should be one of c("reg","sj") , which stand for region and sj/time.
#'        the valuecode for reg should be the region code queried by statscnRegions()
#'        the valuecode for sj should be like '2014' for *nd , '2014C' for *jd , '201405' for *yd.
#'        Be noted that , the moreWd name should be different with either rowcode or colcode
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @return the data frame you are quering
#' @references
#' Xuehui YANG (2016). rstatscn: R Interface for China National Data. R
#' package version 1.1.1. https://CRAN.R-project.org/package=rstatscn
#' @export
#' @examples
#'
#' \dontrun{
#' df <- statscnQueryData('A0201', dbcode = 'hgnd')
#' df <- statscnQueryData('A0201',dbcode = 'fsnd', rowcode = 'zb', colcode = 'sj',
#'                     moreWd = list(name = 'reg', value = '110000'))
#' }
#'
#'
statscnQueryData <- function(zb = "A0201", dbcode = "hgnd", rowcode = 'zb', colcode = 'sj', moreWd = list(name = NA, value = NA))
{
  curQuery <- list(
    m = "QueryData",
    dbcode = dbcode,
    rowcode = rowcode,
    colcode = colcode,
    wds = genDfwds(moreWd$name,moreWd$value),
    dfwds = genDfwds("zb",zb),
    k1 = milSec()
  )
  yy <- GET(statscnbase, query = curQuery)
  assign('lastQuery', curQuery, envir = rstatscnEnv)
  checkHttpStatus(yy)
  ret <- fromJSON(content(yy,"text", encoding = "utf-8"))
  return(dataJson2df(ret,curQuery$rowcode,curQuery$colcode))
}
#' fetch the lastN data
#'
#' fetch the lastN data for the latest query, only affect the number of rows in the returned data.
#' This function can not be used alone , statscnQueryData() has to be called before this function
#' @param n the number of rows to be fetched
#' @return the last n rows data in the latest query
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @references
#' Xuehui YANG (2016). rstatscn: R Interface for China National Data. R
#' package version 1.1.1. https://CRAN.R-project.org/package=rstatscn
#' @export
#' @examples
#' \dontrun{
#' df=statscnQueryData('A0201',dbcode='hgnd')
#' df2=statscnQueryLastN(20)
#' }
#'
#'
statscnQueryLastN<-function(n)
{
  wdcode <- "sj"
  valuecode <- paste("LAST",n,sep = "")
  if( is.null(get('lastQuery', envir = rstatscnEnv)) ){
    stop("please call a statscnQueryData for some data firstly")
  }
  curQuery <- get('lastQuery', envir = rstatscnEnv)
  if( curQuery$m=="QueryData" ) {
    curQuery$dfwds <- genDfwds(wdcode,valuecode)
  }
  yy <- GET(statscnbase, query = curQuery)
  assign('lastQuery', curQuery, envir = rstatscnEnv)
  checkHttpStatus(yy)
  ret <- fromJSON(content(yy, "text", encoding = "utf-8"))
  return(dataJson2df(ret,curQuery$rowcode,curQuery$colcode))
}

#' statscnRowNamePrefix
#'
#' set the rowName prefix in the dataframe
#'
#' in case you encounter the following error:
#'   Error in `row.names<-.data.frame`(`*tmp*`, value = value) :
#'   duplicate 'row.names' are not allowed
#' you need to call this function
#'
#' @param p , how to set the rowname prefix.
#'     it is 'nrow' by default , and it is the only supported value currently
#'     to unset the row name prefix, call this function with p=NULL
#' @references
#' Xuehui YANG (2016). rstatscn: R Interface for China National Data. R
#' package version 1.1.1. https://CRAN.R-project.org/package=rstatscn
#' @return no return
#' @export
statscnRowNamePrefix <- function(p = "nrow")
{
  if (p != "nrow" && ! is.null(p)) {
    stop(sprintf("the only supported prefix is 'nrow' or NULL "))
  }
  assign('prefix', p, envir = rstatscnEnv)
}




