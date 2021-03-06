% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getNBS.R
\name{genDfwds}
\alias{genDfwds}
\title{private function for constructing the query parameter for dfwds}
\usage{
genDfwds(wdcode, valuecode)
}
\arguments{
\item{wdcode}{string value , one of c("zb","sj","reg")}

\item{valuecode}{string value ,  following is the table for available valuecode
zb:   the valudecode can be gotten by statscnQueryZb() function
sj:   the valudecode can be "2014" for nd db,  "2014C" for jd db.
reg:  the valudecode is the region code fetched by statscnRegions(dbcode) function}
}
\value{
return the queyr string for the http request
}
\description{
private function for constructing the query parameter for dfwds
}
\references{
Xuehui YANG (2016). rstatscn: R Interface for China National Data. R
package version 1.1.1. https://CRAN.R-project.org/package=rstatscn
}
