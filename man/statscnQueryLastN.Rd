% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getNBS.R
\name{statscnQueryLastN}
\alias{statscnQueryLastN}
\title{fetch the lastN data}
\usage{
statscnQueryLastN(n)
}
\arguments{
\item{n}{the number of rows to be fetched}
}
\value{
the last n rows data in the latest query
}
\description{
fetch the lastN data for the latest query, only affect the number of rows in the returned data.
This function can not be used alone , statscnQueryData() has to be called before this function
}
\examples{
\dontrun{
df=statscnQueryData('A0201',dbcode='hgnd')
df2=statscnQueryLastN(20)
}


}
\references{
Xuehui YANG (2016). rstatscn: R Interface for China National Data. R
package version 1.1.1. https://CRAN.R-project.org/package=rstatscn
}
