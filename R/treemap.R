# treemap
#' treemap_CWR
#'
#' plot treemap for GDP and water stress
#'
#' @param data a dataframe like \code{GDPmix}
#' @importFrom treemap treemap

treemap_CWR <- function(data) {

  treemap(data,
          index = c('Province'),
          vSize = 'GDP',
          vColor = 'GDP',
          type = 'value'
          #palette = 'Set1'
          )
}


fortify <- function(data, reserve, newColName) {
  data1 <- data[, reserve]
  data2 <- do.call('rbind', rep(list(data1), ncol(data) - length(reserve)))
  data3 <- data[, !(colnames(data) %in% reserve)]
  data4 <- lapply(1:length(data3), function(x) {
    name <- names(data3)[x]
    x1 <- data3[, x]
    y <- data.frame(rep(name, length(x1)), x1)
    names(y) <- c(newColName, 'Value')
    return (y)
  })
  data5 <- do.call('rbind', data4)

  data6 <- cbind(data2, data5)
  return(data6)
}
