#' pivot
#'
#' Transfer 2D table to readable table for tableau
#'
#'
#' @param data a dataframe like \code{GDPmix}
#' @param reserve choose which column to reserve
#' @param newColName name for new column, apart from the reserved columns, all the other will be re-organised into a new column


pivot <- function(data, reserve, newColName) {
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
