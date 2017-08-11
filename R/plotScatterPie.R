

# this is for setting plot radius
getRadius <- function(y, small = 5, medium = 10, large = 15) {
  r <- sapply(y, function(x) {
    if (x <= 40000 & x >= 15000) {
      x <- medium
    } else if (x < 15000) {
      x <- small
    } else {
      x <- large
    }
  })
  return (r)
}


#' plotScatterPie
#'
#' plot scatter pie chart for multidimension analysis, such as waternomics. This plot can
#' provide information about water use/wastewater of each provinces and GDP mix of each provinces,
#' see examples.
#' @param data a dataframe with information like x, y, r, label. See examples about how to assign these columns as required.
#' @param pieRange define which column to which column to be presented by pie chart, see examples
#' @param pieColor color for different colors in pie chart
#' @param labelLine how far is label to pie chart, can be left with default value.
#' @param xmeanLine if plot x mean line
#' @param ymeanLine if plot y mean line
#' @importFrom graphics plot abline text
#' @importFrom ggrepel geom_text_repel
#' @importFrom scatterpie geom_scatterpie
#' @export
#' @import ggplot2
#' @examples
#'
#'
#' GDPColor_CWR <- c("#6B8033", "#020303", "#0D77B9")
#'
#' # Change colnames so that the function can recognize x, y, r, label
#'
#' colnames(GDPmix) <- c('label', 'r', '1st', '2nd', '3rd', 'x', 'y')
#'
#' \dontrun{
#' plotScatterPie(GDPmix, pieRange = 3:5, pieColor = GDPColor_CWR)
#' }
#'
#'

plotScatterPie <- function(data, pieRange, pieColor = NULL, xmeanLine = TRUE, ymeanLine = TRUE, labelLine = NULL) {

  ## input check
  if(is.null(pieRange)) stop("You have to assign which column to which column to be presented by pie chart.")
  if(length(pieRange)!=length(pieColor)) stop("Length of pieRange and pieColor should be the same.")

  # adjustment of x and y
  # since coord_equal is set in order to make the pie chart round, and
  # most of time y axis is tens times of x axis, the plot will be weird.
  # So x and y axis must be adjusted to roughly 4:3, and add the label manually afterwards
  xlim <- getLim(data$x)
  ylim <- getLim(data$y)

  ratio <- (ylim[2] - ylim[1])/(xlim[2] - xlim[1])/0.75
  data$x1 <- data$x
  data$x <- data$x1 * ratio

  i <- round((max(data$x1) - min(data$x1))/5)
  n <- ifelse((max(data$x1) - min(data$x1))/i >= 5, (5 + ceiling((max(data$x1) - min(data$x1))/i - 5)), 5)

  labels <- seq(ceiling(min(data$x1)), by = i, length.out = n)
  breaks <- labels * ratio

  with (data, {
    layer_basic <- ggplot(data, aes(x = x))
 #     geom_point(data = data, aes(x, y))

    data$radius <- getRadius(data$r)


    layer_pie <- geom_scatterpie(data = data, aes(x, y, r = radius),
                                 cols = colnames(data)[pieRange], color = 'white')

    if (is.null(labelLine)) labelLine <- max(data$radius)/3

    layer_label <- geom_text_repel(data = data, aes(x, y, label = label),
                                   point.padding = unit(labelLine, "lines"))
    #layer_legend <- geom_scatterpie_legend(data$radius, x= 0, y=0)
    if (xmeanLine == TRUE) layer_basic <- layer_basic + geom_vline(xintercept = mean(data$x), color = 'red', size = 1.5, linetype = 2)
    if (ymeanLine == TRUE) layer_basic <- layer_basic + geom_hline(yintercept = mean(data$y), color = 'red', size = 1.5, linetype = 2)

    if (!is.null(pieColor)) layer_basic <- layer_basic + scale_fill_manual(values = pieColor)

    layer_plot <- layer_basic + layer_pie + layer_label  +
      coord_equal() + ggstyle() +
      scale_x_continuous(breaks = breaks, labels = labels)


    print(layer_plot)
    return(layer_plot)

  })
}

# this is usefull but not appropriate for the scatter pie
# Maybe because pie needs a coord_equal definitely, and also, scatterpie is dealt with
# in another environment, so the transformation cannot be spread.
# @importFrom scales trans_new
# defnie a new transformation
# trans_trans <- function(ratio) {
#   trans_new('trans',
#             transform = function(x) {
#               y <- x * ratio
#               return(y)
#             },
#             inverse = function(y) {
#               x <- y / ratio
#               return(x)
#             })
# }





#' @import ggplot2
ggstyle <- function() {
  a <- theme_set(theme_bw()) +
    theme(legend.position = 'bottom')
  #   theme(axis.line = element_line(size = 1, colour = "black"))
             #panel.background = element_rect(fill = "white"),
             #panel.grid.major = element_line(colour = "grey50"))
  return(a)
}

adjustxlim <- function(xlim, ylim, ratio = 0.75) {
  length_y <- max(ylim) - min(ylim)
  length_x <- max(xlim) - min(xlim)
  if (length_x < length_y/ratio) xlim <- c(min(xlim), min(xlim) + length_y)
  return(xlim)
}

getLim <- function(x) {
  # decide axis interval
  dig <- nchar(min(round(x)))

  x1 <- round(min(x) - (max(x) - min(x))/length(x), -(dig - 1))
  x2 <- round(max(x) + (max(x) - min(x))/length(x), -(dig - 1))

  return(c(x1, x2))
}



########################################################################################
#######################################################################################
#'
#'
#'
#' #' plotWaternomics
#' #'
#' #' plot special waternomics chart for CWR
#' #'
#' #'
#' #' @param data a dataframe containing columns of GDP, Value-added of fisrt/second/third industry, x and y
#' #' x and y are the coordinates of the plot, and it varies depending on needs, can be per capita
#' #' water use vs. per capita wastewater, etc. See example of year 2015 by \code{GDPmix}
#' #'
#' #' Columns should be exactly the same as GDPmix, which means, first column is province, seconnd is x,
#' #' third is y, fourth is Frist, etc. Load GDPmix to have a look at the columns.
#' #' @param title chart title
#' #' @param xname x axis name
#' #' @param yname y axis name
#' #' @param small radius of small circle, default is 1.5
#' #' @param medium radius of medium circle, default is 3
#' #' @param large radius of large circle, default is 5
#' #' @param legend whether to show legend, default is TRUE
#' #' @param label whether to show label, default is TRUE
#' #' @param xmean a line showing mean value of x
#' #' @param ymean a line showing mean value of y
#' #' @param line whether to show ablines, default is TRUE
#' #' @importFrom mapplots add.pie
#' #' @importFrom graphics plot plot.new
#' #' @importFrom grDevices rgb
#' #' @importFrom plotrix thigmophobe.labels
#' #' @export
#' #' @examples
#' #' \dontrun{
#' #' data(GDPmix)
#' #' plotwaternomics(GDPmix)
#' #' }
#'
#'
#'
#' plotWaternomics <- function(data, title = '', xname = '', yname = '', small = 1.5, medium = 3, large = 5,
#'                             legend = TRUE, label = TRUE, xmean = NULL, ymean = NULL, line = TRUE) {
#'   # plot basic plot based on x and y
#'   #plot.new()
#'
#'   # for some buffer space, extend the x and y axis, redefine them first
#'   xlim <- getLim(data$x)
#'
#'   ylim <- getLim(data$y)
#'
#'
#'   plot(data$x, data$y, main = title, xlab = xname, ylab = yname,
#'        xlim = xlim, ylim = ylim,
#'        bty = 'n')
#'
#'
#'
#'   provinceNum <- nrow(data)
#'
#'   # decide radius of the pie charts
#'   r <- getRadius(data$GDP, small, medium, large)
#'
#'   # define CWR color
#'   col <- c(rgb(107/255, 128/255, 51/255), rgb(2/255, 3/255, 3/255), rgb(13/255, 119/255, 185/255))
#'
#'
#'
#'
#'   # add pie charts
#'   for (i in 1:provinceNum) {
#'
#'     add.pie(x = data$x[i], y = data$y[i], z = c(data$First[i], data$Second[i], data$Third[i]),
#'             labels = '', radius = r[i], col = col, border = "white")
#'     #thigmophobe.labels(data$x[i], data$y[i], data$Province[i], offset = r[i]/3)
#'
#'   }
#'   if (legend == TRUE) {
#'     legend('topright', c("Agriculture","Industry","Services"), cex = 0.8,
#'            fill = col)
#'   }
#'   if (label == TRUE) {
#'     thigmophobe.labels(data$x, data$y, data$Province, offset = 1)
#'   }
#'
#'
#'   if (line == TRUE) {
#'     # add ablines of national mean or mean
#'     if (is.null(xmean)) xmean <- round(mean(data$x), 2)
#'     if (is.null(ymean)) ymean <- round(mean(data$y), 2)
#'     abline(h = ymean, v = xmean, col = '#c00000', lwd = 3, lty = 2)
#'   }
#'
#'   #spread.labels(data$x, data$y, data$Province, offsets = 0.01)
#'
#' }
#'
#'
#'
#' #' plotWaternomics_legend
#' #'
#' #' legend for plot special waternomics chart for CWR
#' #'
#' #' @param label whether to show label, default is TRUE
#' #' @importFrom mapplots add.pie
#' #' @importFrom graphics plot abline text
#' #' @importFrom grDevices rgb
#' #' @export
#' #' @examples
#' #' \dontrun{
#' #' plotwaternomics_legend()
#' #' }
#' plotWaternomics_legend <- function(label = TRUE) {
#'   r <- c(5, 3, 1.5)
#'   x <- c(0, 0, 0)
#'   y <- c(7, 16, 21)
#'   plot (x, y, xlim = c(-3, 3), ylim = c(0, 23), xaxt = 'n', yaxt = 'n', bty = 'n')
#'
#'   col <- c(rgb(107/255, 128/255, 51/255), rgb(2/255, 3/255, 3/255), rgb(13/255, 119/255, 185/255))
#'
#'   for (i in 1:3) {
#'     add.pie(x = x[i], y = y[i], z = c(1, 1, 1), radius = r[i], labels = '', col = col, border = 'white')
#'   }
#'   if (label == TRUE) {
#'     size <- 1.2
#'     text(-0.7, 8.5, "Service", col = 'white', cex = size)
#'     text(0.7, 8.5, "Agriculture", col = 'white', cex = size)
#'     text(0, 4, "Industry", col = 'white', cex = size)
#'   }
#' }
#'
#'
#'
#'
#'
#'
#'
#'

