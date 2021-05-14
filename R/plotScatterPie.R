

# this is for setting plot radius
getRadius <- function(x, rmin = 1, rmax = 6) {
  ratio <- (rmax - rmin) / (max(x) - min(x))

  r <- (x - min(x)) * ratio + rmin
  return (r)
}


#' plotScatterPie
#'
#' plot scatter pie chart for multidimension analysis, such as waternomics. This plot can
#' provide information about water use/wastewater of each provinces and GDP mix of each provinces,
#' see examples.
#' @param data a dataframe with colnames x, y, r, label, these four names must be in colnames.
#' @param pieRange define which column to which column to be presented by pie chart, see examples
#' @param pieColor color for different colors in pie chart
#' @param label_on Whether to show label
#' @param xmeanLine if plot x mean line
#' @param ymeanLine if plot y mean line
#' @param output if you want an ggplot object as output, default is FALSE
#' @importFrom graphics plot abline text
#' @importFrom ggrepel geom_label_repel
#' @importFrom scatterpie geom_scatterpie
#' @importFrom ggplot2 ggplot
#' @export
#' @examples
#'
#'
#' GDPColor_CWR <- c("#6B8033", "#020303", "#0D77B9")
#'
#'
#' data(GDPmix)
#'
#' # in colnames(GDPmix), there must be x, y, r, label.
#' # but right now, GDPmix has x, y, r, but lacks a label column, let's assign label to province column
#' colnames(GDPmix)[1] <- 'label'
#'
#' \dontrun{
#' plotScatterPie(GDPmix, pieRange = 4:6, pieColor = GDPColor_CWR)
#' }
#'
#'
#'

plotScatterPie <- function(data, pieRange, pieColor = NULL, xmeanLine = TRUE, ymeanLine = TRUE, label_on = TRUE,
                           output = FALSE) {

  ## input check
  if(is.null(pieRange)) stop("You have to assign which column to which column to be presented by pie chart.")
  if(!is.null(pieColor) & length(pieRange)!=length(pieColor)) stop("Length of pieRange and pieColor should be the same.")

  # adjustment of x and y
  # since coord_equal is set in order to make the pie chart round, and
  # most of time y axis is tens times of x axis, the plot will be weird.
  # So x and y axis must be adjusted to roughly 4:3, and add the label manually afterwards
  adj <- getLim(data$x, data$y)

  xlim <- adj$lim[1:2]
  ylim <- adj$lim[3:4]
  ratio <- adj$ratio

  if (adj$change == 'x') {
    xlabels <- getLabels(xlim / ratio, 5)
    data$x <- data$x * ratio
    xbreaks <- xlabels * ratio
    ybreaks <- getLabels(ylim, 4)
    ylabels <- ybreaks

  } else if (adj$change == 'y') {
    xbreaks <- getLabels(xlim, 5)
    xlabels <- xbreaks
    ylabels <- getLabels(ylim / ratio, 4)
    data$y <- data$y * ratio
    ybreaks <- ylabels * ratio

  }

  data$radius <- getRadius(data$r) * (ylim[2] - ylim[1]) / 50

  with (data, {
    layer_basic <- ggplot(data, aes(x = x))

    layer_pie <- geom_scatterpie(data = data, aes(x, y, r = radius),
                                 cols = colnames(data)[pieRange], color = 'white')

    if (label_on == FALSE) {
      layer_label <- NULL
    } else {
      #labelLine <- max(data$radius) / 25
      layer_label <- geom_label_repel(data = data, aes(x, y, label = label),
                                   point.padding = unit(1, "lines"))
    }

    #layer_legend <- geom_scatterpie_legend(data$radius, x= 0, y=0)
    if (xmeanLine == TRUE) layer_basic <- layer_basic + geom_vline(xintercept = mean(data$x), color = 'red', size = 1, linetype = 2)
    if (ymeanLine == TRUE) layer_basic <- layer_basic + geom_hline(yintercept = mean(data$y), color = 'red', size = 1, linetype = 2)

    if (!is.null(pieColor)) layer_basic <- layer_basic + scale_fill_manual(values = pieColor)

    layer_plot <- layer_basic + layer_pie + layer_label  +
      coord_equal() + ggstyle() +
      scale_x_continuous(breaks = xbreaks, labels = xlabels, limits = xlim) +
      scale_y_continuous(breaks = ybreaks, labels = ylabels, limits = ylim)


    print(layer_plot)

    if (output == TRUE) return(layer_plot)


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





#' @importFrom  ggplot2 theme theme_light element_blank
ggstyle <- function() {
  a <- theme_light() +
    theme(legend.position = 'bottom',
          panel.grid.minor = element_blank())
#          panel.border = element_rect(color = 'grey', fill = 'transparent'))
  # a <- theme(legend.position = 'bottom',
  #         panel.background = element_rect(fill = 'white'),
  #         panel.border = element_rect(colour = 'black'))
  #   theme(axis.line = element_line(size = 1, colour = "black"))
  #panel.background = element_rect(fill = "white"),
  #panel.grid.major = element_line(colour = "grey50"))
  return(a)
}





# this is used to get xlim and ylim of the plot, also breaks and ratios
getLim <- function(x, y, n = 0.75, xlablen = 5) {
  xl <- (max(x) - min(x))
  yl <- (max(y) - min(y))
  if (yl < n * xl) {
    ratio <- xl * n / yl
    change <- 'y'
    xlim <- c(min(x), max(x))
    ylim <- ratio * c(min(y), max(y))




  } else if (yl >= n * xl) {
    ratio <- yl / n / xl
    change <- 'x'

    xlim <- ratio * c(min(x), max(x))
    ylim <- c(min(y), max(y))


  }


  # enlarge xlim and ylim for elements not to reach the border
  xd <- (xlim[2] - xlim[1]) * 0.4/2
  yd <- (ylim[2] - ylim[1]) * 0.4/2

  xlim <- c(xlim[1] - xd, xlim[2] + xd)
  ylim <- c(ylim[1] - yd, ylim[2] + yd)

  res <- list()
  res$lim <- c(xlim, ylim)
  res$ratio <- ratio
  res$change <- change

  return(res)
}


# setup smart round up axis labels
roundN <- function(x) {
  if ( x >= 5) {
    # n <- -nchar(round(x)) + 1
    # while (round(x) == round(x, n)) {
    #   n <- n - 1
    # }
    # n <- n + 1
    n <- 5

  } else if (0 < x &  x < 5){
    l <- strsplit(as.character(x), '\\.')[[1]]
    l1 <- strsplit(l[2], '')[[1]]
    n <- 0
    for (i in l1) {
      if (i != '0') break
      n <- n + 1
    }
    n <- 5 * 10^(n-1)
  }

  res <- round(x / n) * n

  return(c(res, n))
}

getLabels <- function(lim, labeln) {

  d <- roundN((max(lim) - min(lim))/labeln)

  label1 <- round(min(lim) / d[2]) * d[2]

#  if (label1 < min(lim)) label1 <- label1 + d[1]
  # now the 1st label is fixed, trying to see if there will be enough palce for xlabel intervals, here xlabel = 4
  labeln <- round((max(lim) - label1) / d[1]) + 1 # reajust label numbers
  labelseq <- seq(label1, by = d[1], length.out = labeln)
  if (max(labelseq) > max(lim)) labelseq <- seq(label1, by = d[1], length.out = labeln - 1)

  return(labelseq)

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

