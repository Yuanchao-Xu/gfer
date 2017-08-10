
#' plotScatterPie
#'
#' plot scatter pie chart for multidimension analysis, such as waternomics. This plot can
#' provide information about water use/wastewater of each provinces and GDP mix of each provinces,
#' see examples.
#' @param data a dataframe showing different management intersections. See the data frame in the example
#' @param t is transpose the dataframe, by default, lines flow from row to column, if t == TRUE, lines will
#' flow from columns to rows. Once transposed,
#' @param ifsep if separate row and col categories in the chart, default is TRUE
#' @param trans transparency of the chart's lines, default is 0.3
#' @param highlight a string or string array of highlighted items, MUST be selected from first column (which represents names) or colnames.
#' if highlight has more than 2 items, they should belong to same category, either colnames, or names. One name and one column name is not allowed.
#' @param xlim x limit of the chart, default is c(-1, 1)
#' @param ylim y limte of the chart, default is c(-1, 1)
#' @description if 'Summation of cell padding on y-direction are larger than the height of the cells' appears, just enlarge the xlim or ylim accordingly
#' @import circlize
#' @examples
#'
#'
#'
#' \dontrun{
#' plotChord(cm)
#' plotChord(cm, t = T)
#' plotChord(cm, highlight = 'MEP')
#' plotChord(cm, highlight = 'Investment')
#' }
#'
plotChord <- function(data, t = FALSE, ifsep = TRUE, trans = 0.3, highlight = NULL, xlim = c(-1, 1), ylim = c(-1, 1)) {

  # tidy up data, change data frame to matrix with dimension names filled
  m <- as.matrix(data[2:ncol(data)])
  dimnames(m)[[1]] <- data[,1]

  if (t == TRUE) m <- t(m)
  # clean the canvass
  circos.clear()


  gaps <- NULL
  # set gaps
  if (ifsep == TRUE) {
    # set up small gap and big gap, this is to seperate the whole circle into up and down part
    small_gap = 1
    big_gap = 20

    # calculate how many sectors involved n_sector
    nr = nrow(m)
    nc = ncol(m)
    n_sector = nr + nc

    # calculate each sector takes up how many width, 0.5 means seperate into 2 parts, 0.25 means 4 parts
    row_sector_degree = (360 - small_gap*(n_sector - 2) - big_gap*2) * 0.5 + small_gap*(nr-1)

    start_degree = 0 - (180 - row_sector_degree)/2

    # calculate starting degree
    gaps = c(rep(small_gap, nrow(m) - 1), big_gap, rep(small_gap, ncol(m) - 1), big_gap)
  }




  col <- NULL
  # set up color, if highlight is selected
  if (!is.null(highlight)) {
    col <- m
    col[] <- 'grey'


    if (highlight %in% dimnames(m)[[1]]) col[highlight,] <- 'red'
    if (highlight %in% dimnames(m)[[2]]) col[,highlight] <- 'red'
  }

  # parameters

  circos.par(gap.after = gaps, start.degree = start_degree, canvas.xlim = xlim, canvas.ylim = ylim,
             points.overflow.warning = FALSE)

  # plot basic plot
  chordDiagram(m, annotationTrack = "grid", transparency = trans, col = col)

  # change label settings
  circos.track(track.index = 1, panel.fun = function(x, y) {
    circos.text(CELL_META$xcenter, CELL_META$ylim[1] + 2, CELL_META$sector.index,
                facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
  }, bg.border = NA)
  abline(h = 0, lty = 2, col = "#00000080")


}



