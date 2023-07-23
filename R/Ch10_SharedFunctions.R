

##########################################
# Determine Layout (perceptual groups)
##########################################
DetermineLayout <- function(n, partitioning = 1) {
  #n = as.integer(n)
  if (n < 3) {
    print("Select at least 3 subregions.")
    return(NA)
  } else if (n > 51) {
    print("Select at most 51 subregions.")
    return(NA)
  }

  if (partitioning == 1) {
    if (n == 3) return(c(2, 1))
    else if (n == 4) return(c(2, 2))
    else if (n == 5) return(c(2, 2))
    else if (n == 6) return(c(3, 3))
    else if (n == 7) return(c(3, 3))
    else if (n == 8) return(c(4, 4))
    else if (n == 9) return(c(4, 4))
    else if (n == 10) return(c(5, 5))
    else if (n == 11) return(c(5, 5))
    else if (n == 12) return(c(5, 2, 5))
    else if (n == 13) return(c(5, 3, 5))
    else if (n == 14) return(c(5, 4, 5))
    else if (n == 15) return(c(5, 5, 5))
    else if (n == 16) return(c(5, 3, 3, 5))
    else if (n == 17) return(c(5, 3, 3, 5))
    else if (n == 18) return(c(5, 4, 4, 5))
    else if (n == 19) return(c(5, 4, 4, 5))
    else if (n == 20) return(c(5, 5, 5, 5))
    else if (n == 21) return(c(5, 5, 5, 5))
    else if (n == 22) return(c(5, 5, 2, 5, 5))
    else if (n == 23) return(c(5, 5, 3, 5, 5))
    else if (n == 24) return(c(5, 5, 4, 5, 5))
    else if (n == 25) return(c(5, 5, 5, 5, 5))
    else if (n == 26) return(c(5, 5, 3, 3, 5, 5))
    else if (n == 27) return(c(5, 5, 3, 3, 5, 5))
    else if (n == 28) return(c(5, 5, 4, 4, 5, 5))
    else if (n == 29) return(c(5, 5, 4, 4, 5, 5))
    else if (n == 30) return(c(5, 5, 5, 5, 5, 5))
    else if (n == 31) return(c(5, 5, 5, 5, 5, 5))
    else if (n == 32) return(c(5, 5, 5, 2, 5, 5, 5))
    else if (n == 33) return(c(5, 5, 5, 3, 5, 5, 5))
    else if (n == 34) return(c(5, 5, 5, 4, 5, 5, 5))
    else if (n == 35) return(c(5, 5, 5, 5, 5, 5, 5))
    else if (n == 36) return(c(5, 5, 5, 3, 3, 5, 5, 5))
    else if (n == 37) return(c(5, 5, 5, 3, 3, 5, 5, 5))
    else if (n == 38) return(c(5, 5, 5, 4, 4, 5, 5, 5))
    else if (n == 39) return(c(5, 5, 5, 4, 4, 5, 5, 5))
    else if (n == 40) return(c(5, 5, 5, 5, 5, 5, 5, 5))
    else if (n == 41) return(c(5, 5, 5, 5, 5, 5, 5, 5))
    else if (n == 42) return(c(5, 5, 5, 5, 2, 5, 5, 5, 5))
    else if (n == 43) return(c(5, 5, 5, 5, 3, 5, 5, 5, 5))
    else if (n == 44) return(c(5, 5, 5, 5, 4, 5, 5, 5, 5))
    else if (n == 45) return(c(5, 5, 5, 5, 5, 5, 5, 5, 5))
    else if (n == 46) return(c(5, 5, 5, 5, 3, 3, 5, 5, 5, 5))
    else if (n == 47) return(c(5, 5, 5, 5, 3, 3, 5, 5, 5, 5))
    else if (n == 48) return(c(5, 5, 5, 5, 4, 4, 5, 5, 5, 5))
    else if (n == 49) return(c(5, 5, 5, 5, 4, 4, 5, 5, 5, 5))
    else if (n == 50) return(c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5))
    else if (n == 51) return(c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5))
  } else if (partitioning == 2) {
    if (n == 3) return(c(2, 1))
    else if (n == 4) return(c(2, 2))
    else if (n == 5) return(c(3, 2))
    else if (n == 6) return(c(3, 3))
    else if (n == 7) return(c(2, 3, 2))
    else if (n == 8) return(c(4, 4))
    else if (n == 9) return(c(3, 3, 3))
    else if (n == 10) return(c(5, 5))
    else if (n == 11) return(c(3, 5, 3))
    else if (n == 12) return(c(4, 4, 4))
    else if (n == 13) return(c(4, 5, 4))
    else if (n == 14) return(c(5, 4, 5))
    else if (n == 15) return(c(5, 5, 5))
    else if (n == 16) return(c(4, 4, 4, 4))
    else if (n == 17) return(c(3, 4, 3, 4, 3))
    else if (n == 18) return(c(4, 5, 5, 4))
    else if (n == 19) return(c(4, 4, 3, 4, 4))
    else if (n == 20) return(c(5, 5, 5, 5))
    else if (n == 21) return(c(4, 4, 5, 4, 4))
    else if (n == 22) return(c(5, 4, 4, 4, 5))
    else if (n == 23) return(c(5, 5, 3, 5, 5))
    else if (n == 24) return(c(5, 5, 4, 5, 5))
    else if (n == 25) return(c(5, 5, 5, 5, 5))
    else if (n == 26) return(c(5, 4, 4, 4, 4, 5))
    else if (n == 27) return(c(4, 4, 4, 3, 4, 4, 4))
    else if (n == 28) return(c(4, 5, 5, 5, 5, 4))
    else if (n == 29) return(c(4, 4, 4, 5, 4, 4, 4))
    else if (n == 30) return(c(5, 5, 5, 5, 5, 5))
    else if (n == 31) return(c(4, 4, 5, 5, 5, 4, 4))
    else if (n == 32) return(c(5, 5, 4, 4, 4, 5, 5))
    else if (n == 33) return(c(4, 5, 5, 5, 5, 5, 4))
    else if (n == 34) return(c(5, 5, 5, 4, 5, 5, 5))
    else if (n == 35) return(c(5, 5, 5, 5, 5, 5, 5))
    else if (n == 36) return(c(4, 4, 5, 5, 5, 5, 4, 4))
    else if (n == 37) return(c(4, 4, 4, 4, 5, 4, 4, 4, 4))
    else if (n == 38) return(c(4, 5, 5, 5, 5, 5, 5, 4))
    else if (n == 39) return(c(4, 4, 4, 5, 5, 5, 4, 4, 4))
    else if (n == 40) return(c(5, 5, 5, 5, 5, 5, 5, 5))
    else if (n == 41) return(c(4, 4, 5, 5, 5, 5, 5, 4, 4))
    else if (n == 42) return(c(5, 5, 5, 4, 4, 4, 5, 5, 5))
    else if (n == 43) return(c(4, 5, 5, 5, 5, 5, 5, 5, 4))
    else if (n == 44) return(c(5, 5, 5, 5, 4, 5, 5, 5, 5))
    else if (n == 45) return(c(5, 5, 5, 5, 5, 5, 5, 5, 5))
    else if (n == 46) return(c(4, 4, 5, 5, 5, 5, 5, 5, 4, 4))
    else if (n == 47) return(c(4, 4, 4, 4, 5, 5, 5, 4, 4, 4, 4))
    else if (n == 48) return(c(4, 5, 5, 5, 5, 5, 5, 5, 5, 4))
    else if (n == 49) return(c(4, 4, 4, 5, 5, 5, 5, 5, 4, 4, 4))
    else if (n == 50) return(c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5))
    else if (n == 51) return(c(4, 4, 5, 5, 5, 5, 5, 5, 5, 4, 4))
  } else
    return(NA)
}


DetermineMedianRow <- function(n, partitioning = 1) {
  if (partitioning == 1)
    return(n %in% c(5, 7, 9, 11, 17, 19, 21, 27, 29, 31, 37, 39, 41, 47, 49, 51))
  # else if (partitioning == 2)
  #   return(n %in% c(5))
  else
    return(0)
}


ArrangePanels <- function(posterdat = posterdat, partitioning = 2, AutomaticLayout = TRUE, Layout = NA, MedianRow) {
  # Automatically arrange panels
  if (AutomaticLayout){
    groups = DetermineLayout(nrow(posterdat), partitioning)
    MedianRow = DetermineMedianRow(nrow(posterdat), partitioning)
    rows = length(groups) + MedianRow
  } else if (MedianRow) {
    groups = Layout[-ceiling(length(Layout)/2)]
    rows = length(Layout)
  } else {
    groups = Layout
    rows = length(Layout)
  }


  # Automatically set group beginning and group ending
  iBegin = rep(0, rows)
  iEnd = iBegin

  iBegin[1] = 1
  iEnd[1] = groups[1]

  if (MedianRow){

    for (i in 2:rows){
      if (i == ((rows - 1)/2 + 1)){
        iBegin[i] = iEnd[i - 1] + 1
        iEnd[i] = iBegin[i]
      } else if (i < ((rows - 1)/2 + 1)){
        iBegin[i] = groups[i - 1] + iBegin[i - 1]
        iEnd[i] = groups[i] + iBegin[i] - 1
      } else {
        iBegin[i] = sum(groups[1:(i - 2)]) + 2
        iEnd[i] = sum(groups[1:(i - 1)]) + 1
      }
    }

  } else {

    for (i in 2:rows){
      iBegin[i] = groups[i - 1] + iBegin[i - 1]
      iEnd[i] = groups[i] + iBegin[i] - 1
    }
  }
  return(list(iBegin, iEnd, rows))

}

##########################################
# Other Panel Design functions
##########################################

panelFill <- function (col = "#D0D0D0", border = NA, ...)
{
  xy <- graphics::par("usr")
  graphics::polygon(xy[c(1, 2, 2, 1)], xy[c(3, 3, 4, 4)], col = col,
                    border = border, xpd = T, ...)
}


panelGrid <- function (x = NULL, y = NULL, col = 2, lwd = 1, lty = 1)
{
  if (!is.null(x))
    graphics::abline(v = x, lwd = lwd, lty = lty, col = col)
  if (!is.null(y))
    graphics::abline(h = y, lwd = lwd, lty = lty, col = col)
}


panelInbounds <- function (bnds)
{
  grid = pretty(bnds)
  return(grid[bnds[1] < grid & grid < bnds[2]])
}



panelLayout <- function (nrow = 1, ncol = 1, leftMargin = 0, rightMargin = 0,
          topMargin = 1, bottomMargin = 0, borders = rep(0.5, 4),
          colSize = 1, rowSize = 1, colSep = 0, rowSep = 0, pad = NULL)
{
  oldpar = graphics::par()
  din = oldpar$din
  din.x = din[1]
  din.y = din[2]
  plotX = din.x - borders[1] - borders[2] - leftMargin - rightMargin
  plotY = din.y - borders[3] - borders[4] - bottomMargin -
    topMargin
  xbnds = c(0, leftMargin, leftMargin + plotX, leftMargin +
              plotX + rightMargin) + borders[1]
  ybnds = c(0, bottomMargin, bottomMargin + plotY, bottomMargin +
              plotY + topMargin) + borders[4]
  fig.scale = c(din.x, din.x, din.y, din.y)
  leftfig = c(xbnds[1] - borders[1], xbnds[2] + borders[2],
              ybnds[1] - borders[4], ybnds[4] + borders[3])
  rightfig = c(xbnds[3] - borders[1], xbnds[4] + borders[2],
               ybnds[1] - borders[4], ybnds[4] + borders[3])
  topfig = c(xbnds[1] - borders[1], xbnds[4] + borders[2],
             ybnds[3] - borders[4], ybnds[4] + borders[3])
  botfig = c(xbnds[1] - borders[1], xbnds[4] + borders[2],
             ybnds[1] - borders[4], ybnds[2] + borders[3])
  colSep = panelLengthen(colSep, ncol + 1)
  rowSep = panelLengthen(rowSep, nrow + 1)
  if (is.null(pad))
    pad = c(borders[1] + colSep[1] + leftMargin, borders[2] +
              colSep[ncol + 1] + rightMargin, borders[3] + rowSep[1] +
              topMargin, borders[4] + rowSep[nrow + 1] + bottomMargin)
  colSep = cumsum(colSep)
  rowSep = cumsum(rowSep)
  plotX = plotX - colSep[ncol + 1]
  plotY = plotY - rowSep[nrow + 1]
  relx = panelLengthen(colSize, ncol)
  rely = panelLengthen(rowSize, nrow)
  relx = relx/sum(relx)
  rely = rely/sum(rely)
  xinc = plotX * cumsum(c(0, relx))
  yinc = plotY * cumsum(c(0, rely))
  fig = matrix(0, nrow = nrow * ncol, ncol = 4)
  k = 0
  for (i in 1:nrow) {
    for (j in 1:ncol) {
      k = k + 1
      fig[k, 1] = xbnds[2] + xinc[j] + colSep[j] - pad[1]
      fig[k, 2] = xbnds[2] + xinc[j + 1] + colSep[j] +
        pad[2]
      fig[k, 4] = ybnds[3] - yinc[i] - rowSep[i] + pad[3]
      fig[k, 3] = ybnds[3] - yinc[i + 1] - rowSep[i] -
        pad[4]
    }
  }
  labfig = rbind(leftfig, rightfig, topfig, botfig)
  fig = abs(t(t(fig)/fig.scale))
  labfig = t(t(labfig)/fig.scale)
  coltabs = cbind(c(0, colSep + xinc + leftMargin), leftMargin +
                    c(0, colSep) + c(xinc, xinc[length(xinc)] + rightMargin))
  rowtabs = cbind(c(ybnds[3], ybnds[3] - rowSep - c(yinc[-1],
                                                    yinc[nrow + 1] + bottomMargin)), c(ybnds[4], ybnds[3] -
                                                                                         rowSep - yinc)) - borders[4]
  list(dim = c(nrow, ncol), datfig = round(fig, 6), labfig = round(labfig,
                                                                   6), brd = borders, pad = pad, coltabs = coltabs, rowtabs = rowtabs,
       figsize = c(din.x, din.y))
}


panelLengthen <- function (x, n = 1)
{
  if (n < 1)
    stop("panelLengthen: invalid required length")
  if (length(x) == 0)
    return(rep(0, n))
  newx = rep(x, ceiling(n/length(x)))
  length(newx) = n
  return(newx)
}



panelOutline <- function (col = "black", lwd = 1, lty = 1)
{
  xy <- graphics::par("usr")
  graphics::polygon(xy[c(1, 2, 2, 1)], xy[c(3, 3, 4, 4)], density = 0,
                    col = col, xpd = T)
}


panelScale <- function (rx = c(0, 1), ry = c(0, 1), firstp = FALSE, inches = FALSE)
{
  if (inches) {
    pin = graphics::par("pin")
    rx = c(0, pin[1])
    ry = c(0, pin[2])
  }
  warn = unlist(options("warn"))
  options(warn = -1)
  graphics::par(new = TRUE)
  options(warn = warn)
  graphics::plot(rx, ry, type = "n", axes = F, xaxs = "i", yaxs = "i",
                 xlab = "", ylab = "", main = "")
  return(list(rx = rx, ry = ry))
}



panelSelect <- function (layout, i = 1, j = 1, margin = NULL)
{
  dim = layout$dim
  if (i > dim[1] || j > dim[2])
    message("Dimension error")
  if (is.null(margin)) {
    k = dim[2] * (i - 1) + j
    graphics::par(fig = layout$datfig[k, ], mai = layout$pad[c(4,
                                                               1, 3, 2)])
  }
  else {
    vec = 1:4
    nam = c("left", "right", "top", "bottom", "bot")
    ind = match(margin, nam)
    if (is.na(ind))
      message("Bad label region name")
    if (ind == 5)
      ind = 4
    graphics::par(fig = layout$labfig[ind, ], mai = layout$brd[c(4,
                                                                 1, 3, 2)])
  }
}

##########################
# details for plotting
###########################

details <- list(
  bot = 0.05, # bottom margin for the panel
  top = 0.6, # top margin for the panel
  left = 0, # left margin for the panel
  right = 0, # right margin for the panel
  dcex = 0.96, # text size option 1
  tCex = 1.08, # text size option 2
  cex = 0.7, # text size option 3
  fontsize = 12, # font size option 1
  font = 1, # font size option 2
  line1 = 0.2, # on which margin line, starting at 0 counting outwards
  line2 = 1.0, # same as above
  line3 = 0.2, # same as above
  ypad = 0.65, # change the scale of the panel vertically
  nameShift = 0.12, # label location shift
  wyellow = rgb(1, 1, 0.82), # color option 1 (for plot background)
  wgray = rgb(0.945, 0.945, 0.945), # color option 2 (for panel background)
  lwd = 1, # plot polygon line type
  density = 0 # polygon density
  
)



details_layout <- list(
  pheight = 17.2, # panel height
  pwidth = 2.9, # panel width
  mheight = 3.2, # median row height divider
  ngroups = 2, # number of groups excluding median
  pcspace = 0.1, # space between panels in columns
  prspace = 0.01, # space between panels in rows
  ocol = 'black', # group outline color
  text_x = 0.5, # x coordinate for group labels
  text_y = 0.55, # y coordinate for group labels
  alpha = 0.6, # transparency of plot polygons
  lwd = 1, # plot polygon line type
  density = 0 # polygon density
)


details_labels <-  list(
  header_size = 0.7, # header size
  label_cex = 0.7, # label text size 
  dcex = 0.96, # label dot size
  font = 1, # header font
  points_x = 0.1, # x coordinate of label dot
  text_x = 0.18, # x coordinate of label text
  points_pch1 = 16, # label point pch
  points_pch2 = 1 # label point pch
)


details_dotplot <- list(
  rmulti = 1.3, # multiplier for determining range
  rmarks = 0.5, # steps for determining axis ticks automatically
  gmulti = 0.28, # multiplier for determining grid 
  tck = -0.04, # axis tick label location
  pch1 = 16, # pch for points
  pch2 = 1 # pch for points
)



details_boxplot <- list(
  pmulti = 0.5, # size multiplier for lines
  mmulti = 0.05, # scaling multiplier for plotting grid
  thin_box = 0.2, # size of the inner box
  thick_box = 0.60, # size of the outer box
  use_black = FALSE, # for outliers
  median_line = 0.88, # median line width
  lwd_median = 2, # line width for median
  cex_outlier = 0.6, # cex for the outlier dot
  lwd_outlier = 0.4, # line width for outlier
  pch_outlier = 1, # dot type for outlier
  sc = 1.3, # scaling for range
  text_size = 0.7, # text size for boxplot
  tck = -0.04 # location to label ticks
)




