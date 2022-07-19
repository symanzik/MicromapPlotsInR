

####################################
# Define some graphic parameters
####################################
details <- list(

  bot = 0.77,
  top = 0.85,
  left = 0,
  right = 0,
  dcex = 0.96,
  tCex = 1.08,
  cex = 0.7,
  fontsize = 12,
  font = 1,
  line1 = .2,
  line2 = 1.0,
  line3 = .2,
  ypad = .65,
  nameShift = .12,
  wyellow = rgb(1,1,.82),
  # wgray = rgb(0.82, 0.82, 0.82)  #white gray
  wgray = rgb(0.945, 0.945, 0.945)
)
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

##############################################
# Functions for microposter plot columns
##############################################


m.labels <- function(panel.col,
                     title,
                     header.size = 0.7,
                     cex = 0.7,
                     dcex = 0.96,
                     font = 1,
                     posterdat = posterdat,
                     stateVBorders,
                     nationVBorders,
                     panels,
                     nGroups,
                     MedianRow,
                     iBegin,
                     iEnd,
                     hdColors){
  
  
  stateNames = posterdat$section # [ord]
  # title column
  panelSelect(panels, 1, panel.col)
  panelScale()
  
  if (MedianRow){
    graphics::mtext(side = 3, line = details$line2, title, cex = header.size)
  } else {
    graphics::mtext(side = 3, line = details$line1, title, cex = header.size)
  }
  
  # draw region names
  for (i in 1:nGroups) {
    
    gsubs = iBegin[i]:iEnd[i]
    gnams = posterdat$section[gsubs]
    nsubs = length(gnams)
    pen = 1:nsubs
    laby = nsubs:1
    
    panelSelect(panels, i, panel.col)
    panelScale(c(0, 1), c(1 - details$ypad, nsubs + details$ypad))
    
    if (MedianRow == TRUE && i == ((nGroups + 1)/2)){
      pen = length(hdColors)
    }
    
    for (j in 1:length(pen)) {
      graphics::points(0.1, laby[j], pch = 16, col = hdColors[pen[j]], cex = dcex)
      graphics::points(0.1, laby[j], pch = 1, col = "black", cex = dcex)
      graphics::text(0.18, laby[j] + details$nameShift, gnams[j], cex = cex, adj = 0, col = "black", font = font)
      
      if (gnams[j] == "Blank") {
        graphics::points(0.1, laby[j], pch = 16, col = "white", cex = dcex)
        graphics::points(0.1, laby[j], pch = 1, col = "black", cex = dcex)
      }
      
    }
  }
  
}


m.dotplot <- function(var,
                      posterdat,
                      panel.num,
                      title,
                      cex = 0.7,
                      dcex = 0.96,
                      axis.ticks = NA,
                      axis.labels = NA,
                      panels,
                      nGroups,
                      MedianRow,
                      iBegin,
                      iEnd,
                      hdColors){
  
  var = posterdat[, var]
  sectionNames = posterdat$section
  countRange = range(var)
  countRange = mean(countRange) + 1.3 * diff(countRange) * c(-0.5, 0.5)
  if(is.na(axis.ticks)){
    countGrid = panelInbounds(countRange)  # used pretty values that are in bounds
  } else {
    countGrid = axis.ticks
  }
  
  panelSelect(panels, 1, panel.num)
  panelScale()
  graphics::mtext(side = 3, line = details$line1, text = title, cex = cex)
  
  
  for (i in 1:nGroups) {
    gsubs = iBegin[i]:iEnd[i]
    gnams = sectionNames[gsubs]
    nsubs = length(gsubs)
    pen = 1:nsubs
    laby = nsubs:1
    panelSelect(panels, i, panel.num)
    countRange2 = c(min(countGrid) - mean(countGrid)*0.28, max(countGrid) + mean(countGrid)*0.28)
    panelScale(countRange2, c(1 - details$ypad, nsubs + details$ypad))
    panelFill(col = details$wgray)
    panelGrid(x = countGrid, col = "white")
    panelOutline(col = "white")
    
    
    if (i == nGroups) {
      if (is.na(axis.labels)){
        graphics::axis(side = 1, at = countGrid, labels = as.character(countGrid), col = "black", mgp = c(1, 0, 0),
                       tck = -0.04, cex.axis = cex)
      } else {
        graphics::axis(side = 1, at = countGrid, labels = as.character(axis.labels), col = "black", mgp = c(1, 0, 0),
                       tck = -0.04, cex.axis = cex)
      }
    }
    
    if (MedianRow == TRUE & i == ((nGroups + 1)/2))
      pen = length(hdColors)
    graphics::lines(var[gsubs], laby, col = "black", lwd = 1)
    
    for (j in 1:length(pen)) {
      graphics::points(var[gsubs[j]], laby[j], pch = 16, cex = dcex, col = hdColors[pen[j]])
      graphics::points(var[gsubs[j]], laby[j], pch = 1, cex = dcex, col = "black")
      if (gnams[j] == "Blank") {
        graphics::points(var[gsubs[j]], laby[j], pch = 16, cex = dcex, col = "white")
        graphics::points(var[gsubs[j]], laby[j], pch = 1, cex = dcex, col = "black")
      }
    }
    
  }
}


m.boxplot <- function(dat,
                       posterdat,
                       panel.num,
                       text.size = 0.7,
                       panels,
                       title,
                       axis.ticks = NA,
                       axis.labels = NA,
                       nGroups,
                       MedianRow,
                       iBegin,
                       iEnd,
                       hdColors) {
  
  if (dat == "speed"){
    dat = posterdat[[2]]
  } else if (dat == "pupildat"){
    dat = posterdat[[3]]
  }
  
  sectionNames = posterdat[[1]]$section
  ## Boxplot
  # box plot parameters
  thinBox = .2     #.29            ## JP decreased line width
  thickBox = .60   #.58
  useBlack = FALSE    # for outliners
  medianLine = .88
  colDotMedian = "white"
  pchMedian = 19
  cexMedian = .95
  lwdMedian = 2
  colMedian = "white"
  cexOutlier = .6 # see cexDot   ## JP decreased dot size
  lwdOutlier = .4                ## JP decreased dot line width
  sc = 1.3
  pad = .67       # y axis padding for integer plotting locates
  lwdGrid = 1
  
  
  boxlist = graphics::boxplot(dat, plot = FALSE)
  # y boxplot scaling              # standard - horizontal box - no vertical (y) dimensions
  py = c(-.5, -.5, .5, .5)
  thiny = thinBox*py
  thicky = thickBox*py
  medy = medianLine*c(-.5, .5)
  ry = c(0, 1)                      # used in y scaling for grid lines
  
  #_______________Gather stats and put in State Order______________
  
  # For the moment match on names
  #                     Boxlist = names, stats, out, group,
  stats = boxlist$stats       # Name,1-low,2-25%,3-median,4-75%,5-high  - 5 variables for each state.
  thin = stats[c(1, 5, 5, 1 ), ]   # a column for each state - thin line - outliers - columns in boxlist (1,5,5,1)
  thick = stats[c(2, 4, 4, 2), ]  # a column for each state - thick line - 25% to 75% - columns in boxlist(2,4,4,2)
  med = stats[3, ]             # a single value for each state (median)
  nam = boxlist$names         # state name.
  
  # conf = boxlist$conf
  outlier = rep(FALSE, length(med))
  if(!is.null(boxlist$out)){
    out = boxlist$out
    group = boxlist$group
    outlier[unique(group)] = TRUE
  }
  
  # Need to put in order
  ord = match(sectionNames, nam)
  
  ## Run
  if(is.null(out)) rx = range(stats) else
    rx  = range(stats, out)
  countRange  = sc*diff(rx)*c(-.5,.5) + mean(rx)
  
  if(is.na(axis.ticks)){
    countGrid = panelInbounds(countRange)  # used pretty values that are in bounds
  } else {
    countGrid = axis.ticks
  }
  
  panelSelect(panels, 1, panel.num)
  panelScale()
  graphics::mtext(side = 3, line = details$line1, text = title, cex = text.size)
  
  
  for (i in 1:nGroups) {
    gsubs = iBegin[i]:iEnd[i]
    nsubs = length(gsubs)
    gnams = sectionNames[gsubs]
    pen = 1:nsubs
    laby = nsubs:1
    panelSelect(panels, i, panel.num)
    countRange2 = c(min(countGrid) - stats::median(countGrid)*0.05, max(countGrid) + stats::median(countGrid)*0.05)
    panelScale(countRange2, c(1 - details$ypad, nsubs + details$ypad))
    panelFill(col = details$wgray)
    panelGrid(x = countGrid, col = "white")
    panelOutline(col = "white")
    if (i == nGroups) {
      if(is.na(axis.labels)){
        graphics::axis(side = 1, at = countGrid, labels = as.character(countGrid), col = "black", mgp = c(1, 0, 0),
                       tck = -0.04, cex.axis = text.size)
      } else {
        graphics::axis(side = 1, at = countGrid, labels = as.character(axis.labels), col = "black", mgp = c(1, 0, 0),
                       tck = -0.04, cex.axis = text.size)
      }
    }
    
    
    if (MedianRow == TRUE & i == ((nGroups + 1)/2)){
      pen = length(hdColors)
      # lines(pupildat[gsubs], laby, col = "black", lwd = 1)
    }
    
    for (k in 1:length(pen)){
      
      m = ord[gsubs[k]] #m is the location of the state in boxlist
      if(is.na(m)) next
      kp = pen[k]      # color number
      ht = laby[k]
      
      
      if(outlier[m]){
        vals = out[group == m]
        if (gnams[k] == "Blank"){
          graphics::points(vals, rep(ht,length(vals)), pch = 1,
                           col = ifelse(useBlack, "black", "#FFFFFF"),
                           cex = cexOutlier,lwd = lwdOutlier)
        } else {
          graphics::points(vals, rep(ht, length(vals)), pch = 1,
                           col = ifelse(useBlack, "black", hdColors[kp]),
                           cex = cexOutlier,lwd = lwdOutlier)
        }
      }
      
      if (gnams[k] == "Blank"){
        
        graphics::polygon(thin[, m], rep(ht, 4) + thiny, col = "#FFFFFF", border = NA)
        graphics::polygon(thick[, m], rep(ht, 4) + thicky, col = "#FFFFFF", border = NA)
        graphics::segments(med[m], ht + medy[1], med[m], ht + medy[2],
                           col = "black", lwd = lwdMedian)
        
      } else {
        
        graphics::polygon(thin[,m], rep(ht, 4) + thiny,col = hdColors[kp], border = NA)
        graphics::polygon(thick[,m], rep(ht, 4) + thicky,col = hdColors[kp], border = NA)
        graphics::segments(med[m],ht + medy[1], med[m], ht + medy[2],
                           col = "black",lwd = lwdMedian)
        
      }
    }
    
  }
}

