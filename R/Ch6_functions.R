# Functions


# understanding OOP -----------------------------------------------

# delete this code later
if (FALSE) {
  mySummary <- function(x,...) {
    UseMethod("mySummary")
  }
  
  mySummary.numeric <- function(x, na.rm = FALSE) {
    sum(x, na.rm = na.rm)
  }
  
  mySummary.default <- function(x) {
    'this is the default'
  }
  
  mySummary.data.frame <- function(x) {
    names(x)
  }
  
  mySummary('text')
  mySummary(1:10)
  mySummary(c(5, NA))
  mySummary(c(5, NA), na.rm = TRUE)
  mySummary(cars)
}



# converting points to polygons (circles) ---------------------------------


#' Calculate dist for st_buffer
#'
#' @description If this function is included in a package it would probably
#' not be exported, but instead be used inside other functions that 
#' are exported
#'
#' @param x an sf object
#' @param radius percent of the width of the sf object that you want
#' the radius of the circles around the point to be
#'
#' @return a single number (to be passed to dist argument of sf::st_buffer)
calc_buffer_dist <- function(x, radius) {
  
  stopifnot('sf' %in% class(x))
  
  bbox <- sf::st_bbox(x)
  
  # creating an object that gives the bottom two corners of the extent
  # of the points
  # passing it the crs from x is key here
  corners <- sf::st_sfc(st_point(bbox[c('xmin', 'ymin')]), # lower left corner
                        st_point(bbox[c('xmax', 'ymin')]),
                        crs = st_crs(x)) # lower right corner
  
  # note what units the distance is calculated in depends on the crs
  # and so this distance is then in the correct units that are needed
  # for st_buffer (for the given object)
  width <- st_distance(corners)[2, 1]
  
  # multiply the distance by the percent of the width the radius should be
  out <- width*radius/100
  out
}




#' Convert points to circles
#'
#' @param x dataframe or sf object
#' @param ... arguments passed to the methods
#'
#' @return sf object
#' @export
#'
#' @examples
points2circles <- function(x, ...) {
  UseMethod('points2circles')
}

# method for dataframe objects
points2circles.data.frame <- function(x, coords, radius = 5, crs = NA) {
  points_sf <- sf::st_as_sf(x, coords = coords, crs = crs)
  # create buffer around the points (i.e. circular polygons)
  dist <- calc_buffer_dist(points_sf, radius = radius)
  circles_sf <- sf::st_buffer(points_sf,dist = dist, nQuadSegs = 4)
  circles_sf
}

# method for sf objects
points2circles.sf <- function(x, radius = 5) {
  # NEXT--make sure the geometries of x are all actually points
  # whe crs is lat/long the final 'circles' aren't round (b/ the radius
  # is actually equidistant on the ground), consider fixing. 
  # consider making this an argument (i.e. draw circles--for the given
  # projection, or draw equidistant
  # polygons)
  points_sf <- x
  dist <- calc_buffer_dist(points_sf, radius = radius)
  circles_sf <- st_buffer(points_sf,dist = dist, nQuadSegs = 4)
  circles_sf
}



# repel points ------------------------------------------------------------


#' wrapper around FField::FFieldPtRep
#' 
#' @description normalizes points so both .x and .y range between 0 and 100
#' before they're passed to FFieldPtRep. That way the default values 
#' used in that function work pretty well. This function is used inside
#' point_repel()
#'
#' @param .x numeric vector
#' @param .y numeric vector
#' @param rep.fact repulsion force factor
#' @param rep.dist.lmt repulsion distance limit
#' @param attr.fact attraction force factor
#' @param adj.max maximum position adjustment at each iteration
#' @param adj.lmt position adjustment limit at which the simulation stops.
#' @param iter.max 	the maximum number of iterations beyond which simulation 
#' will end and a warning will be reported.
#'
#' @return dataframe with two columns x and y
point_repel_xy <- function(.x, .y,
                           rep.fact, 
                           rep.dist.lmt, 
                           attr.fact, 
                           adj.max, 
                           adj.lmt,
                           iter.max) {
  
  # first make  coordinates positive (with min == 0)
  # (I'm not yet sure if this is necessary)
  x_pos <- .x - min(.x)
  
  y_pos <- .y - min(.y)
  
  # Normalize coordinates to maintain constant aspect ratio
  # (as per FField documentation)
  x.fact <- 100 / max(x_pos)
  y.fact <- 100 / max(y_pos)
  
  coords_norm <- cbind(x_pos * x.fact, y_pos * y.fact)
  
  coords_norm2 <- FField::FFieldPtRep(coords = coords_norm, 
                                      rep.fact = rep.fact, 
                                      rep.dist.lmt = rep.dist.lmt, 
                                      attr.fact = attr.fact, 
                                      adj.max = adj.max, 
                                      adj.lmt = adj.lmt,
                                      iter.max = iter.max)
  # convert jittered coordinates back to original units
  out <- data.frame(
    x = coords_norm2[[1]]/x.fact + min(.x),
    y = coords_norm2[[2]]/y.fact + min(.y)
  )
  out
}

# Continue here--this function hasn't been developed yet
#' Repel points that are close to each other
#'
#' @param x a dataframe or sf object
#' @param coords if 
#' @param ... arguments passed to the methods
#'
#' @return object of same class as x, but with close points repelled from
#' each other
#' @export
#'
#' @examples
#' df = data.frame(x = rnorm(30), y = rnorm(30), z = 1:30)
#' # when using a dataframe
#' plot(df$x, df$y)
#' df_repelled <- point_repel(df, coords = c("x", "y"))
#' plot(df_repelled$x, df_repelled$y)
#' 
#' # when using a simple feature object
#' x <- sf::st_as_sf(df, coords = c("x", "y"))
#' plot(x)
#' plot(point_repel(x))
point_repel <- function(x, ...) {
  UseMethod('point_repel')
}

# method for sf objects
point_repel.sf <- function(x, 
                           rep.fact = 20, 
                           rep.dist.lmt = 10, 
                           attr.fact = 0.2, 
                           adj.max = 0.1, 
                           adj.lmt = 0.5,
                           iter.max = 10000) {
  
  stopifnot(
    'sf' %in% class(x)
  )
  
  # confirm all geometry elements are points
  if(any(sf::st_geometry_type(x) != "POINT")) {
    stop("geometry must contain only points")
  }
  
  xy <- sf::st_coordinates(x)
  
  df_repelled <- point_repel_xy(.x = xy[, 1], 
                                 .y = xy[, 2],
                                 rep.fact = rep.fact, 
                                 rep.dist.lmt = rep.dist.lmt, 
                                 attr.fact = attr.fact, 
                                 adj.max = adj.max, 
                                 adj.lmt = adj.lmt,
                                 iter.max = iter.max)
  
  # replace original geometry with repelled points
  out <- x
  out$geometry <- NULL
  out$.x <- df_repelled$x
  out$.y <- df_repelled$y
  
  # convert back to sf object
  out <- sf::st_as_sf(out, coords = c('.x', '.y'),
                      crs = sf::st_crs(x))
  out
}

# method for dataframes
point_repel.data.frame <- function(x, 
                       coords,
                       rep.fact = 20, 
                       rep.dist.lmt = 10, 
                       attr.fact = 0.2, 
                       adj.max = 0.1, 
                       adj.lmt = 0.5,
                       iter.max = 10000){
  
  stopifnot(
    is.data.frame(x),
    is.vector(coords),
    length(coords) == 2,
    coords %in% names(x)
  )
  
  # repelling points
  df_repelled <- point_repel_xy(.x =  x[[coords[1]]], 
                                 .y = x[[coords[2]]],
                                 rep.fact = rep.fact, 
                                 rep.dist.lmt = rep.dist.lmt, 
                                 attr.fact = attr.fact, 
                                 adj.max = adj.max, 
                                 adj.lmt = adj.lmt,
                                 iter.max = iter.max)
  
  # adding the repelled points back into the original dataframe
  out <- x
  out[[coords[1]]] <- df_repelled$x
  out[[coords[2]]] <- df_repelled$y
  out
  
}


