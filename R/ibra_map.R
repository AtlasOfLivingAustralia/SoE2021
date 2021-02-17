#' IBRA map
#'
#' Simplified map of IBRA regions
#'
#' @title Map of IBRA regions
#' @name ibra_map
#' @format sf spatial object
#' source \url{http://www.environment.gov.au/metadataexplorer/explorer.jsp}
NULL

# code to generate:
# ibra_map <- sf::read_sf("./spatial/IBRA7_regions/ibra7_regions.shp")
# make lower res for speed reasons
# ibra_map <- sf::st_simplify(ibra_map, dTolerance = 0.05)
# plot basic map of ibra regions
# save(ibra_map, file = "ibra_map.RData")