#' @importFrom viridisLite viridis
#' @import ozmaps
#' @importFrom dplyr inner_join

plot_bar <- function(data, pars){

  ## test code for ordering factor levels (fails)
  # data_tr <- data[order(data[[y]], decreasing = FALSE), ]
  # data_tr[[pars$x]] <- factor(data_tr[[pars$x]],
  #   levels = seq_along(unique(data_tr))[[pars$x]],
  #   labels = data_tr[[pars$x]])

  if(pars$log_scale){
    y_var <- paste0("log(", pars$y, ")")
  }else{
    y_var <- pars$y
  }

  if(pars$color == "none"){

    # choose a colour
    palette <- viridisLite::viridis(
      n = 1,
      option = pars$color_scheme,
      begin = 0.5)

    # draw
    p <- ggplot(data,
      aes_string(
        x = pars$x, # colnames(data)[1],
        y = y_var)) +
      geom_bar(stat = "identity", color = palette, fill = palette) +
      theme_bw() +
      labs(x = label_name(pars$x), y = label_name(pars$y, pars$log_scale))

  }else{

    # set color direction
    if(pars$color_reverse){
      palette_direction <- -1
    }else{
      palette_direction <- 1
    }

    # set palette etc
    color <- pars$color
    color_n <- length(unique(data[[pars$color]]))
    palette <- viridisLite::viridis(
      n = color_n,
      option = pars$color_scheme,
      direction = palette_direction,
      begin = 0.1,
      end = 0.9)

    # draw
    p <- ggplot(data,
      aes_string(
        x = pars$x, # colnames(data)[1],
        y = y_var,
        color = color,
        fill = color)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(
        values = palette,
        aesthetics = c("fill", "color")) +
      theme_bw() + labs(x = label_name(pars$x),
                        y = label_name(pars$y, pars$log_scale))
  }

  return(p)

}

# # facet
# if(is.null(input$time_facet)){
#   facet <- NULL
# }else{
#   facet <- ggplot2::facet_wrap(ggplot2::vars(input$time_facet))
# }


plot_heatmap <- function(data, pars){

  # set log scale
  if(pars$log_scale){
    z_var <- paste0("log(", pars$z, ")")
  }else{
    z_var <- pars$z
  }

  # set color direction
  if(pars$color_reverse){
    palette_direction <- -1
  }else{
    palette_direction <- 1
  }

  p <- ggplot(data,
    aes_string(
      x = pars$x, # colnames(data)[1],
      y = pars$y,
      fill = z_var)) +
    geom_tile() +
    scale_fill_viridis(
      option = pars$color_scheme,
      direction = palette_direction) +
    theme_bw() + labs(x = label_name(pars$x),
                      y = label_name(pars$y),
                      fill = label_name(pars$z, pars$log_scale))
  return(p)

}

plot_map <- function(data, pars) {
  # join data to state/ibra map
  if(pars$log_scale){
    z_var <- paste0("log(", pars$z, ")")
  }else{
    z_var <- pars$z
  }
  
  if(pars$color_reverse){
    palette_direction <- -1
  }else{
    palette_direction <- 1
  }
  
  if(pars$map_type == "australianStatesAndTerritories") {
    data <- inner_join(ozmaps::ozmap_states, data,
                       by = c("NAME" = "australianStatesAndTerritories"))
  } else {
    data <- inner_join(ibra_map, data,
                       by = c("REG_NAME_7" = "iBRA7Regions"))
  }
  p <- ggplot(data) +
    geom_sf(aes_string(fill = z_var), color = "grey50", size = 0.2) +
    lims(x = c(110, 155), y = c(-45, -10)) +
    scale_fill_viridis(
      option = "viridis",
      direction = palette_direction) +
    theme_bw() +
    labs(fill = label_name(pars$z, pars$log_scale))
  return(p)
}


## Plot helper functions

label_name <- function(v, log = FALSE) {
  name <- switch(v,
         "year_group" = "Year",
         "taxon" = "Taxon",
         # "threatened" = "Threatened Status",
         "basisOfRecord" = "Basis of Record",
         "australianStatesAndTerritories" = "States",
         "iBRA7Regions" = "IBRA Regions",
         "national_parks" = "National Parks",
         "n_records" = "Record count",
         "n_spp" = "Species count")
  if (log) {
    name <- paste0("log(", name, ")")
  }
  name
}
