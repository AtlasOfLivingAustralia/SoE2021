#' @importFrom viridisLite viridis
#' @import ozmaps
#' @importFrom dplyr inner_join filter

plot_bar <- function(data, pars){

  ## test code for ordering factor levels (fails)
  # data_tr <- data[order(data[[y]], decreasing = FALSE), ]
  # data_tr[[pars$x]] <- factor(data_tr[[pars$x]],
  #   levels = seq_along(unique(data_tr))[[pars$x]],
  #   labels = data_tr[[pars$x]])
  
  if (pars$x == "australianStatesAndTerritories") {
    data$australianStatesAndTerritories <-
      state_abb(data$australianStatesAndTerritories)
  }

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

  if (pars$x == "australianStatesAndTerritories") {
    data$australianStatesAndTerritories <-
      state_abb(data$australianStatesAndTerritories)
  }

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
  if (pars$facet != "none") {
    p <- p + facet_wrap(as.formula(paste("~", pars$facet)))
  }
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
  
  if (pars$taxon != "All") {
    data <- data %>% filter(taxon == pars$taxon)
  }
  
  if (pars$year != "All") {
    data <- data %>% filter(year_group == pars$year)
  }
  
  if (pars$basis != "All") {
    data <- data %>% filter(basisOfRecord == pars$basis)
  }
  
  p <- ggplot(data) +
    geom_sf(aes_string(fill = z_var), color = "grey50", size = 0.2) +
    lims(x = c(110, 155), y = c(-45, -10)) +
    scale_fill_viridis(
      option = pars$color_scheme,
      direction = palette_direction) +
    theme_bw() +
    labs(fill = label_name(pars$z, pars$log_scale)) +
    # remove axes labels
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()
    )
  
  if (pars$facet != "none") {
    p <- p + facet_wrap(as.formula(paste("~", pars$facet)))
  }
  return(p)
}


plot_i_map <- function(data, pars) {
  if (pars$z == "n_records") {
    title <- "Record Count"
  } else {
    title <- "Species Count"
  }
  
  #if(pars$log_scale){
  #  z_var <- paste0("log(", pars$z, ")")
  #}else{
  #  z_var <- pars$z
  #}
  
  if (pars$taxon != "All") {
    data <- data %>% filter(taxon == pars$taxon)
  }
  
  if (pars$year != "All") {
    data <- data %>% filter(year_group == pars$year)
  }
  
  if (pars$basis != "All") {
    data <- data %>% filter(basisOfRecord == pars$basis)
  }

  data <- inner_join(ibra_map, data,
                     by = c("REG_NAME_7" = "iBRA7Regions"))
  
  colPalette <- "Reds" 
  colPal <- colorNumeric(
    palette = colPalette,
    domain = data[[z_var]])
  
  # Leaflet visualisation
  p <- leaflet(data = data) %>%
    setView(lng = 133.88362, lat = -23.69748, zoom = 4) %>%
    addTiles() %>%
    addPolygons(fillColor = colPal(data[[z_var]]),
                fillOpacity = .8, color = "#111111", weight = 1, stroke = TRUE,
                highlightOptions = highlightOptions(color = "#222222", weight = 3, bringToFront = TRUE),
                label = build_labels(data, pars, z_var),
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 5px"),
                  textsize = "12px",
                  direction = "auto")) %>%
    addLegend("bottomright", pal = colPal, values = data[[z_var]],
              title = title,
              opacity = 1)
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

state_abb <- function(states) {
  abbs <- unlist(lapply(states, function(s) {
    switch(s, 
           "Australian Capital Territory" = "ACT",
           "Queensland" = "QLD",
           "Victoria" = "VIC",
           "New South Wales" = "NSW",
           "Tasmania" = "TAS",
           "Western Australia" = "WA",
           "Northern Territory" = "NT",
           "South Australia" = "SA"
    )
  }))
}

# TODO: enable taxa and period-specific labels
build_labels <- function(data, pars, z_var) {
  taxa <- pars$taxon
  period <- pars$year
  if (pars$map_type == "iBRA7Regions") {
    labels <- sprintf("<strong>%s</strong><br/>Code: %s<br/>ID: %g<br/>Area (km<sup>2</sup>): %g<br/><br/><strong>Taxa: %s</strong><br/>Period: %s<br/>Records Count: %g",
                      data$REG_NAME_7, data$REG_CODE_7, data$REC_ID, data$SQ_KM,
                      taxa, period, data[[z_var]]) %>%
      lapply(htmltools::HTML)
  } else if (pars$map_type == "IMCRA") {
    labels <- sprintf("<strong>%s</strong><br/>Code: %s<br/>ID: %g<br/>Area (km<sup>2</sup>): %g<br/><br/><strong>Taxa: %s</strong><br/>Records Count: %g",
                      data$MESO_NAME, data$MESO_ABBR, data$MESO_NUM, data$AREA_KM2, taxa, data$z_var) %>%
      lapply(htmltools::HTML)
  } else if (pars$map_type == "CAPAD16") {
    labels <- sprintf("<strong>%s</strong><br/>Code: %s<br/>ID: %s<br/>Area (km<sup>2</sup>): %g<br/><br/><strong>Taxa: %s</strong><br/>Records Count: %g",
                      data$NAME, data$RES_NUMBER, data$PA_ID, data$GIS_AREA, taxa, data$z_var) %>%
      lapply(htmltools::HTML)
  } else {
    print('Not a valid region name.')  
  }
}
