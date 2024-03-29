#' draw plots within shiny
#'
#' Internal functions called by SoE app
#'
#' @importFrom viridisLite viridis
#' @import ozmaps
#' @importFrom dplyr inner_join filter
#' @importFrom ggplot2 ggplot geom_sf lims theme_bw aes
#' @importFrom viridis scale_fill_viridis
#' @importFrom scales comma

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

  if(pars$color == "none"){

    # choose a colour
    if (pars$color_scheme == "ala") {
      palette <- ala_pal(1)
    } else {
      palette <- viridisLite::viridis(
        n = 1,
        option = pars$color_scheme,
        begin = 0.5)
    }

    # draw
    p <- ggplot(data,
      aes_string(
        x = pars$x, # colnames(data)[1],
        y = pars$y)) +
      geom_bar(stat = "identity", color = palette, fill = palette) +
      theme_bw() +
      labs(x = label_name(pars$x), y = label_name(pars$y, pars$log_scale)) +
      bar_style() + scale_y_continuous(labels = comma, trans = scale_trans(pars$log_scale))

  }else{

    # set color direction
    palette_direction <- palette_dir(pars$color_reverse)

    # set palette etc
    color <- pars$color
    color_n <- length(unique(data[[pars$color]]))


    if (pars$color_scheme == "ala") {
      palette <- ala_pal(color_n, reverse = pars$color_reverse)
    } else {
      palette <- viridisLite::viridis(
        n = color_n,
        option = pars$color_scheme,
        direction = palette_direction,
        begin = 0.1,
        end = 0.9)
    }

    # draw
    p <- ggplot(data,
      aes_string(
        x = pars$x, # colnames(data)[1],
        y = pars$y,
        color = color,
        fill = color)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      scale_fill_manual(
        values = palette,
        aesthetics = c("fill", "color")) +
      theme_bw() + labs(x = label_name(pars$x),
                        y = label_name(pars$y, pars$log_scale),
                        fill = label_name(pars$color),
                        color = label_name(pars$color)) +
      bar_style() +
      scale_y_continuous(labels = comma, trans = scale_trans(pars$log_scale)) +
      legend_style()
  }

  return(p)

}

plot_heatmap <- function(data, pars){

  if (pars$x == "australianStatesAndTerritories") {
    data$australianStatesAndTerritories <-
      state_abb(data$australianStatesAndTerritories)
  }

  # set color direction
  palette_direction <- palette_dir(pars$color_reverse)

  if (pars$color_scheme == "ala") {
    scale_fill <- scale_fill_gradientn(colours = ala_pal(2, pars$color_reverse),
                                       labels = comma, trans = scale_trans(pars$log_scale))
  } else {
    scale_fill <- scale_fill_viridis(
      option = pars$color_scheme,
      direction = palette_direction,
      labels = comma, trans = scale_trans(pars$log_scale))
  }
  p <- ggplot(data,
    aes_string(
      x = pars$x, # colnames(data)[1],
      y = pars$y,
      fill = pars$z)) +
    geom_tile() +
    scale_fill +
    theme_bw() + labs(x = label_name(pars$x),
                      y = label_name(pars$y),
                      fill = label_name(pars$z, pars$log_scale)) +
    axes_style() + legend_style()
  if (pars$facet != "none") {
    p <- p + facet_wrap(as.formula(paste("~", pars$facet)))
  }
  return(p)

}

plot_map <- function(data, pars) {

  palette_direction <- palette_dir(pars$color_reverse)

  data <- build_map_data(data, pars)

  if (pars$color_scheme == "ala") {
    scale_fill <- scale_fill_gradientn(colours = ala_pal(2, pars$color_reverse),
                                       labels = comma, trans = scale_trans(pars$log_scale))
  } else {
    scale_fill <- scale_fill_viridis(option = pars$color_scheme,
                                     direction = palette_direction,
                                     labels = comma, trans = scale_trans(pars$log_scale))
  }

  p <- ggplot(data) +
    geom_sf(aes_string(fill = pars$z), color = "grey50", size = 0.02) +
    lims(x = c(110, 155), y = c(-45, -10)) +
    scale_fill +
    theme_bw() +
    labs(fill = label_name(pars$z, pars$log_scale)) +
    # remove axes labels
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) + legend_style()

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

  data <- build_map_data(data, pars)

  colPalette <- "Reds"
  colPal <- colorNumeric(
    palette = colPalette,
    domain = data[[pars$z]])

  # Leaflet visualisation
  p <- leaflet(data = data) %>%
    setView(lng = 133.88362, lat = -23.69748, zoom = 4) %>%
    addTiles() %>%
    addPolygons(fillColor = colPal(data[[pars$z]]),
                fillOpacity = .8, color = "#111111", weight = 1, stroke = TRUE,
                highlightOptions = highlightOptions(color = "#222222", weight = 3,
                                                    bringToFront = TRUE),
                label = build_labels(data, pars),
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 5px"),
                  textsize = "12px",
                  direction = "auto")) %>%
    addLegend("bottomright", pal = colPal, values = data[[pars$z]],
              title = title,
              opacity = 1)
  return(p)
}

## Plot helper functions

label_name <- function(v, log = FALSE) {
  name <- switch(v,
         "year_group" = "Year",
         "taxon" = "Taxon",
         "threat_status" = "Threat Status",
         "basisOfRecord" = "Basis of Record",
         "australianStatesAndTerritories" = "States",
         "iBRA7Regions" = "IBRA Regions",
         "national_parks" = "National Parks",
         "n_records" = "Record count",
         "n_spp" = "Species count")
  #if (log) {
  #  name <- paste0("log(", name, ")")
  #}
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
build_labels <- function(data, pars) {
  taxa <- pars$taxon
  threat_status <- pars$threat
  if (pars$map_type == "iBRA7Regions") {
    labels <- sprintf("<strong>%s</strong><br/>Code: %s<br/>ID: %g<br/>Area (km<sup>2</sup>): %g<br/><br/><strong>Taxa: %s</strong><br/>Threat status: %s<br/>Records Count: %g",
                      data$REG_NAME_7, data$REG_CODE_7, data$REC_ID, data$SQ_KM,
                      taxa, threat_status, data[[pars$z]]) %>%
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

build_map_data <- function(data, pars) {
  if (pars$taxon != "All") {
    data <- data %>% filter(taxon == pars$taxon)
  }
  #if (pars$year != "All") {
  #  data <- data %>% filter(year_group == pars$year)
  #}
  if (pars$basis != "All") {
    data <- data %>% filter(basisOfRecord == pars$basis)
  }
  if (pars$threat != "All") {
    data <- data %>% filter(threat_status == pars$threat)
  }
  
  if (pars$griis != "All") {
    data <- data %>% filter(griis_status == pars$griis)
  }

  if (pars$map_type == "australianStatesAndTerritories") {
    data <- inner_join(ozmaps::ozmap_states, data,
                       by = c("NAME" = "australianStatesAndTerritories"))
  } else {
    data <- inner_join(ibra_map, data,
                       by = c("REG_NAME_7" = "iBRA7Regions"))
  }
  data
}

palette_dir <- function(reverse) {
  if(reverse){
    palette_direction <- -1
  }else{
    palette_direction <- 1
  }
  palette_direction
}

scale_trans <- function(log_scale) {
  if (log_scale) "log10" else "identity"
}

bar_style <- function() {
  axes_style() + theme(legend.position = "bottom")
}

axes_style <- function() {
  theme(
    axis.title = element_text(family = "Roboto", size = 14),
    axis.text = element_text(family = "Roboto", size = 14)
  )
}

legend_style <- function() {
  theme(
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12)
  )
}

log_fun <- function(x, log){
  if(log){log(x + 1)}else{x}
}

leg_title <- function(type, log) {
  if (type == "record"){ type <- "records"}
  txt <- paste0("Number of ", type)
  if (log) { txt <- paste0("Log(", txt, ")")}
  txt
}
