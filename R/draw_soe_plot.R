#' draw a plot
#'
#' Internal function called by SoE app
#'
#' @importFrom ggplot2 ggplot geom_sf lims theme_bw aes
#' @importFrom viridis scale_fill_viridis
#' @export draw_soe_plot

draw_soe_plot <- function(
  data, # returned by get_soe_data()
  palette,
  reverse_colours = FALSE,
  log_scale = FALSE,
  count_type
){
  if(reverse_colours){
    viridis_direction <- (-1)
  }else{
    viridis_direction <- 1
  }

  # if(log_scale){
  #   transform_value <- "log"
  # }else{
  #   transform_value < -"identity"
  # }

  # only implemented for spatial data so far
  p <- ggplot(data) +
    geom_sf(aes(fill = log_fun(count, log_scale)), color = "grey50", size = 0.2) +
    lims(x = c(110, 155), y = c(-45, -10)) +
    scale_fill_viridis(
      option = palette,
      direction = viridis_direction,
      name = leg_title(count_type, log_scale)) +
      # trans = transform_value) +
    theme_bw()

  return(p)
}
# draw_soe_plot(data = test, palette = "magma", log_scale = TRUE, reverse_colours = TRUE)

log_fun <- function(x, log){
  if(log){log(x + 1)}else{x}
}

leg_title <- function(type, log) {
  if (type == "record"){ type <- "records"}
  txt <- paste0("Number of ", type)
  if (log) { txt <- paste0("Log(", txt, ")")}
  txt
}