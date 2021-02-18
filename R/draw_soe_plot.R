#' draw a plot
#'
#' Internal function called by SoE app
#'
#' @importFrom ggplot2 ggplot geom_sf lims theme_bw
#' @importFrom viridis scale_fill_viridis
#' @export draw_soe_plot

draw_soe_plot <- function(
  data, # returned by get_soe_data()
  palette,
  reverse_colours
){
  if(reverse_colours){
    viridis_direction <- (-1)
  }else{
    viridis_direction <- 1
  }

  # only implemented for spatial data so far
  ggplot(data) +
    geom_sf(aes(fill = count), color = "grey50", size = 0.2) +
    lims(x = c(110, 155), y = c(-45, -10)) +
    scale_fill_viridis(option = palette, direction = reverse_direction) +
    theme_bw()
}