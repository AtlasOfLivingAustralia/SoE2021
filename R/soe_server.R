#' server function for SoE data
#'
#' Server stuff goes here
#'
#' @importFrom ggplot2 ggplot lims theme_bw geom_sf

soe_server <- function(input, output, session){

  #
  # observeEvent({
  #
  # })

  # render the requested plot
  output$chart_space <- renderPlot({
    ggplot(ibra_map) +
      geom_sf() +
      lims(x = c(110, 155), y = c(-45, -10)) +
      theme_bw()
  })

  ## if save_plot selected, open a modal to allow save options
  observeEvent(input$save_plot, {save_modal()})

  # download handler
  # output$plot_download <- downloadHandler(
  #   filename = function(){
  #     paste
  #   },
  #  content = 
  # )
}