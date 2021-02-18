#' server function for SoE data
#'
#' Server stuff goes here
#'
#' @importFrom shiny observeEvent reactiveValues renderPlot

soe_server <- function(input, output, session){

  # save reactive objects
  internal_info <- reactiveValues(
    data = NULL,
    plot = NULL)

  observeEvent(input$calculate, {
    calculating_modal()
    internal_info$data <- get_soe_data(
      # type = input$count_type,
      taxa = input$taxa,
      spatial = input$spatial
      # temporal = input$temporal
    )
    internal_info$plot <- draw_soe_plot(
      data = internal_info$data,
      # type = input$plot_type # not yet implemented
      palette = input$color_scheme,
      reverse_colours = input$color_reverse,
      log_scale = input$log_scale
    )
    removeModal()
  })

  # change color scheme but not raw data
  observeEvent(input$redraw, {
    internal_info$plot <- draw_soe_plot(
      data = internal_info$data,
      # type = input$plot_type # not yet implemented
      palette = input$color_scheme,
      reverse_colours = input$color_reverse,
      log_scale = input$log_scale
    )
  })

  # render the requested plot
  output$chart_space <- renderPlot({
    if(!is.null(internal_info$data)){
      print(internal_info$plot)
    }
  })

  # download handler
  # output$plot_download <- downloadHandler(
  #   filename = function(){
  #     paste
  #   },
  #  content =
  # )
}