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

  observeEvent({
    # what info to get:
    # input$count_type
    input$taxa
    input$spatial
    # input$temporal
    # how to present this info:
    input$plot_type
    input$color_scheme
    input$reverse_colours
  },{
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
      reverse_colours = input$color_reverse
    )
  })

  # render the requested plot
  output$chart_space <- renderPlot({
    # if(!is.null(internal_info$plot)){
    #   internal_info$plot
    # }
    ggplot(ibra_map) +
      geom_sf() +
      lims(x = c(110, 155), y = c(-45, -10)) +
      theme_bw()
  })

  # download handler
  # output$plot_download <- downloadHandler(
  #   filename = function(){
  #     paste
  #   },
  #  content =
  # )
}