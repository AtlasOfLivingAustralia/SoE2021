#' server function for SoE data
#'
#' Server stuff goes here
#'
#' @importFrom shiny observeEvent reactiveValues renderPlot plotOutput
#' @importFrom galah ala_config
#' @importFrom ggplot2 ggplot aes geom_line geom_point theme_bw labs

soe_server <- function(input, output, session){

  # time series plot
  output$time_plot <- renderPlot({

    # get relevant data
    cols_tr <- c(
      "year_group",
      input$time_colour,
      input$time_facet)
    cols_tr <- cols_tr[!is.null(cols_tr)]
    xtab_check <- unlist(lapply(
      strsplit(names(xtab_list), "::"),
      function(a){
        (length(a) == length(cols_tr)) & all(cols_tr %in% a)
      }))
    if(any(xtab_check)){
      data_tr <- xtab_list[[xtab_check]]
    }else{
      data_tr <- NULL
    }

    # facet
    if(is.null(input$time_facet)){
      facet <- NULL
    }else{
      facet <- ggplot2::facet_wrap(ggplot2::vars(input$time_facet))
    }

    # color
    if(is.null(input$time_colour)){
      time_aes <- ggplot2::aes_string(
        x = "year_group",
        y = input$time_y,
        size = input$time_size,
        group = 1)
    }else{
      time_aes <- ggplot2::aes_string(
        x = "year_group",
        y = input$time_y,
        size = input$time_size,
        group = input$time_colour,
        colour = input$time_colour)
    }

    # draw
    ggplot(data_tr, time_aes) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::theme_bw() +
      ggplot2::labs(
        x = "Years",
        y = switch(input$time_y,
          "n_records" = "Number of Records",
          "n_spp" = "Number of Species"
      )) +
      facet
  })

  output$heatmap_plot <- renderPlot({
    ggplot(xtab_list$year_group,
      ggplot2::aes_string(x = "year_group", y = input$time_y)) +
      ggplot2::geom_path() +
      ggplot2::geom_point() +
      ggplot2::theme_bw()
  })

  output$map_plot <- renderPlot({
    ggplot(xtab_list$year_group,
      ggplot2::aes_string(x = "year_group", y = input$time_y)) +
      ggplot2::geom_path() +
      ggplot2::geom_point() +
      ggplot2::theme_bw()
  })


  # save reactive objects
  # internal_info <- reactiveValues(
  #   data = NULL,
  #   plot = NULL)

#  observeEvent(input$redraw, {
    # calculating_modal()
    # internal_info$data <- get_soe_data(
    #   type = input$count_type,
    #   taxa = input$taxa,
    #   spatial = input$spatial
    #   # temporal = input$temporal
    # )
    # internal_info$plot <- draw_soe_plot(
    #   data = internal_info$data,
    #   # type = input$plot_type # not yet implemented
    #   palette = input$color_scheme,
    #   reverse_colours = input$color_reverse,
    #   log_scale = input$log_scale,
    #   count_type = input$count_type
    # )
    # removeModal()
  # })

  # # change color scheme but not raw data
  # observeEvent(input$redraw, {
  #   internal_info$plot <- draw_soe_plot(
  #     data = internal_info$data,
  #     # type = input$plot_type # not yet implemented
  #     palette = input$color_scheme,
  #     reverse_colours = input$color_reverse,
  #     log_scale = input$log_scale,
  #     count_type = input$count_type
  #   )
  # })
  #
  # # render the requested plot
  # output$chart_space <- renderPlot({
  #   if(!is.null(internal_info$data)){
  #     print(internal_info$plot)
  #   }
  # })

  # download handler
  # output$plot_download <- downloadHandler(
  #   filename = function(){
  #     paste
  #   },
  #  content =
  # )
}