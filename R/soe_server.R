#' server function for SoE data
#'
#' Server stuff goes here
#'
#' @importFrom shiny observeEvent reactiveValues renderPlot plotOutput
#' @importFrom galah ala_config
#' @import ggplot2
#' @import leaflet
#' @importFrom mapview mapshot

# @importFrom thematic thematic_shiny
# thematic_shiny()

soe_server <- function(input, output, session){

  df <- reactiveValues(
    current_values = NULL,
    lookup = NULL,
    current_data = NULL,
    plot_bar = NULL,
    plot_heatmap = NULL,
    plot_map = NULL,
    plot_i_map = NULL
  )

  # observeEvent(input$redraw, {
  observe({
    # work out what the input dropdowns currently say & remove irrelevant info
    current_vals <- switch(input$tabs,
      "bar" = {list(
        x = input$x_bar,
        y = input$y_bar,
        log_scale = input$log_bar,
        color = input$colour_bar,
        color_scheme = input$color_scheme_bar,
        color_reverse = input$color_reverse_bar)
      },
      "heatmap" = {list(
          x = input$x_heatmap,
          y = input$y_heatmap,
          z = input$z_heatmap,
          log_scale = input$log_heatmap,
          color_scheme = input$color_scheme_heatmap,
          color_reverse = input$color_reverse_heatmap,
          facet = input$facet_heatmap)
      },
      "map" = {list(
        z = input$z_map,
        map_type = input$map_spatial,
        taxon = input$taxon_map,
        year = input$year_map,
        basis = input$basis_map,
        log_scale = input$log_map,
        color_scheme = input$color_scheme_map,
        color_reverse = input$color_reverse_map,
        facet = input$facet_map)
      },
      "i_map" = {list(
        z = input$z_i_map,
        map_type = input$i_map_spatial,
        taxon = input$taxon_i_map,
        year = input$year_i_map,
        basis = input$basis_i_map
        #log_scale = input$log_i_map,
      )
      }
    )
    df$current_values <- current_vals

    variable_lookup <- switch(input$tabs,
      "bar" = {current_vals[c("x", "color")]},
      "heatmap" = {current_vals[c("x", "y", "facet")]}, # "facet"
      "map" = { current_vals[c("map_type", "facet")]},
      "i_map" = {current_vals[c("map_type")]}) # "facet"

    if (!is.null(current_vals$taxon) && current_vals$taxon != "All") {
      variable_lookup$taxon <- "taxon"
    }

    if (!is.null(current_vals$year) && current_vals$year != "All") {
      variable_lookup$year_group <- "year_group"
    }

    if (!is.null(current_vals$basis) && current_vals$basis != "All") {
      variable_lookup$basisOfRecord <- "basisOfRecord"
    }

    # determine which entry from xtab_list contains the requisite data
    df$lookup <- which(unlist(lapply(
      strsplit(names(data_list), "::"),
      function(a, x){all(a %in% x) & length(a) == length(x)},
      x = unlist(unique(variable_lookup[variable_lookup != "none"]))
    )))

    # save to 'current_data'
    df$current_data <- data_list[[df$lookup]]

    # draw
    switch(input$tabs,
      "bar" = {df$plot_bar <- plot_bar(
        data = df$current_data,
        pars = df$current_values)
      },
      "heatmap" = {df$plot_heatmap <- plot_heatmap(
        data = df$current_data,
        pars = df$current_values)
      },
      "map" = {df$plot_map <- plot_map(
        data = df$current_data,
        pars = df$current_values)
      },
      "i_map" = {df$plot_i_map <- plot_i_map(
        data = df$current_data,
        pars = df$current_values)}
    )
  })

  # testing window
  # output$text_1 <- renderPrint({df$current_values})
  # output$text_2 <- renderPrint({df$current_values})

  # plots
  output$barplot <- renderPlot({
    validate(
      need(df$plot_bar, "Choose data to continue")
    )
    print(df$plot_bar)
  })

  output$heatmap <- renderPlot({
    validate(
      need(df$plot_heatmap, "Choose data to continue")
    )
    print(df$plot_heatmap)
  })

  output$map <- renderPlot({
    validate(
      need(df$plot_map, "Choose data to continue")
    )
    print(df$plot_map)
  })
  output$i_map <- renderLeaflet({
    validate(
      need(df$plot_i_map, "Choose data to continue")
    )
    print(df$plot_i_map)
  })

  # Download plot- ugly but works
  output$download_map <- downloadHandler(
    filename = "map_plot.png",
    content = function(file) {
      ggsave(file)
    }
  )
  output$download_i_map <- downloadHandler(
    filename = "i_map_plot.png",
    content = function(file) {
      mapshot(df$plot_i_map, file = "i_map_plot.png")
    }
  )
  output$download_bar <- downloadHandler(
    filename = "bar_plot.png",
    content = function(file) {
      ggsave(file)
    }
  )
  output$download_heatmap <- downloadHandler(
    filename = "heatmap_plot.png",
    content = function(file) {
      ggsave(file)
    }
  )

  # save modal
  #observeEvent(input$download_modal, {
  #  save_modal()
  #})

  # when 'save' is hit, save the file as requested
  #observeEvent(input$save_data_execute, {
  #  ggsave(
  #    filename = paste(
  #      input$save_filename,
  #      tolower(input$save_type),
  #      sep = "."),
  #    plot = df$plot,
  #    device = tolower(input$save_type)
  #  )
    ## note that the above saves to the app directory
    ## a better choice is to use a download handler:
    # downloadHandler(
    #   filename = paste(
    #     input$save_filename,
    #     input$save_type,
    #     collapse = "."
    #   ),
    #   content = function()
    # )
  #})

}
