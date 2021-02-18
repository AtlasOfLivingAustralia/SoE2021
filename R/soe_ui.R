#' Build UI for shiny app
#'
#' @importFrom shiny fluidPage titlePanel sidebarLayout sidebarPanel mainPanel
#' selectInput plotOutput downloadButton checkboxInput
#' @importFrom bslib bs_theme

soe_ui <- function(){
  fluidPage(
    titlePanel("State of the Environment 2021 | example data from ALA"),
    br(),
    theme = bs_theme(version = 4, bootswatch = "minty"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "count_type",
          label = "Show data on:",
          choices = c(
            "Number of records" = "n_records",
            "Number of species" = "n_species"),
          width = "250px"),
        selectInput(
          inputId = "taxa_shown",
          label = "Taxonomic breakdown:",
          choices = c(
            "All species",
            "Show individual taxa"),
          width = "250px"),
        # note: may need reactive UI to show a checkboxGroupInput of taxa to include
        selectInput(
          inputId = "plot_type",
          label = "Plot type:",
          choices = c(
            "Map",
            "Heatmap",
            "Barchart"),
          width = "250px"),
        # colors
        hr(),
        selectInput(
          inputId = "color_scheme",
          label = "Colour scheme:",
          choices = c(
            "viridis",
            "magma",
            "inferno",
            "plasma",
            "cividis"),
          width = "250px"),
        checkboxInput(
          inputId = "color_reverse",
          label = "Reverse colors",
          value = FALSE),
        checkboxInput(
          inputId = "log_scale",
          label = "Log scale",
          value = FALSE),
        # download options
        hr(),
        selectInput(
          inputId = "download_type",
          label = "Download format:",
          choices = c(
            "pdf",
            "png",
            "jpeg"),
          width = "250px"),
        downloadButton("plot_download", "Save plot")
      ),
          # if "map", map by IBRA regions only for now
          # could choose to show a spatial heatmap by hex too
      mainPanel(
        plotOutput(outputId = "chart_space", width = "100%", height = "75vh")
      )
    )
  )
}