#' Build UI for shiny app
#'
#' @importFrom shiny fluidPage titlePanel sidebarLayout sidebarPanel mainPanel
#' selectInput plotOutput downloadButton checkboxInput
#' @importFrom bslib bs_theme

soe_ui <- function(){
  fluidPage(
    titlePanel("State of the Environment 2021"),
    br(),
    theme = bs_theme(version = 4, bootswatch = "minty"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "count_type",
          label = "Show data on:",
          choices = c(
            "Number of records" = "record",
            "Number of species" = "species"),
          width = "250px"),
        selectInput(
          inputId = "plot_type",
          label = "Plot type:",
          choices = c(
            "Map",
            "Heatmap",
            "Barchart"),
          width = "250px"),
        # breakdown section
        hr(),
        selectInput(
          inputId = "taxa",
          label = "Taxonomic breakdown:",
          choices = c(
            "All species" = "all",
            "Mammals" = "mammalia",
            "Birds" = "aves",
            "Reptiles" = "reptilia",
            "Frogs" = "amphibia",
            "Fishes" = "actinopterygii",
            "Insects" = "insecta",
            "Plants" = "plantae"
          ),
          width = "250px"),
        # note: may need reactive UI to show a checkboxGroupInput of taxa to include
        selectInput(
          inputId = "spatial",
          label = "Spatial Breakdown:",
          choices = c(
            "None" = "all",
            "IBRA Regions" = "ibra"),
          selected = "ibra",
          width = "250px"),
        selectInput(
          inputId = "temporal",
          label = "Temporal Breakdown:",
          choices = c(
            "None" = "all",
            "5-Year Increments" = "increments"),
          width = "250px"),
        actionButton(
          inputId = "calculate",
          label = "Calculate",
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
        actionButton(
          inputId = "redraw",
          label = "Redraw",
          width = "250px"),
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
        downloadButton(
          outputId = "plot_download",
          label = "Save plot",
          width = "250px")
      ),
          # if "map", map by IBRA regions only for now
          # could choose to show a spatial heatmap by hex too
      mainPanel(
        plotOutput(outputId = "chart_space", width = "100%", height = "75vh")
      )
    )
  )
}