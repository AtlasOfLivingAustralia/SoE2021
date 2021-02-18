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
        # titlePanel("Choose settings:"),
        fluidRow(
          selectInput(
            inputId = "plot_type",
            label = "Plot type:",
            choices = c(
              "map",
              "heatmap",
              "barchart"
            )
          )
        )
      ),
          # if "map", map by IBRA regions only for now
          # could choose to show a spatial heatmap by hex too
      mainPanel(
        plotOutput(outputId = "chart_space", width = "100%", height = "75vh")
      )
    )
  )
}