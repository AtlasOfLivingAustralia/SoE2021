#' Build UI for shiny app
#'
#' @importFrom shiny navbarPage sidebarLayout sidebarPanel titlePanel fluidRow
#' selectInput plotOutput downloadButton
#' @importFrom bslib bs_theme


soe_ui <- function(){
  navbarPage(
    "State of the Environment 2021 | example data from ALA",
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
      mainPanel(
        fluidRow(
          plotOutput(outputId = "chart_space", width = "100%", height = "75vh"),
          downloadButton("save_chart")
        )
      )
    )
  )
}