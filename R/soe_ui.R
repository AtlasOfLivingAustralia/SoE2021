soe_ui <- function(){
  sidebarLayout(
    sidebarPanel(
      titlePanel("ALA / SoE data viz"),
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
    fluidRow(
      plotOutput(inputId = "chart_space", width = "100%", height = "75vh"),
      downloadButton("save_chart")
    )
  )
}