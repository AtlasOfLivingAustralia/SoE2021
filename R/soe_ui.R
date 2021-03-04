#' Build UI for shiny app
#'
#' @importFrom shiny fluidPage titlePanel sidebarLayout sidebarPanel mainPanel
#' selectInput plotOutput downloadButton checkboxInput tabPanel
#' @importFrom bslib bs_theme

soe_ui <- function(){
  fluidPage(
    titlePanel("State of the Environment 2021"),
    theme = bs_theme(version = 4, bootswatch = "minty", secondary = "#F26649"),
    tabsetPanel(type = "pills",
      tabPanel("Time Series",
        br(),
        sidebarLayout(
          sidebarPanel(
            # y
            selectInput(
              inputId = "time_y",
              label = "Y Axis:",
              choices = c(
                "Number of records" = "n_records",
                "Number of species" = "n_spp")),
            checkboxInput(
              inputId = "log_scale",
              label = "Log scale",
              value = FALSE),
            # size
            selectInput(
              inputId = "time_size",
              label = "Size:",
              choices = c(
                "Number of records" = "n_records",
                "Number of species" = "n_spp")),
            # color
            selectInput(
              inputId = "time_color",
              label = "Colour:",
              choices = c(
                "None" = "none",
                "Year" = "year_group",
                "Taxon" = "taxon",
                # "Threatened Status" = "threatened",
                "Basis of Record" = "basisOfRecord",
                "States" = "australianStatesAndTerritories",
                "IBRA Regions" = "iBRA7Regions",
                "National Parks" = "national_parks"),
              selected = "none"),
            # facet
            selectInput(
              inputId = "time_facet",
              label = "Facet:",
              choices = c(
                "None" = "none",
                "Year" = "year_group",
                "Taxon" = "taxon",
                # "Threatened Status" = "threatened",
                "Basis of Record" = "basisOfRecord",
                "States" = "australianStatesAndTerritories",
                "IBRA Regions" = "iBRA7Regions",
                "National Parks" = "national_parks"),
              selected = "none"),
            add_color_options(),
            hr(),
            actionButton(
              inputId = "download_modal",
              label = "Download",
              width = "100%")
          ),
          mainPanel(
            plotOutput(outputId = "chart_space", width = "100%", height = "75vh")
          )
        )
      ),
      tabPanel("Heatmap",
        br(),
        sidebarLayout(
          sidebarPanel(
            selectInput(
              inputId = "heatmap_x",
              label = "X Axis:",
              choices = c(
                "Year" = "year_group",
                "Taxon" = "taxon",
                # "Threatened Status" = "threatened",
                "Basis of Record" = "basisOfRecord",
                "States" = "australianStatesAndTerritories",
                "IBRA Regions" = "iBRA7Regions",
                "National Parks" = "national_parks"),
              selected = "states"),
            selectInput(
              inputId = "heatmap_y",
              label = "Y Axis:",
              choices = c(
                "Year" = "year_group",
                "Taxon" = "taxon",
                # "Threatened Status" = "threatened",
                "Basis of Record" = "basisOfRecord",
                "States" = "australianStatesAndTerritories",
                "IBRA Regions" = "iBRA7Regions",
                "National Parks" = "national_parks"),
              selected = "taxon"),
            selectInput(
              inputId = "heatmap_z",
              label = "Z axis:",
              choices = c(
                "Number of records" = "n_records",
                "Number of species" = "n_spp")),
            checkboxInput(
              inputId = "log_scale",
              label = "Log scale",
              value = FALSE),
            selectInput(
              inputId = "heatmap_facet",
              label = "Facets:",
              choices = c(
                "None" = "none",
                "Year" = "year_group",
                "Taxon" = "taxon",
                # "Threatened Status" = "threatened",
                "Basis of Record" = "basisOfRecord",
                "States" = "australianStatesAndTerritories",
                "IBRA Regions" = "iBRA7Regions",
                "National Parks" = "national_parks"),
              selected = "none"),
            add_color_options(),
            hr(),
            actionButton(
              inputId = "download_modal",
              label = "Download",
              width = "100%")
          ),
          mainPanel(
            plotOutput(outputId = "chart_space", width = "100%", height = "75vh")
          )
        )
      ),
      tabPanel("Map",
        br(),
        sidebarLayout(
          sidebarPanel(
            selectInput(
              inputId = "count_type",
              label = "Counts:",
              choices = c(
                "Number of records" = "n_records",
                "Number of species" = "n_spp")),
            checkboxInput(
              inputId = "log_scale",
              label = "Log scale",
              value = FALSE),
            selectInput(
              inputId = "map_spatial",
              label = "Regions:",
              choices = c(
                "States" = "states",
                "IBRA Regions" = "ibra"),
              selected = "states"),
            selectInput(
              inputId = "map_facet",
              label = "Facets:",
              choices = c(
                "None" = "none",
                "Year" = "year_group",
                "Taxon" = "taxon",
                # "Threatened Status" = "threatened",
                "Basis of Record" = "basisOfRecord",
                "National Parks" = "national_parks"),
              selected = "none"),
            add_color_options(),
            hr(),
            actionButton(
              inputId = "download_modal",
              label = "Download",
              width = "100%")
          ),
          mainPanel(
            plotOutput(outputId = "chart_space", width = "100%", height = "75vh")
          )
        )
      )
    )
  )
}


add_color_options <- function(){
  list(
    selectInput(
      inputId = "color_scheme",
      label = "Colour scheme:",
      choices = c(
        "viridis",
        "magma",
        "inferno",
        "plasma",
        "cividis"),
      width = "100%"),
    checkboxInput(
      inputId = "color_reverse",
      label = "Reverse colors",
      value = FALSE),
    actionButton(
      inputId = "redraw",
      label = "Redraw",
      width = "100%")
  )
}
