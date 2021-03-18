#' Build UI for shiny app
#'
#' @importFrom shiny fluidPage titlePanel sidebarLayout sidebarPanel mainPanel
#' selectInput plotOutput downloadButton checkboxInput tabPanel verbatimTextOutput
#' @importFrom bslib bs_theme

soe_ui <- function(){
  fluidPage(
    app_style(),
    titlePanel("State of the Environment 2021"),
    theme = bs_theme(version = 4, bootswatch = "minty",
                     primary = ala_cols("flamingo"),
                     secondary = ala_cols("silver")),
    tabsetPanel(
      id = "tabs",
      type = "pills",
      tabPanel("Barchart", value = "bar",
        br(),
        sidebarLayout(
          sidebarPanel(
            # x
            category_dropdown(
              inputId = "x_bar",
              label = "X Axis:",
              selected = "Year"),
            # y
            count_dropdown(
              inputId = "y_bar",
              label = "Y Axis:"),
            checkboxInput(
              inputId = "log_bar",
              label = "Log scale",
              value = FALSE),
            # color
            category_dropdown(
              inputId = "colour_bar",
              label = "Colour:",
              selected = "Year",
              include_none = TRUE),
            add_color_options(suffix = "bar")
          ),
          mainPanel(
            # verbatimTextOutput("text_1"),
            plotOutput(outputId = "barplot", width = "100%", height = "75vh")
          )
        )
      ),
      tabPanel("Heatmap", value = "heatmap",
        br(),
        sidebarLayout(
          sidebarPanel(
            category_dropdown(
              inputId = "x_heatmap",
              label = "X Axis:",
              selected = "australianStatesAndTerritories"),
            category_dropdown(
              inputId = "y_heatmap",
              label = "Y Axis:",
              selected = "taxon"),
            count_dropdown(
              inputId = "z_heatmap",
              label = "Z Axis:"),
            checkboxInput(
              inputId = "log_heatmap",
              label = "Log scale",
              value = FALSE),
            category_dropdown(
              inputId = "facet_heatmap",
              label = "Facet:",
              selected = "None", include_none = TRUE),
            add_color_options(suffix = "heatmap")
          ),
          mainPanel(
            # verbatimTextOutput("text_2"),
            plotOutput(outputId = "heatmap", width = "100%", height = "75vh")
          )
        )
      ),
      tabPanel("Map", value = "map",
        br(),
        sidebarLayout(
          sidebarPanel(
            count_dropdown(
              inputId = "z_map",
              label = "Counts:"),
             selectInput(
               inputId = "map_spatial",
               label = "Regions:",
               choices = c(
                 "States" = "australianStatesAndTerritories",
                 "IBRA Regions" = "iBRA7Regions"),
               selected = "states"),
              map_dropdowns(
                suffix = "map"
              ),
            checkboxInput(
              inputId = "log_map",
              label = "Log scale",
              value = FALSE),
            category_dropdown(
              inputId = "facet_map",
              label = "Facet:",
              selected = "None", include_none = TRUE),
            add_color_options(suffix = "map")
          ),
          mainPanel(
            plotOutput(outputId = "map", width = "100%", height = "75vh")
          )
        )
      ),
      tabPanel("Interactive map", value = "i_map",
        br(),
        sidebarLayout(
         sidebarPanel(
           count_dropdown(
             inputId = "z_i_map",
             label = "Counts:"),
           selectInput(
             inputId = "i_map_spatial",
             label = "Regions:",
             choices = c(
               "IBRA Regions" = "iBRA7Regions"),
             selected = "iBRA7Regions"),
           map_dropdowns(
             suffix = "i_map"
           )
           #checkboxInput(
           #  inputId = "log_i_map",
           #  label = "Log scale",
          #   value = FALSE),
         ),
         mainPanel(
           leafletOutput(outputId = "i_map", width = "100%", height = "75vh")
         )
        )
      )
    )
  )
}


# preset some types of UI
category_dropdown <- function(inputId, label, selected, include_none = FALSE){
  choices_default <- c(
    "Year" = "year_group",
    "Taxon" = "taxon",
    "Threatened Status" = "threatened_status",
    "Basis of Record" = "basisOfRecord",
    "States" = "australianStatesAndTerritories",
    "IBRA Regions" = "iBRA7Regions",
    "National Parks" = "national_parks")
  if(include_none){
    choices <- c("None" = "none", choices_default)
  }else{
    choices <- choices_default
  }
  return(selectInput(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected))
}

count_dropdown <- function(inputId, label){
  selectInput(
    inputId = inputId,
    label = label,
    choices = c(
      "Number of records" = "n_records",
      "Number of species" = "n_species"))
}



map_dropdowns <- function(suffix) {
  list(
    taxon_dropdown(suffix),
    year_dropdown(suffix),
    basis_dropdown(suffix)
  )
}

taxon_dropdown <- function(suffix) {
  selectInput(
    inputId = paste0("taxon_", suffix),
    label = "Taxon",
    choices = c(
      "All",
      "Birds",
      "Reptiles",
      "Plants",
      "Invertebrates",
      "Amphibians",
      "Mammals",
      "Fungi",
      "Other Vertebrates",
      "Other"
    ),
    selected = "All"
  )
}

year_dropdown <- function(suffix) {
  selectInput(
    inputId = paste0("year_", suffix),
    label = "Years",
    choices = c(
      "All",
      "1981-1985",
      "1986-1990",
      "1991-1995",
      "1996-2000",
      "2001-2005",
      "2006-2010",
      "2011-2015",
      "2016-2020"
    )
  )
}

basis_dropdown <- function(suffix) {
  selectInput(
    inputId = paste0("basis_", suffix),
    label = "Basis of record",
    choices = c(
      "All" = "All",
      "Human observation" = "HumanObservation",
      "Preserved specimen" = "PreservedSpecimen",
      # "Not recorded" = "", # doesn't work for some reason
      "Environmental DNA" = "EnvironmentalDNA",
      "Machine observation" = "MachineObservation",
      "Material sample" = "MaterialSample",
      "Genomic DNA" = "GenomicDNA",
      "Image" = "Image",
      "LivingSpecimen" = "LivingSpecimen",
      "Sound" = "Sound",
      "FossilSpecimen" = "FossilSpecimen",
      "Video" = "Video"
    )
  )
}

app_style <- function() {
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      @import url(https://fonts.googleapis.com/css?family=Lato);
      @import url(https://fonts.googleapis.com/css?family=Roboto);

      h2 {
        font-family: Lato;
        font-weight: normal;
      }
      body { font-family: Roboto; }"))
  )
}

add_color_options <- function(suffix){
  list(
    selectInput(
      inputId = paste0("color_scheme_", suffix),
      label = "Colour scheme:",
      choices = c(
        "ala",
        "viridis",
        "magma",
        "inferno",
        "plasma",
        "cividis"),
      width = "100%"),
    checkboxInput(
      inputId = paste0("color_reverse_", suffix),
      label = "Reverse colors",
      value = FALSE),
    #actionButton(
    #  inputId = "download_modal",
    #  label = "Download",
    #  width = "100%")
    downloadButton(
      outputId = paste0("download_", suffix)
    )
  )
}





## TIME SERIES (I.E. LINKED DOT AND LINE PLOT) REMOVED FOR NOW
# tabPanel("Time Series", value = "time",
#   br(),
#   sidebarLayout(
#     sidebarPanel(
#       # y
#       selectInput(
#         inputId = "time_y",
#         label = "Y Axis:",
#         choices = c(
#           "Number of records" = "n_records",
#           "Number of species" = "n_spp")),
#       checkboxInput(
#         inputId = "log_scale",
#         label = "Log scale",
#         value = FALSE),
#       # size
#       selectInput(
#         inputId = "time_size",
#         label = "Size:",
#         choices = c(
#           "None" = NULL,
#           "Number of records" = "n_records",
#           "Number of species" = "n_spp")),
#       # color
#       selectInput(
#         inputId = "time_colour",
#         label = "Colour:",
#         choices = c(
#           "None" = NULL,
#           "Year" = "year_group",
#           "Taxon" = "taxon",
#           # "Threatened Status" = "threatened",
#           "Basis of Record" = "basisOfRecord",
#           "States" = "australianStatesAndTerritories",
#           "IBRA Regions" = "iBRA7Regions",
#           "National Parks" = "national_parks"),
#         selected = "None"),
#       # facet
#       selectInput(
#         inputId = "time_facet",
#         label = "Facet:",
#         choices = c(
#           "None" = NULL,
#           "Year" = "year_group",
#           "Taxon" = "taxon",
#           # "Threatened Status" = "threatened",
#           "Basis of Record" = "basisOfRecord",
#           "States" = "australianStatesAndTerritories",
#           "IBRA Regions" = "iBRA7Regions",
#           "National Parks" = "national_parks"),
#         selected = "None"),
#       add_color_options()
#     ),
#     mainPanel(
#       plotOutput(outputId = "time_plot", width = "100%", height = "75vh")
#     )
#   )
# ),
