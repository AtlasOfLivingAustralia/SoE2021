#' Build a shiny app for visualising ALA data
#'
#' This function builds an app in the users' workspace
#'
#' @importFrom shiny runApp
#' @export build_soe_app

build_soe_app <- function(
  name = "test_app",
  launch = TRUE
){

  app_data <- list(
    ibra = "ibra"#ibra_map
    # other info goes here
  )

  if(dir.exists(name)) {
    unlink(name, recursive = TRUE)
  }
  dir.create(name)
  dir.create(paste0(name, "/data"))
  saveRDS(
    list(
      server = soe_server,
      ui = soe_ui(),
      data = app_data),
    file = paste0("./", name, "/data/app.rds"))

  # write script to load this app into app.R
  utils::write.table(
    "library(SoE2021)
    app <- readRDS('data/app.rds')
    shinyApp(server = app$server, ui = app$ui)
    ",
    paste0(name, "/app.R"),
    sep = "\n",
    quote = FALSE,
    row.names = FALSE,
    col.names = FALSE
  )

  if(launch){runApp(name)}

}