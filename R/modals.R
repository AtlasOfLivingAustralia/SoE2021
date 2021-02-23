# NOTE: not currently implemented, as there are no live calls to ALA in the app
calculating_modal <- function(){
  showModal(
    modalDialog(
      HTML("This may take some time"),
      title = "Getting data from ALA",
      footer = NULL,
      easyClose = FALSE
    )
  )
}