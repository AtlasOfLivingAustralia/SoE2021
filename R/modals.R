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