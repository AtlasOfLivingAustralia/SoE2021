# modals

# NOTE: not currently implemented, as there are no live calls to ALA in the app
# calculating_modal <- function(){
#   showModal(
#     modalDialog(
#       HTML("This may take some time"),
#       title = "Getting data from ALA",
#       footer = NULL,
#       easyClose = FALSE
#     )
#   )
# }

save_modal <- function(){

  showModal(
    modalDialog(
      textInput(
        inputId = "save_filename",
        label = "File Name"
        # value = paste(unlist(info), sep = "-")
      ),
      selectInput(
        inputId = "save_type",
        label = "File type",
        choices = c("PDF", "PNG", "JPEG", "TIFF"),
        multiple = FALSE
      ),
      actionButton(
        inputId = "save_data_execute",
        label = "Save"
      ),
      modalButton("Cancel"),
      footer = NULL,
      easyClose = TRUE
    )
  )

}
