download_data <- function(data) {
  downloadHandler(
    filename = function() {
      paste("soe_data", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data, file, row.names = FALSE)
    }
  )
}

download_plot <- function(type) {
  if (type == "i_map") {
    downloadHandler(
      filename = paste0(type, "_plot.png"),
      content = function(file) {
        mapshot(df$plot_i_map, file = "i_map_plot.png")
      }
    )
  } else {
    downloadHandler(
      filename = paste0(type, "_plot.png"),
      content = function(file) {
        ggsave(file, width = 20, height = 15)
      }
    )
  }
}
