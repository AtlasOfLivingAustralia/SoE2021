#' ALA data for SoE
#'
#' @title ALA data
#' @export get_ala_data
#' @importFrom data.table fread rbindlist


# function to get the raw data
# NOTE: this takes a VERY long time to run
get_ala_data <- function(){

  # set config (delete before sharing)
  ala_config(
    email = "martinjwestgate@gmail.com",
    cache_directory = "./cache",
    caching = TRUE,
    download_reason_id = 10,
    verbose = TRUE)

  states <- sort(find_field_values("cl22")$category[1:8])
  state_df <- data.frame(
    acronym = c("ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC", "WA"),
    full_name = states)

  lapply(
    split(state_df, seq_len(nrow(state_df)))[2:8],
    function(a){
      # ala_counts(filters = select_filters(cl22 = a$full_name))
      result <- ala_occurrences(
        # taxa = select_taxa(term = "Litoria peronii"), # testing
        filters = select_filters(
          year = c(1980:2021),
          cl22 = a$full_name),
        columns = select_columns(
          "species_guid", # species ID
          "year",
          "kingdom",
          "phylum",
          "class",
          "basisOfRecord",
          "cl22", # states
          "cl1048", # IBRA
          "cl10944" # CAPAD (national park names)
        )
      )
      saveRDS(result,
        paste0("./cache/", a$acronym, ".rds"))
    }
  )

}
# note: sometimes caching loads the wrong file.
# e.g. when I changed select_filters to call a$full_name rather than
# "Australian Capital Territory" it loaded data, even though a$full_name == NSW
