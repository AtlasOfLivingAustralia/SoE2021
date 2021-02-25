#' ALA data for SoE
#'
#' @title ALA data
#' @name lookup_df
#' @name data_list
#' @export build_ala_data


build_ala_data <- function(){

  # start with states, build data.frame with following columns
    # threatened status
    # taxonomic group (from ui list, with 'other' category added)
    # Australian State
    # Year group (i.e. 5-year intervals 1970-today)
    # Number of Records
    # Number of species
  # then use group functions to sum counts for un-required subcategories in plotting
  # repeat for IBRA regiosn


}




# create a data.frame with every combination of variables
# lookup_df <- expand.grid(
#   taxon = c(
#     "all",
#     "mammalia",
#     "aves",
#     "reptilia",
#     "amphibia",
#     "actinopterygii",
#     "insecta",
#     "plantae"),
#   year_start = c(9999, seq(1971, 2016, by = 5)),
#   ibra = c(FALSE, TRUE),
#   type = c("record", "species"),
#   stringsAsFactors = FALSE)
# lookup_df$taxon_id <- c(
#   NA,
#   select_taxa(c(
#     "mammalia",
#     "aves",
#     "reptilia",
#     "amphibia",
#     "actinopterygii",
#     "insecta",
#     "plantae"))$taxon_concept_id)
# save(lookup_df, file = "lookup_df.RData")
#
# data_list <- lapply(
#   split(lookup_df, seq_len(nrow(lookup_df))),
#   function(a){
#     if(a$taxon == "all"){
#       taxon <- NULL
#     }else{
#       taxon <- a$taxon_id
#     }
#     if(a$year_start == 9999){
#       filter <- NULL
#     }else{
#       filter <- select_filters(
#         year = seq(a$year_start, a$year_start + 4, 1))
#     }
#     if(a$ibra){
#       result <- ala_counts(
#         taxa = taxon,
#         filters = filter,
#         group_by = "cl1048",
#         type = a$type)
#     }else{
#       result <- ala_counts(
#         taxa = taxon,
#         filters = filter,
#         type = a$type)
#     }
#   return(result)
# })
# save(data_list, file = "data_list.RData")
