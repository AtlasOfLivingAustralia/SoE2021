#' ALA data for SoE
#'
#' @title ALA data
#' @name lookup_df
#' @name data_list
#' @export build_ala_data


get_ala_data <- function(){

  # start with states, build data.frame with following columns
    # threatened status
    # taxonomic group (from ui list, with 'other' category added)
    # Australian State
    # Year group (i.e. 5-year intervals 1970-today)
    # Number of Records
    # Number of species
  # then use group functions to sum counts for un-required subcategories in plotting
  # repeat for IBRA regions


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
      ala_counts(filters = select_filters(cl22 = a$full_name))
      result <- ala_occurrences(
        # taxa = select_taxa(term = "Litoria peronii"), # testing
        filters = select_filters(cl22 = a$full_name),
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

# process data
file <- "./cache/ACT.rds"
crosstab_ala_data <- function(file){

  # import
  data_in <- readRDS(file)

  # set up which taxonomic groups to keep
  data_in$taxon <- NA
  chordates <- data_in$phylum == "Chordata"
  data_in$taxon[chordates] <- data_in$class[chordates]
  data_in$taxon[(data_in$kingdom == "Animalia" & !chordates)] <- "Invertebrates"
  data_in$taxon[which(data_in$kingdom == "Plantae")] <- "Plants"
  data_in$taxon[which(data_in$kingdom == "Fungi")] <- "Fungi"
  data_in$taxon[is.na(data_in$taxon)] <- "Other"

  # classify by threatened status

  # get national parks information
  data_in$national_parks <- factor(
    as.numeric(data_in$cAPAD2016Terrestrial != "") + 1,
    levels = c(1, 2),
    labels = c("Other Land Use", "National Park"))

  # determine which basis of record to keep (i.e. any 'other' group?)
  data_in$basisOfRecord[data_in$basisOfRecord == ""] <- "Not Recorded"

  # classify by year

  # crosstabulate by IBRA region

  # crosstabulate by state

  # rbind

  # export


  ## BUT SPECIES NUMBERS DON'T SUM
  # so we need to revert to our earlier attempt
  # i.e. create every unique cobination of of entries as a list
  # this isn't strinctly necessary for record counts, but could save time

  # fortunately, we can still get n_spp and n_rec at same time
  crosstab_columns <- c("taxon",
    "basisOfRecord",
    "australianStatesAndTerritories", "iBRA7Regions", "national_parks")
  combination_list <- do.call(c,
    lapply(
      seq_along(crosstab_columns),
      function(a){combn(crosstab_columns, a, simplify = FALSE)}))


  # below intended to be passed to lapply
  a <- combination_list[[9]] # for testing purposes
  formula_counts <- formula(paste0(" ~ ", paste(a, collapse = " + ")))
  crosstab_counts <- as.data.frame(xtabs(formula_counts, data = data_in))
  colnames(crosstab_counts)[ncol(crosstab_counts)] <- "n_records"

  # and species
  # frist crosstab gets record count by species + other vars
  formula_species <- formula(paste0(" ~ species_guid + ", paste(a, collapse = " + ")))
  crosstab_species <- as.data.frame(
    xtabs(formula_species,
      data = data_in[data_in$species_guid != "", ]),
    stringsAsFactors = FALSE)
  crosstab_species <- crosstab_species[
    crosstab_species$Freq > 0,
    colnames(crosstab_species) != "Freq"]
  # second cross-tab gets rows (=species) with more than one occurrence
  crosstab_spp <- as.data.frame(xtabs(
    formula(paste0("~ ", paste(a, collapse = " + "))),
    data = crosstab_species))
  colnames(crosstab_spp)[ncol(crosstab_spp)] <- "n_spp"

  # merge counts of records and species
  result <- merge(crosstab_counts, crosstab_spp) # merge on all shared columns
  return(result)
  # end lapply

}

# OLD species counting code
# crosstab_counts$n_spp <- unlist(lapply(
#   split(
#     crosstab_counts[, -ncol(crosstab_counts)],
#     seq_len(nrow(crosstab_counts))),
#   function(b){
#     # get one column of logicals per column of entry
#     logical_list <- lapply(seq_len(ncol(b)), function(x){
#       data_in[[colnames(b)[[x]]]] == as.character(b[, 1])
#     })
#     # check for matches across all columns
#     if(ncol(b) > 1){
#       lookup <- apply(logical_list, 1, all)
#     }else{
#       lookup <- logical_list[[1]]
#     }
#     # extract species, get count
#     result <- length(unique(data_in$species_guid[lookup]))
#     return(result)
#   }
# ))

# OLD CODE for downloading data from ALA
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
