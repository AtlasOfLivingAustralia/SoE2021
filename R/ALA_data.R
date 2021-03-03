#' ALA data for SoE
#'
#' @title ALA data
#' @name lookup_df
#' @name data_list
#' @importFrom parallel mcapply detectCores
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
files <- paste0("./cache/",
  c("ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC", "WA"),
  ".rds")

crosstab_ala_data <- function(files){

  # import
  data_in <- do.call(rbind, lapply(files, function(a){readRDS(a)}))

  # set up which taxonomic groups to keep
  data_in$taxon <- NA
  chordates <- data_in$phylum == "Chordata"
  data_in$taxon[chordates] <- data_in$class[chordates]
  data_in$taxon[(data_in$kingdom == "Animalia" & !chordates)] <- "Invertebrates"
  data_in$taxon[which(data_in$kingdom == "Plantae")] <- "Plants"
  data_in$taxon[which(data_in$kingdom == "Fungi")] <- "Fungi"
  data_in$taxon[is.na(data_in$taxon)] <- "Other"

  # classify by year
  year_start <- seq(1971, 2016, 5)
  data_in$year_group <- NA
  # lapply(
  for(a in seq_along(year_start)){
    # function(a){
      data_in$year_group[
        which(
          data_in$year >= year_start[a] & data_in$year < (year_start[a] + 5)
        )] <- a
    }# )
  # xtabs(~data_in$year_group)
  data_in$year_group <- factor(
    data_in$year_group,
    levels = seq_along(year_start),
    labels = paste(year_start, year_start + 4, sep = "-"))
  # test this
  # xtabs(~ year + year_group, data = data_in)

  # get national parks information
  data_in$national_parks <- factor(
    as.numeric(data_in$cAPAD2016Terrestrial != "") + 1,
    levels = c(1, 2),
    labels = c("Other Land Use", "National Park"))

  # determine which basis of record to keep (i.e. any 'other' group?)
  data_in$basisOfRecord[data_in$basisOfRecord == ""] <- "Not Recorded"

  # classify by threatened status
  # NOT ACHIEVED YET


  ## BUT SPECIES NUMBERS DON'T SUM
  # so we need to revert to our earlier attempt
  # i.e. create every unique cobination of of entries as a list
  # this isn't strictly necessary for record counts, but could save time

  # fortunately, we can still get n_spp and n_rec at same time
  crosstab_columns <- c(
    "year_group",
    "taxon",
    "basisOfRecord",
    # "threatened_status",
    "australianStatesAndTerritories",
    "iBRA7Regions",
    "national_parks")

  # save progress
  # saveRDS(
    # data_in[, c("species_guid", crosstab_columns)],
    # "./cache/alldata.rds")
  # NOTE: This takes ages due to large file size

  combination_list <- do.call(c,
    lapply(
      seq_len(4), # maximum number of combinations
      # seq_along(crosstab_columns),
      function(a){combn(crosstab_columns, a, simplify = FALSE)}))
  # the above is in list format, which is useful for data extraction
  # BUT we also need a data.frame version to act as a lookup table inside the app

  # now we can use xtabs to get all the information at once
  # below intended to be passed to lapply
  # a <- combination_list[[9]] # for testing purposes
  xtab_list <- lapply(combination_list[1:3], function(a){

    # determine all levels of factors in the variables included this time
    unique_list <- lapply(data_in[, a], unique)

    # convert into a data.frame showing every unique combination
    result_df <- as.data.frame(
      lapply(expand.grid(unique_list), function(a){as.character(a)}))

    # for every combination of variable levels, calculate the number of
    # records and species
    result_list <- mclapply(
      split(result_df, seq_len(nrow(result_df))),
      function(b){
        logical_tr <- eval(str2expression(
            paste(
              paste0(
                "data_in$",
                paste(colnames(b),
                  paste0("'", b[1, ], "'"),
                  sep = " == ")),
              collapse = " & ")
        ))
        # return two numbers
        # n_records <- length(which(logical_tr))
        # if(n_records > 0){}else{}
        return(c(
          n_records = length(which(logical_tr)),
          n_spp = length(which(unique(data_in$species_guid[logical_tr]) != ""))
        ))
      },
      mc.cores = detectCores() - 1)

    result_df <- cbind(result_df, do.call(rbind, result_list))

    return(result_df)

  }) # end lapply



}

# OLD crosstab code
# xtab_list <- lapply(combination_list, function(a){

  # crosstab for counts
  # formula_counts <- formula(paste0(" ~ ", paste(a, collapse = " + ")))
  # crosstab_counts <- as.data.frame(xtabs(formula_counts, data = data_in))
  # colnames(crosstab_counts)[ncol(crosstab_counts)] <- "n_records"

  # and then species
  # first crosstab gets record count by species + other vars
    # formula_species <- formula(paste0(" ~ species_guid + ", paste(a, collapse = " + ")))
  # crosstab_species <- as.data.frame(xtabs(
  #   # formula_species,
  #   formula(paste0(" ~ species_guid + ", paste(a, collapse = " + "))),
  #   data = data_in[data_in$species_guid != "", ]),
  #   stringsAsFactors = FALSE)
  # crosstab_species <- crosstab_species[
  #   crosstab_species$Freq > 0,
  #   colnames(crosstab_species) != "Freq"]
  # # second cross-tab gets rows (= species) with more >1 occurrence
  # crosstab_spp <- as.data.frame(xtabs(
  #   formula(paste0("~ ", paste(a, collapse = " + "))),
  #   data = crosstab_species),
  #   stringsAsFactors = FALSE)
  # colnames(crosstab_spp)[ncol(crosstab_spp)] <- "n_spp"
  # the above works for small datasets, but exhausts memory for large

  # merge counts of records and species
  # result <- merge(crosstab_counts, crosstab_spp) # merge on all shared columns
#
#   return(result_df)
#
# }) # end lapply




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
