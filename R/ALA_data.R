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

# process data
files <- paste0("./cache/",
  c("ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC", "WA"),
  ".rds")

crosstab_ala_data <- function(files){

  # import
  data_in <- do.call(rbind, lapply(files, function(a){readRDS(a)}))
  # subset to rows with usable data
  # data_in <- data_in[!is.na(data_in$year), ]
  # data_in <- data_in[data_in$year > 1980, ] # can go older if needed
  # data_in <- data_in[data_in$species_guid != "", ]

  # classify by year
  year_start <- seq(1981, 2016, 5)
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

  # set up which taxonomic groups to keep
  data_in$taxon <- NA
  chordates <- data_in$phylum == "Chordata"
  data_in$taxon[chordates] <- "Other Vertebrates"
  data_in$taxon[(data_in$kingdom == "Animalia" & !chordates)] <- "Invertebrates"
  data_in$taxon[which(data_in$class == "Amphibia")] <- "Amphibians"
  data_in$taxon[which(data_in$class == "Reptilia")] <- "Reptiles"
  data_in$taxon[which(data_in$class == "Mammalia")] <- "Mammals"
  data_in$taxon[which(data_in$class == "Aves")] <- "Birds"
  data_in$taxon[which(data_in$kingdom == "Plantae")] <- "Plants"
  data_in$taxon[which(data_in$kingdom == "Fungi")] <- "Fungi"
  data_in$taxon[is.na(data_in$taxon)] <- "Other"

  # get national parks information
  data_in$national_parks <- factor(
    as.numeric(data_in$cAPAD2016Terrestrial != "") + 1,
    levels = c(1, 2),
    labels = c("Other Land Use", "National Park"))

  # determine which basis of record to keep (i.e. any 'other' group?)
  # data_in$basisOfRecord[data_in$basisOfRecord == ""] <- "Not Recorded"
  data_in$basisOfRecord[
    !(data_in$basisOfRecord %in% c("HumanObservation", "PreservedSpecimen"))] <- "Other"
  # xtabs(~data_in$basisOfRecord)

  # classify by threatened status
  # NOT ACHIEVED YET

  # determine which columns you want to cross-tabulate
  crosstab_columns <- c(
    "year_group",
    "taxon",
    "basisOfRecord",
    # "threatened_status",
    "australianStatesAndTerritories",
    "iBRA7Regions", # takes a long time to run
    "national_parks")

  # save progress
  # saveRDS(
  #   data_in[, c("species_guid", crosstab_columns)],
  #   "./cache/alldata.rds")
  # NOTE: This takes ages due to large file size
  # data_in <- readRDS("./cache/alldata.rds")

  combination_list <- do.call(c,
    lapply(
      seq_len(3), # maximum number of combinations
      # seq_along(crosstab_columns),
      function(a){combn(crosstab_columns, a, simplify = FALSE)}))

  # we never need to combine states and IBRA regions - remove pairs with these attributes
  combination_list <- combination_list[
    !unlist(lapply(combination_list, function(a){
      all(
        c("australianStatesAndTerritories", "iBRA7Regions") %in% a)
    }))]

  # get a list of unique values of each entry
  unique_list <- lapply(data_in[, crosstab_columns], function(x){
    out <- unique(x)
    out <- out[out != ""]
    out <- out[!is.na(out)]
    return(out)
  })

  # convert into a data.frame showing every unique combination
  factor_list <- lapply(combination_list, function(a){
    unique_tr <- unique_list[a]
    result_df <- as.data.frame(
      lapply(expand.grid(unique_tr), function(a){as.character(a)}))
    return(result_df)
  })
  # sum(unlist(lapply(factor_list, nrow))) # check how many calculations are needed

  # use lapply to get crosstabs for all combinations of data that we are interested in
  xtab_list <- lapply(factor_list, function(a){

    # track progress
    print(paste0("starting run ", paste(colnames(a), collapse = " & ")))
    print(Sys.time())

    # for every combination of variable levels, calculate the number of
    # records and species
    result_list <- lapply(
      split(a, seq_len(nrow(a))),
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
        return(c(
          n_records = length(which(logical_tr)),
          n_spp = length(which(unique(data_in$species_guid[logical_tr]) != ""))
        ))
      })

    result_df <- cbind(a, do.call(rbind, result_list))
    return(result_df)

  })#, mc.cores = 7) # end lapply

  names(xtab_list) <- unlist(lapply(
    combination_list,
    function(a){paste(a, collapse = "::")}))

  # last stage is to export xtab list and a corresponding index data.frame
  save(xtab_list, file = "./SoE2021/data/xtab_data.RData")

}
