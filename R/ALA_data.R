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

  # determine which columns you want to cross-tabulate
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
  # data_in <- readRDS("./cache/alldata.rds")

  combination_list <- do.call(c,
    lapply(
      seq_len(4), # maximum number of combinations
      # seq_along(crosstab_columns),
      function(a){combn(crosstab_columns, a, simplify = FALSE)}))
  # the above is in list format, which is useful for data extraction
  # BUT we also need a data.frame version to act as a lookup table inside the app

  # get a list of unique values of each entry
  unique_list <- lapply(data_in[, -1], function(x){
    out <- unique(x)
    return(out[!is.na(out)])
  })

  # use lapply to get crosstabs for all combinations of data that we are interested in
  xtab_list <- lapply(combination_list, function(a){
  # xtab_list <- as.list(rep(NA, length(combination_list)))
  # for(i in seq_along(xtab_list)){

    print(paste0("starting run ", paste(a, collapse = " & ")))
    print(Sys.time())

    # a <- combination_list[[i]]
    # determine all levels of factors in the variables included this time
    # unique_list <- lapply(data_in[, a], unique)
    unique_tr <- unique_list[a]

    # convert into a data.frame showing every unique combination
    result_df <- as.data.frame(
      lapply(expand.grid(unique_tr), function(a){as.character(a)}))

    # for every combination of variable levels, calculate the number of
    # records and species
    result_list <- lapply(
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
        return(c(
          n_records = length(which(logical_tr)),
          n_spp = length(which(unique(data_in$species_guid[logical_tr]) != ""))
        ))
      })

    result_df <- cbind(result_df, do.call(rbind, result_list))

    xtab_list[[i]] <- result_df
    # return(result_df)

  })#, mc.cores = 7) # end lapply

  # this fails

  # last stage is to export xtab list and a corresponding index data.frame

}
