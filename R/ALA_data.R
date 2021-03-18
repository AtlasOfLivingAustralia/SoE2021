#' ALA data for SoE
#'
#' @title ALA data
#' @name lookup_df
#' @name data_list
#' @export get_ala_data
#' @importFrom data.table fread rbindlist


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




# how to group data by year - function required below
add_year_group <- function(df){
  var_factor <- cut(df$year, breaks = seq(1980, 2020, 5))
  levels(var_factor) <- paste(
    seq(1980, 2015, 5) + 1,
    seq(1985, 2020, 5),
    sep = "-")
  return(var_factor)
}
# earlier approach based on string manipulation
# year_levels <- levels(var_factor)
# year_levels <- gsub(",", "-", year_levels)
# year_levels <- gsub("\\(|\\]", "", year_levels)
# levels(var_factor) <- year_levels
# overwrite instead

add_taxon <- function(df){
  var <- rep(1, nrow(df)) # Other life forms
  var[which(df$kingdom == "Fungi")] <- 2 # "Fungi"
  var[which(df$kingdom == "Plantae")] <- 3 # "Plants"
  chordates <- df$phylum == "Chordata"
  var[(df$kingdom == "Animalia" & !chordates)] <- 4 # "Invertebrates"
  var[chordates] <- 9 # "Other Vertebrates"
  var[which(df$class == "Aves")] <- 5 # "Birds"
  var[which(df$class == "Mammalia")] <- 6 # "Mammals"
  var[which(df$class == "Reptilia")] <- 7 # "Reptiles"
  var[which(df$class == "Amphibia")] <- 8 #"Amphibians"

  # convert to factor
  var_factor <- factor(var,
    levels = seq_len(9),
    labels = c(
      "Other life forms",
      "Fungi",
      "Plants",
      "Invertebrates",
      "Birds",
      "Mammals",
      "Reptiles",
      "Amphibians",
      "Other vertebrates"))
  return(var_factor)
}


#
build_epbc_df <- function(){
  # classify by threatened status
  threatened_list <- read.csv("./SoE2021/inst/extdata/EPBC_list.csv")
  threatened_ala <- select_taxa(threatened_list$scientific_name)
  # threatened_ala[threatened_ala$match_type == "higherMatch", ]
  threatened_ala <- threatened_ala[!is.na(threatened_ala$species), ]
  threatened_df <- merge(threatened_list,
    threatened_ala[, c("search_term", "taxon_concept_id")],
    by.x = "scientific_name", by.y = "search_term",
    all = FALSE)

  # length(unique(threatened_list$taxon_concept_id)) == nrow(threatened_list)
  threated_list <- split(threatened_df, threatened_df$taxon_concept_id)
  # threated_list[which(unlist(lapply(threated_list, nrow)) > 1)]
  threatened_df <- do.call(rbind, lapply(threated_list, function(a){a[1, ]}))
  save(threatened_df, file = "./SoE2021/data/threatened_df.RData")
}


add_threatened_status <- function(df){
  data_out <- merge(df,
    threatened_df[, c("taxon_concept_id",  "conservation_status")],
    by.x = "species_guid",
    by.y = "taxon_concept_id",
    all.x = TRUE,
    all.y = FALSE)
  # convert raw data to a numeric variable
  status_vector <- rep(1, nrow(data_out))
  status_vector[data_out$conservation_status == "Conservation dependent"] <- 2
  status_vector[data_out$conservation_status == "Vulnerable"] <- 3
    status_vector[data_out$conservation_status == "Endangered"] <- 4
  status_vector[data_out$conservation_status == "Critically Endangered"] <- 5
  status_vector[data_out$conservation_status == "Extinct in the wild"] <- 6
  status_vector[data_out$conservation_status == "Extinct"] <- 7
  # convert to factor
  status_factor <- factor(
    status_vector,
    levels = seq_len(7),
    labels = c(
      "Not threatened",
      "Conservation dependent",
      "Vulnerable",
      "Endangered",
      "Critically Endangered",
      "Extinct in the wild",
      "Extinct"))
  return(status_factor)
}



add_required_cols <- function(df, combns){
  if(any(combns == "year_group")){
    df$year_group <- add_year_group(df)
  }
  if(any(combns == "taxon")){
    df$taxon <- add_taxon(df)
  }
  if(any(combns == "threatened_status")){
    df$threatened_status <- add_threatened_status(df)
  }
  return(df)
}


crosstab_ala_data.table <- function(files){

  # get zip files
  cache_dir <- "./cache"
  cached_files <- list.files(cache_dir)
  cached_files <- cached_files[grepl(".zip$", cached_files)]
  cached_df <- data.frame(
    source = paste0(cache_dir, "/", cached_files),
    out = paste0(cache_dir, "/", gsub(".zip", "", cached_files)))
  # below section only needs to be run once, to unzip your files
  # invisible(lapply(
  #   split(cached_df, seq_len(nrow(cached_df))),
  #   function(a){unzip(a$source, exdir = a$out)}))

  # detect all files named some variant of 'data.csv'
  all_files <- unlist(lapply(cached_df$out, function(a){
    all_files <- list.files(a)
    return(
      paste0(a, "/", all_files[grepl("^data", all_files)]))
  }))

  # import
  data_list <- lapply(all_files, function(a){data.table::fread(file = a)})
  data_in <- data.table::rbindlist(data_list)
  colnames(data_in)[7:9] <- c(
    "australianStatesAndTerritories",
    "iBRA7Regions",
    "CAPAD2016Terrestrial")

  # saveRDS(data_in, "./cache/alldata.rds")
  # data_in <- readRDS("./cache/alldata.rds")

  # set up national park information
  national_park_logical <- data_in$CAPAD2016Terrestrial != ""
  national_park_numeric <- as.numeric(national_park_logical) + 1
  national_park_factor <- factor(
    national_park_numeric,
    levels = seq_len(2),
    labels = c("Other land use", "National Park"))
  # data_in <- cbind(data_in, data.frame(national_parks = national_park_factor))
  data_in$national_parks <- national_park_factor

  # work out required combinations of variables
  crosstab_columns <- c(
    "year_group",
    "taxon",
    "basisOfRecord",
    "threatened_status",
    "australianStatesAndTerritories",
    "iBRA7Regions",
    "national_parks")

  # get all combinations
  combination_list <- do.call(c,
    lapply(
      seq_len(4), # maximum number of combinations
      # seq_along(crosstab_columns),
      function(a){combn(crosstab_columns, a, simplify = FALSE)}))

  # we never need to combine states and IBRA regions - remove pairs with these attributes
  combination_list <- combination_list[
    !unlist(lapply(combination_list, function(a){
      all(
        c("australianStatesAndTerritories", "iBRA7Regions") %in% a)
    }))]


  # now create list of combinations that we can populate with data
  data_list <- as.list(rep(NA, length(combination_list)))
  exceptions_group <- c("year_group", "threatened_status", "taxon")

  for(a in seq_along(data_list)){ # c(8:length(data_list))){#
    combn_tr <- combination_list[[a]]
    if(any(combn_tr %in% exceptions_group)){
      included_vars <- combn_tr[!(combn_tr %in% exceptions_group)]
      if(any(combn_tr == "year_group")){
        included_vars <- c(included_vars, "year")
      }
      if(any(combn_tr == "threatened_status")){
        included_vars <- c(included_vars, "species_guid")
      }
      if(any(combn_tr == "taxon")){
        included_vars <- c(included_vars, "kingdom", "phylum", "class")
      }
    }else{
      included_vars <- combn_tr
    }

    # use included_vars to crosstab
    xtab1 <- add_required_cols(
      data_in[, .(.N), keyby = included_vars],
      combn_tr)
    xtab_final <- xtab1[, .(sum(N)), keyby = combn_tr]
    colnames(xtab_final)[length(combn_tr) + 1] <- "n_records"

    # get information by species
    if(any(included_vars == "species_guid")){
      xtab_species <- xtab1
    }else{
      xtab_species <- add_required_cols(
        data_in[, .(.N), keyby = c(included_vars, "species_guid")],
        combn_tr)
    }
    xtab_species_count <- xtab_species[
      (species_guid != ""),
      .(.N),
      keyby = combn_tr]
    # xtab_species_count_final <- xtab_species_count[, .(sum(N)), keyby = combn_tr]
    colnames(xtab_species_count)[length(combn_tr) + 1] <- "n_species"
    result <- merge(xtab_final, xtab_species_count)

    data_list[[a]] <- result
    cat(paste0("Run ", a, " complete: ", Sys.time(), "\n"))

  } # end loop

  names(data_list) <- unlist(lapply(
    combination_list,
    function(a){paste(a, collapse = "::")}))

  # last stage is to export xtab list and a corresponding index data.frame
  save(data_list, file = "./SoE2021/data/data_list.RData")

}


# working for original data.table code
# # import
# data_list <- lapply(all_files, function(a){data.table::fread(file = a)})
# data_in <- data.table::rbindlist(data_list)
#
# # do a cross-tabulation by year
# year_xtab <- data_in[, .(.N), keyby = .(year)] # works
# # print(year_xtab, topn = 200)
# # then cross-tab by factor
# # note this is needed because original df too large for factor generation
# year_xtab$year_group <- create_year_factor(year_xtab$year)
# year_group_xtab <- year_xtab[, .(sum(N)), keyby = year_group]
# colnames(year_group_xtab)[2] <- "n_records"
#
# # ditto species counts
# year_species <- data_in[, .(.N), by = .(year, species_guid)]
# # year_species_count <- year_species[N > 0, .(.N), keyby = .(year)]
# # print(year_species_count, topn = 100)
#
# # take a specific example
# # taxon_tr <- "urn:lsid:biodiversity.org.au:afd.taxon:ffdaa478-99ef-49e3-9b36-ca2ce0fa8f64"
# # rows <- which(year_species$species_guid == taxon_tr)
# # length(rows)
# # sum(year_species$N[rows])
# # year_species[rows, ]
#
# # any(is.na(year_species$species_guid)) # false - no missing values
# # any(year_species$N < 1) # false - no zeroes
# year_species$year_group <- create_year_factor(year_species$year)
# year_species_count <- year_species[
#   (species_guid != ""),
#   .(.N),
#   keyby = .(year_group, species_guid)]
# # note that this still includes species_guid == ""
# # then sum on this
#
# # rows2 <- which(year_species_count$species_guid == taxon_tr)
# # year_species_count[rows2,]
# # sum(year_species_count$N[rows2])
#
# # print(year_species_count, topn = 100)
# # any(year_species_count$N < 1)
# year_group_spp_xtab <- year_species_count[, .(sum(N)), keyby = year_group]
# year_group_xtab$n_species <- year_group_spp_xtab$V1
#
# # compare results
# # xtab_list$year_group # very different, but no obvious error in new code
