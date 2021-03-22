#' ALA data for SoE
#'
#' @title ALA data
#' @name lookup_df
#' @name data_list
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


# take data from inst/extdata and save as a cleaned verison in /data
# first for EPBC
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

# Then for Weeds of National Significance (WONS)
build_wons_df <- function(){
  wons_df <- read.csv(
    "./SoE2021/inst/extdata/Weeds_of_National_Significance_(WoNS)_as_at_Feb._2013.csv")[,
    c("guid", "Supplied.Name")]
  colnames(wons_df) <- c("species_guid", "wons_supplied_name")
  wons_df$wons_included <- TRUE
  wons_df <- wons_df[wons_df$species_guid != "", ]
  save(wons_df, file = "./SoE2021/data/wons_df.RData")
}


# Sub-functions to support corss-tabulation by various categories

# group data by year
add_year_group <- function(df){
  var_factor <- cut(df$year, breaks = seq(1980, 2020, 5))
  levels(var_factor) <- paste(
    seq(1980, 2015, 5) + 1,
    seq(1985, 2020, 5),
    sep = "-")
  return(var_factor)
}

# by taxonomic group (pretty arbitrary categories)
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

# EPBC status
add_threatened_status <- function(df){
  df$order <- seq_len(nrow(df))
  data_out <- merge(df,
    threatened_df[, c("taxon_concept_id",  "conservation_status")],
    by.x = "species_guid",
    by.y = "taxon_concept_id",
    all.x = TRUE,
    all.y = FALSE)
  data_out <- data_out[order(data_out$order), ] # NOTE: this hasn't been checked
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

# Invasive status
add_invasive_spp <- function(df){
  df$order <- seq_len(nrow(df))
  data_out <- merge(df,
    wons_df[, c("species_guid",  "wons_included")],
    by = "species_guid",
    all.x = TRUE,
    all.y = FALSE)
  data_out$wons_included[is.na(data_out$wons_included)] <- FALSE
  data_out <- data_out[order(data_out$order), ] # NOTE: this hasn't been checked
  return(data_out$wons_included)
}

# aggregator function to group all relevant information at once.
# Called by crosstab_ala_data.table()
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


# files <- paste0("./cache/",
#   c("ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC", "WA"),
#   ".rds")

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
