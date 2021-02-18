#' Function to get from ALA data in a standard format
#'
#' Primarily internal function for getting data in correct format
#'
#' @importFrom galah ala_counts select_taxa select_filters
#' @export get_soe_data

get_soe_data <- function(
  type, # either n_records or n_species
  taxa, # either 'all' or a taxonomic name to pass to select_taxa
  spatial, # either 'all' (no breakdown) or 'ibra'
  temporal # either 'all' (no breakdown) or 'increments' (divide into 5-year blocks)
){
  # get examples for counts
  if(spatial == "ibra"){
    if(taxa == "all"){
      new_data <- ala_counts(group_by = "cl1048")
    }else{
      new_data <- ala_counts(taxa = select_taxa(taxa), group_by = "cl1048")
    }
    data_out <- merge(ibra_map, new_data,
      by.x = "REG_NAME_7",
      by.y = "name",
      all = TRUE)
  }else{ # i.e. no spatial aspect
    if(taxa == "all"){
      data_out <- ala_counts()
    }else{ # i.e. specific taxon
      data_out <- ala_counts(taxa = select_taxa(taxa))
    }
  }
  return(data_out)
}
# test <- get_soe_data(spatial = "ibra", taxa = "all")