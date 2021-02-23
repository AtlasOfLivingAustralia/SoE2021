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
  # get examples for counts from a call to ALA
  # if(spatial == "ibra"){
  #   if(taxa == "all"){
  #     new_data <- ala_counts(group_by = "cl1048", type = type)
  #   }else{
  #     new_data <- ala_counts(taxa = select_taxa(taxa), group_by = "cl1048", type = type)
  #   }
  #   data_out <- merge(ibra_map, new_data,
  #     by.x = "REG_NAME_7",
  #     by.y = "name",
  #     all = TRUE)
  # }else{ # i.e. no spatial aspect
  #   if(taxa == "all"){
  #     data_out <- ala_counts(type = type)
  #   }else{ # i.e. specific taxon
  #     data_out <- ala_counts(taxa = select_taxa(taxa), type = type)
  #   }
  # }
  # Sys.sleep(1)
  # return(data_out)

  # new version; call from internal data:
    # lookup_df for indexing against supplied values
    # data_list for actual values in list form

  
}
# test <- get_soe_data(spatial = "ibra", taxa = "all")