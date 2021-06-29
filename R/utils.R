library(portrpaths)
library(pipetree)
library(rlang)

#' Function that loads the cache from SPEED-EXTRACT pipeline
#' and the specified table
#' @param table a string for the table to ne loaded
#' @param config_dir directory that contains the data profiles
#' @param profile strin that defines the data profile to be used
#' @return a dataframe
load_table <- function(table,
                       config_dir = "D:/projects/SPEED-EXTRACT.pipeline/local.yaml",
                       profile = "3m_hpc"){

  options(pipetree.config=config_dir)
  pp <- pipetree::get_portrpath()

  pp$profile <- profile
  #### get the cache that we can use with drake ####
  cache <- pipetree::get_cache()
  table_sym <- rlang::sym(table)


  data_table <- pipetree::load_merged_partitions(!!table_sym,cache=cache)[[table_sym]] %>%
    dplyr::rename_all(toupper)

  return(data_table)

}
