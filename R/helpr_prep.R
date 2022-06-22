



check_keys_available <- function(dat, keys_path){

  keys_path_named <- prep_paths(keys_path)

  #Match between keys needed and keys available
  keys_needed <-
    dat %>% dplyr::pull(.data$key_id) %>% base::unique()
  keys_available <- base::names(keys_path_named)
  keys_index <- keys_needed %in% keys_available
  missing_keys <- keys_needed[keys_index == F]
  matching_keys <- keys_needed[keys_index == T]
  matching_keys_paths <- base::paste0(keys_path,
                                      "/",
                                      matching_keys)
  names(matching_keys_paths) <- matching_keys

  #Return list of missing, matching, and paths for matching
  keys_list <- list(missing_keys,
                    matching_keys,
                    matching_keys_paths)
  names(keys_list) <- c("missing_keys",
                        "matching_keys",
                        "matching_keys_paths")

  return(keys_list)

}
match_keys_available <- function(dat, keys_list){

  #REPORT
  if(base::length(keys_list$missing_keys) > 0){
    warning(base::paste0("I could not find the following ",
                         base::length(keys_list$missing_keys), " keys: \n",
                         base::paste(keys_list$missing_keys, collapse = "\n"), "\n",
                         "These forms have been dropped from the dataset."))

    #FILTER data to drop forms without keys
    dat %>% dplyr::filter(.data$key_id %in% keys_list$matching_keys) %>% return()
  } else {
    base::cat("\n", "All forms had matching keys.")
    return(dat)
  }
}
prep_paths <- function(path, ending = "csv"){

  path <- stringr::str_replace_all(path, "[\\\\]", "/")

  #Is the path to a directory?
  path_is_dir <- utils::file_test("-d", path)

  #Name paths by file name
  if(path_is_dir){

    #Get all .csv files from directory, and name using .
    file_names <- base::list.files(path, pattern= paste0("*.", ending, "$"))
    files_paths <- base::paste0(path, "/", file_names)
    base::names(files_paths) <- c(file_names)

    return(files_paths)

  }else {

    #Chop off file name from file path to use as name for path.
    file_name <- stringr::str_extract(path,
                                      pattern = paste0("[^/]*.",
                                                       ending,
                                                       "$"))
    base::names(path) <- file_name
    return(path)
  }

}
