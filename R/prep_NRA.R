#' Combines NRFSP data files with
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#'
#' @param string path_dat Path to folder containing csv files, or path to single csv file.
#' @param string path_key Path to folder containing csv files, or path to single csv file.
#'
#' @return A comprehensive tibble where every row is a candidate response, but which can be nested to conduct a variety of analysis.
#'
#'
#'



# Main function

#' @export
prep_NRFSP <- function(path_dat, path_key){


  #CANDIDATE DATA
  dat <-
    prep_NRFSP_dat(path_dat)


  #CHECK KEYS
  keys_list <- check_keys_available(dat, path_key)

  if(base::length(keys_list$missing_keys) > 0){
    warning(base::paste0("I could not find the following ",
                         base::length(keys_list$missing_keys), " keys: \n",
                         base::paste(keys_list$missing_keys, collapse = "\n"), "\n",
                         "These forms have been dropped from the dataset."))

    #Filter data to drop forms without keys
    dat <-
      dat %>% dplyr::filter(.data$key_id %in% keys_list$matching_keys)
  } else {
    base::cat("\n", "All forms had matching keys.")
  }

  #KEY TABLE
  key_table <- prep_NRFSP_key_table(keys_list)


  #JOIN and FACTORIZE
  dat <-
    dat %>%
    tidyr::unnest(.data$responses) %>%
    dplyr::left_join(key_table,
                     by = c("key_id", "item_seq")) %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(), factor)) %>% #Maybe not everything
    dplyr::ungroup()

  return(dat)
}




#Helper function
#' @export
read_NRFSP_csv <- function(path){
  readr::read_csv(path,
                  col_types = list(Candidate_ID = readr::col_double(),
                                   Form_ID = readr::col_factor(),
                                   Version_ID = readr::col_factor(),
                                   Raw_Score = readr::col_double(),
                                   Pass_Fail = readr::col_factor(),
                                   Responses = readr::col_character()),
                  col_select = c(Candidate_ID,
                                 Form_ID,
                                 Version_ID,
                                 Raw_Score,
                                 Pass_Fail,
                                 Responses))
}


#Helper function
#' @export
prep_NRFSP_dat <- function(path_dat){

  #HELPERS
  read_NRFSP_csv <- function(csv_paths){
    readr::read_csv(csv_paths,
                    col_types = list(Candidate_ID = readr::col_double(),
                                     Form_ID = readr::col_factor(),
                                     Version_ID = readr::col_factor(),
                                     Raw_Score = readr::col_double(),
                                     Pass_Fail = readr::col_factor(),
                                     Responses = readr::col_character()),
                    col_select = c(Candidate_ID,
                                   Form_ID,
                                   Version_ID,
                                   Raw_Score,
                                   Pass_Fail,
                                   Responses))
  }

  #Function to change response list to column for join (may become generalized)
  resp_list_to_col <- function(.list){
    return(
      tibble::as_tibble_col(.list,
                            column_name = "responses") %>%
        dplyr::mutate(item_seq = 1:nrow(.))
    )

  }

  #START
  csv_paths <- prep_paths(path_dat, "csv")

  dat <-
    purrr::map_dfr(.x = csv_paths,
                   .f = read_NRFSP_csv,
                   .id = "file") %>%
    dplyr::mutate(Responses = base::strsplit(.data$Responses, split = ""),
                  Responses = purrr::map(.data$Responses, resp_list_to_col),
                  form_id = paste0(.data$Form_ID, "_", .data$Version_ID),
                  KeyID = paste0(.data$Form_ID, .data$Version_ID, "_keys.csv"))

  base::cat(base::paste0("Found and compiled the following: \n",
                         base::paste(names(csv_paths), collapse = "\n")))
  return(dat)


}

#' @export
join_keys_to_dat <- function(dat, keys_path){


  #Match between keys needed and keys available
  keys_needed <-
    dat %>% dplyr::pull(.data$KeyID) %>% base::unique()

  keys_available <-
    base::list.files(keys_path)

  keys_index <- keys_needed %in% keys_available #maybe this

  missing_keys <- keys_needed[keys_index == F]
  matching_keys <- keys_needed[keys_index == T]


  #Filter data to drop forms without keys
  dat <-
    dat %>% dplyr::filter(.data$KeyID %in% keys_available)


  #Print results to console
  if(length(missing_keys) > 0){
    warning(base::paste0("I could not find the following ",
            base::length(missing_keys), " keys: \n",
            base::paste(missing_keys, collapse = "\n"), "\n",
              "These forms have been dropped from the dataset."))
  } else {
    base::cat("All forms had matching keys.")
  }


  #Prep key_table
  path_to_keys <- base::paste0(keys_path, "/", matching_keys)
  base::names(path_to_keys) <- matching_keys
  key_table <-
    purrr::map_dfr(.x = path_to_keys,
            .f = ~readr::read_csv(.x, col_types = list(itemID = readr::col_factor())),
            .id = "key_id")


  #Function to prep Responses column for join
  prep_for_join <- function(.list){
    return(
      tibble::as_tibble_col(.list,
                    column_name = "Responses") %>%
        dplyr::mutate(sequence = 1:nrow(.))
    )

  }



  dat <-
    dat %>%
    dplyr::mutate(Responses = purrr::map(.data$Responses, prep_for_join)) %>%

    #Join tables
    tidyr::unnest(Responses) %>%
    dplyr::right_join(key_table, by = c("KeyID", "sequence"))


  #Rename column headers
  dat <-
  dat %>% dplyr::rename(file = "File",
                 cand_id = "Candidate_ID",
                 form_id = "Form_ID",
                 version_id = "Version_ID",
                 rawscore = "Raw_Score",
                 passfail = "Pass_Fail",
                 responses = "Responses",
                 item_id = "itemID",
                 item_seq = "sequence")

  return(dat)

}



