#' Combines NRFSP data files with key files.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#'
#' @param path_dat String pointing to folder containing csv files, or path to single csv file.
#' @param path_key String pointing to folder containing csv files, or path to single csv file.
#' @param item_pilot List of numbers that correspond pilot item sequence (e.g., 70, 71, 72, 73, 74, 75).
#' @return A comprehensive tibble where every row is a candidate response, but which can be nested to conduct a variety of analysis.
#'
#'
#'

utils::globalVariables(".") #Silence 'no visible binding' warning

# Main function

#' @export
prep_NRFSP <- function(path_dat, path_key, item_pilot = c(80, 81, 82, 83, 84, 85)){


  #CANDIDATE DATA
  dat <-
    prep_NRFSP_dat(path_dat)


  #MATCH KEYS
  keys_list <- check_keys_available(dat, path_key)
  dat <- match_keys_available(dat, keys_list)

  #KEY TABLE
  key_table <- prep_NRFSP_key_table(keys_list)


  #JOIN
  dat <-
    dat %>%
    tidyr::unnest(.data$response) %>%
    dplyr::left_join(key_table,
                     by = c("key_id", "item_seq"))


  #FACTORIZE, GET CORRECT
  response_options <- dat$item_key %>% unique()
  dat <-
    dat %>%
    dplyr::mutate(dplyr::across(c(.data$file,
                                  .data$cand_id,
                                  .data$form_id,
                                  .data$key_id,
                                  ), factor),
                  dplyr::across(c(.data$response,
                                  .data$item_key),
                                ~factor(.x, levels = response_options)),
                  pass = ifelse(.data$pass == "Pass", T, F),
                  correct = ifelse(.data$item_key == .data$response, 1, 0),
                  item_pilot = ifelse(as.numeric(.data$item_seq) %in% item_pilot,
                                      T, F)) %>%
    dplyr::select(
           .data$file,
           .data$form_id,
           .data$key_id,
           .data$cand_id,
           .data$pass,
           .data$score,
           .data$item_seq,
           .data$item_pilot,
           .data$item_id,
           .data$item_domain,
           .data$response,
           .data$item_key,
           .data$correct)


  return(dat)
}

#' @export
read_NRFSP_csv <- function(csv_paths){
  readr::read_csv(csv_paths,
                  col_types = list(Candidate_ID = readr::col_double(),
                                   Form_ID = readr::col_factor(),
                                   Version_ID = readr::col_factor(),
                                   Raw_Score = readr::col_double(),
                                   Pass_Fail = readr::col_factor(),
                                   Responses = readr::col_character()),
                  col_select = c(cand_id = .data$Candidate_ID,
                                 .data$Form_ID,
                                 .data$Version_ID,
                                 score = .data$Raw_Score,
                                 pass = .data$Pass_Fail,
                                 response = .data$Responses))
}

#' @export
prep_NRFSP_dat <- function(path_dat){

  #HELPERS
  read_NRFSP_csv <- function(csv_paths){
    readr::read_csv(csv_paths,
                    col_types = list(Candidate_ID = readr::col_double(),
                                     Form_ID = readr::col_character(),  #This has to be character to properly combine into form_id and key_id
                                     Version_ID = readr::col_character(), #Same
                                     Raw_Score = readr::col_double(),
                                     Pass_Fail = readr::col_factor(),
                                     Responses = readr::col_character()),
                    col_select = c(cand_id = .data$Candidate_ID,
                                   .data$Form_ID,
                                   .data$Version_ID,
                                   score = .data$Raw_Score,
                                   pass = .data$Pass_Fail,
                                   response = .data$Responses))
  }

  #Function to change response list to column for join (may become generalized)
  resp_list_to_col <- function(.list){
    return(
      tibble::as_tibble_col(.list,
                            column_name = "response") %>%
        dplyr::mutate(item_seq = 1:nrow(.))
    )

  }

  #START
  csv_paths <- prep_paths(path_dat, "csv")


  #names_files <- base::names(csv_paths) #DELETE

  dat <-
    purrr::map_dfr(.x = csv_paths,
                   .f = read_NRFSP_csv,
                   .id = "file") %>%
    dplyr::mutate(response = base::strsplit(.data$response, split = ""),
                  response = purrr::map(.data$response, resp_list_to_col),
                  form_id = paste0(.data$Form_ID, "_", .data$Version_ID),
                  key_id = paste0(.data$Form_ID, .data$Version_ID, "_keys.csv"),
                  across(c(form_id, key_id), factor))

  base::cat(base::paste0("Found and compiled the following: \n",
                         base::paste(names(csv_paths), collapse = "\n")))
  return(dat)


}

#' @export
prep_NRFSP_key_table <- function(keys_list){

  key_table <-
    purrr::map_dfr(.x = keys_list$matching_keys_paths,
                   .f = ~readr::read_csv(.x,
                                         col_types = list(itemID = readr::col_factor(),
                                                          key = readr::col_factor(),
                                                          domain = readr::col_factor()),
                                         show_col_types = F),
                   .id = "key_id") %>%
    dplyr::rename(item_seq = .data$sequence,
                  item_id = .data$itemID,
                  item_key = .data$key,
                  item_domain = .data$domain)

  return(key_table)
}





