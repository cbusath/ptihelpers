
#' Combines FACE response files (sdf) with key (csv)
#'
#' @param string path_dat Path to folder containing csv files, or path to single csv file.
#'
#' @param string path_key Path to folder containing csv files, or path to single csv file.
#' @param string list of the names of the forms (e.g., c("Form199", "Form199", "Form198")) that correspond to each file.
#'
#' @return A comprehensive tibble where every row is a candidate response, but which can be nested to conduct a variety of analysis.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'



#Overall
#' @export
prep_FACE <- function(path_dat, path_key, list_form_id,
                      possible_response_options = c("A", "B", "C", "D", "ABCD"),
                      passing_score = 60){

  #CANDIDATE DATA
  dat <-
    prep_FACE_dat(path_dat, list_form_id)

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
  key_table <- prep_FACE_key_table(keys_list)


  #JOIN and FACTORIZE
  dat <-
    dat %>%
    tidyr::unnest(.data$responses) %>%
    dplyr::left_join(key_table,
                     by = c("key_id", "item_seq")) %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(), factor)) %>% #tidyselect
    dplyr::ungroup()

  #GET SCORE AND PASSING
  dat <-
    dat %>%
    dplyr::mutate(dplyr::across(c(.data$responses,
                                  .data$item_key),
                                ~factor(.x, levels = possible_response_options)),
                  correct = dplyr::case_when(.data$responses == .data$item_key ~ 1,
                                             .data$item_key == "ABCD" ~ 1,
                                             .data$responses != .data$item_key ~ 0
                  )) %>%
    tidyr::nest(item_dat = c(.data$item_seq,
                             .data$item_id,
                             .data$responses,
                             .data$item_key,
                             .data$correct)) %>%
    dplyr::mutate(score = purrr::map_dbl(.data$item_dat,
                                         function(df) base::sum(df$correct,
                                                                na.rm = T)),
                  pass = ifelse(.data$score >= passing_score, T, F)) %>%
    tidyr::unnest(.data$item_dat) %>%

    #CLEAN UP COLS
    dplyr::select(.data$file, .data$form_id, .data$key_id,
                  .data$cand_id, .data$pass, .data$score,
                  .data$item_seq, .data$item_id, response = "responses", .data$item_key, .data$correct)

  return(dat)
}


prep_FACE_dat <- function(sdf_paths, list_form_id){



  #HELPERS
  read_FACE_sdf <- function(path){

      readr::read_csv(path, col_names = F, show_col_types = F) %>%
      tidyr::separate(col = .data$X1,
               into = c("cand_id", "responses", "cand_firstname"),
               sep = c(12, -4)) %>%   #Break off the first 12, and the last 4.
      dplyr::mutate(cand_id = base::paste0(.data$cand_id,
                                           "_",
                                           .data$cand_firstname)) %>%
      dplyr::select(-.data$cand_firstname)

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
  sdf_paths <- prep_paths(sdf_paths, "sdf")

  #form_id must be a list the same that matches the order and number of files
  n_form_id = base::length(list_form_id)
  names_files <- base::names(sdf_paths)

  if(n_form_id != base::length(names_files)){

    warning("The list_form_id did not have the same number of names as
            the number of files in sdf_paths")

    return(base::cat(base::paste0("Number of sdf data files: ", base::length(names_files), "\n",
                      "Number of form_id names: ", n_form_id)))
  }

  dat <-
    purrr::map_dfr(.x = sdf_paths,
                   .f = read_FACE_sdf, #helper function defined above
                   .id = "file") %>%
    dplyr::mutate(responses = base::strsplit(.data$responses, split = ""),
                  responses = purrr::map(.data$responses, resp_list_to_col)) %>%
    dplyr::nest_by(file)

  dat_form_id <- tibble::as_tibble_col(list_form_id, "form_id")

  dat <- dplyr::bind_cols(dat, dat_form_id) %>%
    dplyr::mutate(key_id = paste0(.data$form_id, "_key.csv")) %>%
    tidyr::unnest(.data$data)

  base::cat(base::paste0("Found and compiled the following: \n",
                         base::paste(names_files, collapse = "\n")))

  return(dat)


}
prep_FACE_key_table <- function(keys_list){

  key_table <-
    purrr::map_dfr(.x = keys_list$matching_keys_paths,
                   .f = ~readr::read_csv(.x, col_names = F, show_col_types = F),
                   .id = "key_id") %>%
    dplyr::rename(item_seq = .data$X1, item_id = .data$X2, item_key = .data$X3)

  return(key_table)

}


