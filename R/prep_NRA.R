#' Combines NRFSP data files with key files.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#'
#' @param path_dat String pointing to folder containing csv files, or path to single csv file_id.
#' @param path_key String pointing to folder containing csv files, or path to single csv file_id.
#' @param item_pilot List of numbers that correspond pilot item sequence (e.g., 70, 71, 72, 73, 74, 75).
#' @return A comprehensive tibble where every row is a candidate resp_cand, but which can be nested to conduct a variety of analysis.
#'
#'
#'

utils::globalVariables(".") #Silence 'no visible binding' warning

# Main function

#' @export
prep_NRFSP <- function(path_dat, path_key,
                       item_pilot = c(80, 81, 82, 83, 84, 85),
                       include_groups = F){


  #CANDIDATE DATA
  dat <-
    prep_NRFSP_dat(path_dat, item_pilot, include_groups)


  #MATCH KEYS
  keys_list <- check_keys_available(dat, path_key)
  dat <- match_keys_available(dat, keys_list)

  #KEY TABLE
  key_table <- prep_NRFSP_key_table(keys_list)


  #JOIN
  dat <-
    dat %>%
    tidyr::unnest(c(.data$resp_cand, .data$item_seq)) %>%
    dplyr::left_join(key_table,
                     by = c("key_id", "item_seq"))


  #FACTORIZE, GET CORRECT
  response_options <- dat$item_key %>% unique()

  dat <-
    dat %>%
    dplyr::mutate(dplyr::across(c(.data$file_id,
                                  .data$cand_id,
                                  .data$form_id,
                                  .data$key_id
                                  ), factor),
                  dplyr::across(c(.data$resp_cand,
                                  .data$item_key),
                                ~factor(.x, levels = response_options)),

                  resp_correct = ifelse(.data$item_key == .data$resp_cand, 1, 0)
                  #resp_correct = ifelse(is.na(.data$resp_correct), -999, .data$resp_correct)
                  )

  #TIDY
  if(include_groups == F){

    dat <-
      dat %>%
      dplyr::select(
        .data$file_id,
        .data$form_id,
        .data$key_id,
        .data$cand_id,
        .data$cand_pass,
        .data$cand_score,
        .data$item_n,
        .data$item_seq,
        .data$item_pilot,
        .data$item_id,
        .data$item_domain,
        .data$resp_cand,
        .data$item_key,
        .data$resp_correct)
  }else{
    dat <-
      dat %>%
      dplyr::select(
        #TIME
        .data$exam_year,
        .data$exam_month,

        #DATA SOURCES
        .data$file_id,
        .data$form_id,
        .data$key_id,

        #CANDIDATE AND ITEM GENERAL
        .data$cand_id,
        .data$cand_race,
        .data$cand_gender,
        .data$cand_birthyear,
        .data$cand_lang,
        .data$cand_country,
        .data$cand_state,
        .data$cand_company,
        .data$cand_pass,
        .data$cand_score,
        .data$item_n,

        #CANDIDATE AND ITEM SPECIFIC
        .data$item_seq,
        .data$item_pilot,
        .data$item_id,
        .data$item_domain,
        .data$resp_cand,
        .data$item_key,
        .data$resp_correct
        )

  }



  return(dat)
}

#' @export
read_NRFSP_csv <- function(csv_paths){
  readr::read_csv(csv_paths,
                  col_types = list(Candidate_ID = readr::col_double(),
                                   Form_ID = readr::col_factor(),
                                   Version_ID = readr::col_character(),
                                   Raw_Score = readr::col_double(),
                                   Pass_Fail = readr::col_factor(),
                                   Responses = readr::col_character()),
                  col_select = c(cand_id = .data$Candidate_ID,
                                 .data$Form_ID,
                                 .data$Version_ID,
                                 cand_score = .data$Raw_Score,
                                 cand_pass = .data$Pass_Fail,
                                 resp_cand = .data$Responses))
}


read_NRFSP_groups_csv <- function(csv_paths){
  readr::read_csv(csv_paths,
                  col_types = list(Candidate_ID = readr::col_double(),
                                   Form_ID = readr::col_factor(),
                                   Version_ID = readr::col_character(),
                                   Raw_Score = readr::col_double(),
                                   Pass_Fail = readr::col_factor(),
                                   Responses = readr::col_character(),
                                   State = readr::col_factor(),
                                   Race = readr::col_factor(),
                                   CountryID = readr::col_factor(),
                                   Gender = readr::col_factor(),
                                   Birth_Year = readr::col_factor(),
                                   Language = readr::col_factor(),
                                   Language_Desc = readr::col_factor(),
                                   Admin_Name = readr::col_factor(),
                                   Exam_Month = readr::col_factor(),
                                   Exam_Year = readr::col_factor(),
                                   Company_Name = readr::col_factor()),
                  col_select = c(cand_id = .data$Candidate_ID,
                                 .data$Form_ID,
                                 .data$Version_ID,
                                 cand_score = .data$Raw_Score,
                                 cand_pass = .data$Pass_Fail,
                                 resp_cand = .data$Responses,
                                 cand_race = .data$Race,
                                 cand_gender = .data$Gender,
                                 cand_birthyear = .data$Birth_Year,
                                 cand_lang = .data$Language_Desc,
                                 cand_country = .data$CountryID,
                                 cand_state = .data$State,
                                 cand_company = .data$Company_Name,
                                 exam_month = .data$Exam_Month,
                                 exam_year = .data$Exam_Year))
}

#' @export
prep_NRFSP_dat <- function(path_dat, item_pilot, include_groups = F){


  #START
  csv_paths <- prep_paths(path_dat, "csv")
  version_id_missing0 <- as.character(c(1:9))

  #INCLUDE GROUPING VARS?
  if(include_groups == F){

    dat <-
      purrr::map_dfr(.x = csv_paths,
                     .f = read_NRFSP_csv,
                     .id = "file_id")

  }else{

    dat <-
      purrr::map_dfr(.x = csv_paths,
                     .f = read_NRFSP_groups_csv,
                     .id = "file_id")

  }

  #TIDY
  dat <-
    dat %>%
    dplyr::mutate(resp_cand = base::strsplit(.data$resp_cand, split = ""),
                  resp_cand = purrr::map(.data$resp_cand, toupper),
                  Version_ID = ifelse(.data$Version_ID %in% version_id_missing0,
                                      base::paste0("0", .data$Version_ID),
                                      .data$Version_ID),
                  item_n = purrr::map_dbl(.data$resp_cand, length),
                  item_seq = purrr::map(.data$item_n, ~ 1:.x),
                  form_id = paste0(.data$Form_ID, "_", .data$Version_ID),
                  key_id = paste0(.data$Form_ID, .data$Version_ID, "_keys.csv"),
                  cand_pass = ifelse(.data$cand_pass == "Pass", T, F),
                  item_pilot = ifelse(.data$item_seq %in% item_pilot,
                                      T, F),
                  dplyr::across(c(.data$form_id, .data$key_id), factor))



  base::cat(base::paste0("Found and compiled the following: \n",
                         base::paste(names(csv_paths), collapse = "\n"),
                         "\n \n"))
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





