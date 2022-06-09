#' Combines files from datraw folder into Rds file.
#' @importFrom magrittr %>%
#'
#' @param string Alternate path to the appropriate data folder.
#' @param string Alternative name than the one the function creates.
#'
#'
#' @return A tibble of all csvs in folder, saved as an Rds file.
#' @export
#'
#' @examples
#' x <- "my_csv_path"
#' get_csv_paths(x) %>% get_data()
#'

#' @export
get_csv_paths <- function(path){


  path_is_dir <- utils::file_test("-d", path)

  if(path_is_dir){
    csv_names <- list.files(path, pattern="*.csv$")
    csv_files_paths <- paste0(path, "/", csv_names)
    names(csv_files_paths) <- c(csv_names)

    return(csv_files_paths)

  }else {

    #Chop off file name from file path.
    csv_name <- stringr::str_extract(path, pattern = '[^/]*.csv$')
    names(path) <- csv_name
    return(path)
  }

}


read_NRFSP_csv <- function(path){
  readr::read_csv(path, col_types = list(Candidate_ID = readr::col_double(),
                                         Form_ID = readr::col_factor(),
                                         Version_ID = readr::col_double(),
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

#' @export
get_dat <- function(csv_paths){

  names_files <- names(csv_paths)

  dat <-
    purrr::map_dfr(.x = csv_paths,
                   .f = read_NRFSP_csv,
                   .id = "File")

  print(base::paste0("Found and compiled ", base::paste(names_files, collapse = ", ")))
  return(dat)


}
