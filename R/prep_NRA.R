#' Combines files from datraw folder into Rds file.
#'
#' @param string Alternate path to the appropriate data folder.
#' @param string Alternative name than the one the function creates.
#'
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' prep_NRA() #Looks in the datraw folder, and compiles all files into one Rds file.
#'



prep_NRA <- function(diff_path = NULL, diff_name = NULL){



  data_dir_path <- here::here("Data", "2022")
  data_files <- list.files(data_dir_path)
  data_files_paths <- paste0(data_dir_path, "/", data_files)
  names(data_files_paths) <- c("02.22", "03.22") #Generalize this!


}



dat <-
  map_dfr(.x = data_files_paths,
          .f = read_csv,
          .id = "Month") %>%
  select(Month, Candidate_ID,
         Form_ID, Version_ID, Raw_Score,
         Pass_Fail, Responses
  ) %>%
  mutate(Responses = strsplit(Responses, split = ""),
         KeyID = paste0(Form_ID, Version_ID, "_keys.csv"),
         across(c(Month, Candidate_ID, Form_ID, Version_ID, Pass_Fail),
                factor))

keys_needed <-
  dat %>% pull(KeyID) %>% unique()


keys_available <-
  list.files(here("Keys"))

## TODO: Log some sort of warning if not all keys are available.
#Filter out forms that don't have associated keys
dat <-
  dat %>% filter(KeyID %in% keys_available)

keys_to_pull <-
  dat %>%
  pull(KeyID) %>% unique()

path_to_keys <- paste0(here("Keys"), "/", keys_to_pull)


##Name the keys and combine them into one table.
names(path_to_keys) <- keys_to_pull
key_table =
  map_dfr(.x = path_to_keys,
          .f = ~read_csv(.x, col_types = list(itemID = col_factor())),
          .id = "KeyID")
