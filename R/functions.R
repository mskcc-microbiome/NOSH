# library(shiny)
# library(rhandsontable)
# #library(ggplot2)
# #library(tidyverse)
# library(tidyr)
# library(dplyr)
# library(stringr)
# library(lubridate)
# #library(readxl)
# library(janitor)


clean_diet_file <- function(filepath, unit_table){
  computrition_export_raw <- readxl::read_excel(filepath) %>%
    select(where(function(x) any(!is.na(x))))
  
  computrition_export <- computrition_export_raw
  
  new_names <- computrition_export[stringr::str_detect(computrition_export$Name, "Menu Item"),] %>%
    slice(1) %>%
    as.character() %>%
    stringr::str_replace_all("\n|\r", "") %>% as.vector()
  
  new_names[is.na(new_names)] <- c("meal", "mrn")
  
  names(computrition_export) <- new_names
  
  remove_items <- c("Name", "MRN", "Room", "Menu Item Name", "Acuity", "Current Menu", "Current Diet Order", "TVR Order", "PLEASE RUSH ORDER", "Parent/Guest Tray To Follow")
  
  computrition_export_clean <- computrition_export %>% 
    janitor::clean_names() %>%
    mutate(mrn = as.integer(mrn),
           date_intake = case_when(
             stringr::str_detect(menu_item_name, "Date: ") ~ lubridate::mdy(menu_item_name), 
             TRUE ~ NA_Date_)) %>%
    tidyr::fill(c("meal", "mrn", "date_intake"), .direction = "down") %>%
    filter(!stringr::str_detect(tolower(menu_item_name), "(daily|[:digit:]{1} [value|average|total])|(condiments, salt|pepper)|(\\*{8})") &
             !stringr::str_detect(menu_item_name, "Date:  ") & !is.na(meal) & 
             !menu_item_name %in% remove_items
    )
  
  # computrition_export_clean <- filter(computrition_export_clean, !is.na(portion_size)) %>%
  computrition_export_clean <- computrition_export_clean %>%
    select(menu_item_name, meal, date_intake, mrn:iron_mg) %>%
    mutate(
      splits = stringr::str_split_fixed(portion_size, pattern = ' ', n = 2),
      serving_amt = splits[,1],
      unit = splits[,2],
      across(c(serving_amt), 
             function(x) as.numeric(readr::parse_number(x)),
             .names = "{col}_numeric"),
      serving_amt_numeric = case_when(
        str_detect(serving_amt, "1/2") ~ 0.5,
        str_detect(serving_amt, "1/4") ~ 0.25,
        TRUE ~ serving_amt_numeric
      ),
      food_nsc = sub("^\\^", "", menu_item_name),
      portion_consumed = factor(NA_integer_, levels = c(0, 1/4, 1/3, 1/2, 2/3, 3/4, 1))
    ) %>%
    # select(mrn, date_intake, meal, menu_item_name, food_nsc, portion_size, portion_consumed, unit, serving_amt_numeric) %>%
    # mutate(food_nsc = factor(food_nsc, levels = sort(unique(c(unit_table$food_nsc)))),
    #        unit = case_when(is.na(food_nsc) ~ NA_character_, TRUE ~ unit)
    #        )
    mutate(food_nsc = factor(food_nsc)) %>% #, levels = sort(unique(unit_table$food_nsc)))) %>%
    select(mrn, date_intake, meal, food_nsc,serving_amt_numeric, unit,  portion_consumed) %>% 
    rename(
      meal_date=date_intake,
      raw_food_id=food_nsc,
      raw_food_serving_unit=unit,
      serving_size=serving_amt_numeric,
      amt_eaten=portion_consumed)
# left_join(select(unit_table, food_nsc, unit, food_code, description), by = c("food_nsc", "unit")) %>%
  # select(mrn, date_intake, meal, menu_item_name, description, portion_consumed, serving_amt_numeric, unit) %>%
  # mutate(description = factor(description, levels = sort(unique(description))))
  
  
  return(computrition_export_clean)
}



load_unit_table <- function() {
  prefix_vdb <- ifelse(Sys.info()['sysname'] == 'Darwin',   '/Volumes/vandenBrinkLab/',  '//rtssdc/vandenBrinkLab/')
  path_study_folder <- file.path(prefix_vdb, '_clinical_research_team_folder/diet_study/testing_diet_data_processing/food_codes/')
  path_unit_table <- file.path(path_study_folder, "148_both_batches_UNIT_table_EN_exclu.csv")
  unit_table <- read.csv(path_unit_table) %>% clean_names()
  
  # Due to internal changes in computrition, need to include menu items with out any arbitrary dates affixed to them (eg "2017) - computrition files will alternately contain dates or no dates in menu item names
  re <- c("20[:digit:]{2} {0,1},{0,1}", "\\(\\.{0,1}[:digit:]{1}\\)")
  re <- c(re[1], re[2], paste(re, collapse = "|"))
  
  for(r in re) {
    unit_table_new <- unit_table %>%
      mutate(
        index = 1:nrow(.),
        food_nsc = stringr::str_remove_all(food_nsc, r) %>% trimws("both")
      )
    
    unit_table <- unit_table %>%
      mutate(index = 1:nrow(.)) %>%
      rbind(unit_table_new) %>%
      arrange(index) %>%
      select(-index) %>%
      distinct_at(vars(-food_all), .keep_all = T)
  }
  
  return(unit_table)
}
