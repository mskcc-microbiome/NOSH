library(shiny)
library(rhandsontable)
#library(ggplot2)
#library(tidyverse)
library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
#library(readxl)
library(janitor)
library(REDCapR)
library(readr)


clean_diet_file <- function(filepath){
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
             TRUE ~ lubridate::NA_Date_)) %>%
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
        stringr::str_detect(serving_amt, "1/2") ~ 0.5,
        stringr::str_detect(serving_amt, "1/4") ~ 0.25,
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



pull_diet_redcap <- function(mrn_vec) {
  dotenv::load_dot_env()
  
  print(mrn_vec)
  cert_location <- system.file("cacert.pem", package = "openssl")
  if (file.exists(cert_location)) {
    config_options <- list(cainfo = cert_location,
                           content='record',
                           action='export',
                           format='csv',
                           type='flat',
                           csvDelimiter='',
                           rawOrLabel='raw',
                           rawOrLabelHeaders='raw',
                           exportCheckboxLabel='false',
                           exportSurveyFields='false',
                           exportDataAccessGroups='false',
                           returnFormat='csv'
    )
    
    # filter_statement <- paste0("[eb_mrn] in (", toString(sprintf("'%s'", mrn_vec)), ")")
    # print(filter_statement)
    
    redcap_pull = lapply(mrn_vec, FUN = function(mrn){
      
    ds_different_cert_file1 <- redcap_read_oneshot(
      col_types = cols(eb_mrn = col_integer()),
      # records = mrn_vec,
      # forms = c("computrition_data"),
      fields = c("record_id", "eb_mrn"),
      filter_logic = paste0("[eb_mrn]=", mrn),
      # filter_logic = filter_statement,
      redcap_uri = Sys.getenv("DIETDATA_REDCAP_URI"),
      token = Sys.getenv("DIETDATA_REDCAP_TOKEN"),
      config_options = config_options
    )$data
    
    # print(str(ds_different_cert_file1))
    
    
    if(nrow(ds_different_cert_file1) > 0) {
      ds_different_cert_file2 <- redcap_read_oneshot(
        col_types = cols(eb_mrn = col_integer(), raw_food_serving_unit = col_character()),
        records = ds_different_cert_file1$record_id,
        # forms = c("computrition_data"),
        fields = c("record_id", "meal_date", "eb_mrn", "raw_food_id", "raw_food_serving_unit", "serving_size", "amt_eaten"),
        # filter_logic = paste0("[eb_mrn]=", mrn_vec),
        # filter_logic = paste0("[record_id]=", ds_different_cert_file1$record_id),
        redcap_uri = Sys.getenv("DIETDATA_REDCAP_URI"),
        token = Sys.getenv("DIETDATA_REDCAP_TOKEN"),
        config_options = config_options
      )$data
    }
    
    
    # print(str(ds_different_cert_file2))
    
    }) %>%
      bind_rows()
    
    
    
    if(nrow(redcap_pull) > 0) {
      redcap_pull <- redcap_pull%>%
        fill(eb_mrn) %>%
        filter(redcap_repeat_instrument == "computrition_data")
    }

    print(str(redcap_pull))
    
  }
  
  redcap_pull
  
}


