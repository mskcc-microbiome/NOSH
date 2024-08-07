#' get patient data
#'
#' @param dev_data 
#' @param unittable 
#' @param fndds_summary 
#'
#' @return list
#' @export
#'
#' @examples
merge_meals_and_units <- function(dev_data, unittable, fndds_summary) {
  #load packages
  #merge tables 
  #unit table has record_id, raw_food_id, raw_food_serving_unit, fndds_food_code, raw_to_fndds_unit_matcher, fndds_portion_description (is this needed?), fndds_portion_weight_g, unit_table_complete (?)
  #fndds_summary has fndds_food_code, fndds_main_food_description, wweia, food breakdown 
  #dev_data has mrn, meal_date, meal, raw_food_id, serving_size, raw_food_serving_unit, amt_eaten (match by raw food serving unit too?)
  # date intake should be a date type, and amt eaten shouold be numeric
  
  pt_data_w_unit_table <- dplyr::left_join(
    dev_data %>% 
      mutate(meal_date = as.Date(meal_date),
             amt_eaten = as.numeric(amt_eaten)),
    unittable %>% 
      # dplyr::mutate("raw_food_id" = gsub("2017 ", "", raw_food_id)) %>% 
      dplyr::select(-record_id) %>% 
      dplyr::distinct(), 
    by = c("raw_food_id", "raw_food_serving_unit"))
  incomplete_data <- pt_data_w_unit_table %>% filter(is.na(fndds_food_code))
  missing_rows <- nrow(incomplete_data)
  complete_rows <- sum(!is.na(pt_data_w_unit_table$fndds_food_code))
  
  fndds_summary$fndds_food_code <- as.numeric(fndds_summary$fndds_food_code)
  pt_data_full_merge <- dplyr::left_join(pt_data_w_unit_table %>% dplyr::filter(!is.na(fndds_food_code)), fndds_summary, by = "fndds_food_code")
  
  return(list("df"=pt_data_full_merge, "status"=paste(missing_rows, "of the", nrow(pt_data_w_unit_table), "meal entries are missing an FNDDS match"), 
              "top_missing"=incomplete_data %>% select(raw_food_id) %>% dplyr::group_by(raw_food_id) %>% dplyr::count() %>% 
                dplyr::arrange(dplyr::desc(n)) %>% head))
  
}


#' tabulate patient nutrition 
#'
#' @param pt_data_full_merge output from merge_meals_and_units 
#'
#' @return
#' @export
#'
#' @examples
tabulate_pt_nutrition <- function(pt_data_full_merge, mrn, dt_start, dt_end, nutrient_list) {
  if (any(is.na(pt_data_full_merge$amt_eaten))){
    warning("Some amt_eaten entries are NA; please correct these.  Filling with '1' for now")
    pt_data_full_merge[is.na(pt_data_full_merge$amt_eaten), "amt_eaten"] <- 1
  }
  return(
    pt_data_full_merge %>% 
    dplyr::filter(mrn == {{ mrn }}) %>% 
      dplyr::filter(meal_date >= {{ dt_start }} ) %>% 
      dplyr::filter(meal_date  <= {{ dt_end }} ) %>%
      dplyr::select(1:15, all_of(nutrient_list)) %>%
      tidyr::pivot_longer(names_to = "nutrient", cols = dplyr::all_of(nutrient_list)) %>% 
      dplyr::mutate(value = as.numeric(value))  %>% 
      dplyr::mutate(consumed_value = (value/fndds_portion_weight_g) *  (fndds_portion_weight_g/raw_to_fndds_unit_matcher) * amt_eaten)
      
)
}
