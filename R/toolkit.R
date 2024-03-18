#' get patient data
#'
#' @param dev_data 
#' @param unittable 
#' @param fndds_summary 
#'
#' @return
#' @export
#'
#' @examples
merge_meals_and_units <- function(dev_data, unittable, fndds_summary) {
  #load packages
  #merge tables 
  #unit table has record_id, raw_food_id, raw_food_serving_unit, fndds_food_code, raw_to_fndds_unit_matcher, fndds_portion_description (is this needed?), fndds_portion_weight_g, unit_table_complete (?)
  #fndds_summary has fndds_food_code, fndds_main_food_description, wweia, food breakdown 
  #dev_data has mrn, date_intake, meal, raw_food_id, serving_size, raw_food_serving_unit, amt_eaten (match by raw food serving unit too?)

pt_data_w_unit_table <- dplyr::left_join(dev_data, unittable %>% dplyr::mutate("raw_food_id" = gsub("2017 ", "", raw_food_id)) %>% dplyr::select(-record_id) %>% dplyr::distinct(), by = c("raw_food_id", "raw_food_serving_unit"))

missing_rows <- sum(is.na(pt_data_w_unit_table$fndds_food_code))
complete_rows <- sum(!is.na(pt_data_w_unit_table$fndds_food_code))

fndds_summary$fndds_food_code <- as.numeric(fndds_summary$fndds_food_code)
pt_data_full_merge <- dplyr::left_join(pt_data_w_unit_table %>% dplyr::filter(!is.na(fndds_food_code)), fndds_summary, by = "fndds_food_code")

return(pt_data_full_merge)

}


#' tabulate patient nutrition 
#'
#' @param pt_data_full_merge 
#'
#' @return
#' @export
#'
#' @examples
tabulate_pt_nutrition <- function(pt_data_full_merge, mrn, dt_start, dt_end, nutrient_list) {
  
  
}