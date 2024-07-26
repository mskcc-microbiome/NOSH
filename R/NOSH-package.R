#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom dplyr select n_distinct left_join filter rename if_any everything case_when across distinct full_join where slice mutate group_by summarize all_of desc
#' @importFrom utils read.csv head
#' @importFrom stats complete.cases
#' @import shiny 
## usethis namespace: end
NULL
# redcap_variables <- c("eb_mrn", "fndds_main_food_description",
#                       "fndds_portion_description", "fndds_portion_weight_g",  "food_code_desc",)
utils::globalVariables(
  c('meal_date', 'fndds_food_code', "fndds_main_food_description", "fndds_portion_description", "fndds_portion_weight_g", "food_code_desc",
    "amt_eaten",  "consumed_value", "created_by", 
    "daily_total", "eb_mrn", "id", "in_redcap", "meal", "menu_item_name", "mrn",
    "nutrient", "portion_size", "raw_food_id","portion_consumed",
    "raw_food_serving_unit", "raw_to_fndds_unit_matcher", "record_id",
    "redcap_arm_export", "redcap_repeat_instance", "redcap_repeat_instrument",
    "sanitize_token", "serving_amt", "serving_amt_numeric", "serving_size", "session",
    "value", "latest_computrition_repeat_instance","needs_instance", 
    "computrition_amt_eaten", "computrition_consumed_unit", "computrition_consumed_amt",
    "unit_table_complete", "missing_andor_outdated_food_code", "n"
  )
)