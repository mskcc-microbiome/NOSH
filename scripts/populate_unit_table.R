# redcap token and URI are stored in a .env file as UNITTABLE_REDCAP_URI UNITTABLE_REDCAP_TOKEN
dotenv::load_dot_env()
library(REDCapR)
library(tidyverse)

path_unit_table <- file.path("148_both_batches_UNIT_table_EN_exclu.csv")
custom_food <- read.csv(path_unit_table) %>% janitor::clean_names() %>% 
  rename(raw_portion_unit = unit) %>% 
  mutate(raw_to_fndds_unit_matcher=NA)%>% 
  select(food_nsc, food_code, raw_portion_unit, raw_to_fndds_unit_matcher, 
         cal_psu, gram_psu, protein_psu, fat_psu, carbohydrate_psu) %>% 
  left_join(fndds_fai %>%
              select(fndds_food_code, fndds_main_food_description, ingredient_code, ingredient_weight_g) %>%
              group_by(fndds_food_code) %>%
              summarize(fndds_portion_weight_g = sum(ingredient_weight_g)) %>% 
              rename(food_code=fndds_food_code )) %>%
  mutate(raw_to_fndds_unit_matcher = fndds_portion_weight_g / gram_psu,
         fndds_portion_unit="FNDDS Recipe weight")



# # attempt to match up via carbs, as a comparison to the gram_psu unit in the original unit table
# reference_food_codes_nutrients <- fndds_fai %>%
#   select(fndds_food_code, fndds_main_food_description, ingredient_code, ingredient_weight_g) %>% 
#   group_by(fndds_food_code) %>% 
#   mutate(fndds_portion_weight_g = sum(ingredient_weight_g)) %>% 
#   distinct()%>% 
#   ungroup() %>% 
#   left_join(fndds_inv, relationship = "many-to-many") %>% 
#   filter(nutrient_description %in% c("Protein", "Carbohydrate","Energy")) %>% 
#   group_by(fndds_food_code, fndds_portion_weight_g, nutrient_description) %>% 
#   summarize(val=sum(nutrient_value))
# 
# 
# carb_refs <- reference_food_codes_nutrients %>% 
#   filter( nutrient_description == "Carbohydrate") %>% 
#   filter(val > 1)
# 
# foods_with_units_carbsolved <- custom_food %>%
#   select(-fndds_portion_weight_g) %>% 
#   inner_join(carb_refs %>% mutate(fndds_food_code = as.numeric(fndds_food_code)), by=c("food_code" = "fndds_food_code")) %>% 
#   mutate(predicted_portion = (carbohydrate_psu * 100) / val,
#          fndds_portion_unit = "default portion from ingredients table")
# 













keys <- REDCapR::redcap_metadata_read(
  redcap_uri  = Sys.getenv("UNITTABLE_REDCAP_URI"),
  token       = Sys.getenv("UNITTABLE_REDCAP_TOKEN"),
)$data
print("Loading REDcap data")
DF <-
  REDCapR::redcap_read(verbose = TRUE,
    col_types = readr::cols(.default = readr::col_character()),
    redcap_uri = Sys.getenv("UNITTABLE_REDCAP_URI"),
    token = Sys.getenv("UNITTABLE_REDCAP_TOKEN"),
  )$data 

# find the last entered record ID so we know where to start
max_records = 0
if (nrow(DF) >0 ) max_records = max(as.numeric(DF$record_id))

# modify the old spreadsheet's names
tmp <- custom_food %>%
  rename(
    raw_food_id=food_nsc,
    raw_food_serving_unit=raw_portion_unit,
    fndds_food_code=food_code,
    fndds_portion_description=fndds_portion_unit
  ) %>%
  mutate(
    raw_food_id = trimws(raw_food_id),
  ) %>% #dplyr::anti_join(DF, by=c("raw_food_id","raw_food_serving_unit" )) %>% 
  mutate(
    record_id = (max_records+1): (nrow(.)+max_records)
  ) %>% select(all_of(keys$field_name))

redcap_write(ds = tmp, overwrite_with_blanks = FALSE,
             redcap_uri = Sys.getenv("UNITTABLE_REDCAP_URI"),
             token = Sys.getenv("UNITTABLE_REDCAP_TOKEN"),
)




# if needing to clear the table.  Proceed with fear and trepidation
# batchsize = 10
# for(batch in 1:ceiling(nrow(DF))){
#   ids <- c((((batch - 1) * batchsize) + 1):(batch*batchsize))
#   ids <- ids[ids %in% DF$record_id]
#   redcap_delete(records_to_delete = ids,
#   redcap_uri = Sys.getenv("UNITTABLE_REDCAP_URI"),
#   token = Sys.getenv("UNITTABLE_REDCAP_TOKEN")
#   )
# }
