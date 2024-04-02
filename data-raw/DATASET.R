# prepare data
library(magrittr)

files <- list("foodandbev.xlsx"="https://www.ars.usda.gov/ARSUserFiles/80400530/apps/2019-2020%20FNDDS%20At%20A%20Glance%20-%20Foods%20and%20Beverages.xlsx",
              "portionsandweights.xlsx"="https://www.ars.usda.gov/ARSUserFiles/80400530/apps/2019-2020%20FNDDS%20At%20A%20Glance%20-%20Portions%20and%20Weights.xlsx",
              "foodandingredients.xlsx"="https://www.ars.usda.gov/ARSUserFiles/80400530/apps/2019-2020%20FNDDS%20At%20A%20Glance%20-%20FNDDS%20Ingredients.xlsx",
              "nutrients.xlsx"="https://www.ars.usda.gov/ARSUserFiles/80400530/apps/2019-2020%20FNDDS%20At%20A%20Glance%20-%20Ingredient%20Nutrient%20Values.xlsx",
              "foodsummary.xlsx"="https://www.ars.usda.gov/ARSUserFiles/80400530/apps/2019-2020%20FNDDS%20At%20A%20Glance%20-%20FNDDS%20Nutrient%20Values.xlsx"
)
for (i in 1:length(files)){
  if (!file.exists(names(files)[i])){
    download.file(files[[i]], names(files)[i])
  }
}

fndds_fab <- readxl::read_xlsx(names(files)[1], skip = 1, col_types = "text") %>% janitor::clean_names() %>% 
  rename(
    fndds_food_code=food_code,
    fndds_main_food_description=main_food_description)
    
# ingredient nutrietns
fndds_inv <- readxl::read_xlsx(names(files)[4], skip = 1) %>% janitor::clean_names()

# food and ingredienates
# take a look at 14650170 vs  14650160:  sometimes foods have over foods as ingredients, as well as the actual ingredients found in the official fndds ingredients list.
# we have to "unnest" this, til we have no food_codes in the ingredients_columns.
# Could do this recursively but why would i do that to myself on a monday?

# Also, for some reason, Crepe, with meat (58120110) has two milk 11100000 entries (2 and 8): one for 195.2g and one for 162.748g.
# I had to fix that below.  Thats just my lot in life.

fndds_fai_raw <- readxl::read_xlsx(names(files)[3], skip = 1) %>% janitor::clean_names() %>% 
  rename(
    fndds_food_code=food_code,
    fndds_main_food_description=main_food_description) %>% 
  filter(!(fndds_food_code == 58120110 & seq_num == 8)) %>% 
  mutate(ingredient_weight_g = ifelse(fndds_food_code == 58120110 & seq_num == 2,  195.200 + 162.748, ingredient_weight_g))




# if you aren't nauseous, you aren't looking hard enough 
unnest_one_level_of_foodingredients <- function(df){
  # Note the first group_by and summmarize deals with cases where compound foods contain two foods that share ingredients.
  # Eg chicken wings 24168020 consists of [24168000 and 24168010], both of which have 81100000 Table fat, NFS.
  # if we dont aggregate before we do our joins we end up with bad calculations for the total proportions
  # those shared sub-ingredients should contain
  
  # Note also that we have to average the moisture retention. See how Fish NFS (26100110) has 2 (of 4) ingredients
  # 26100130 for baked and breaded as well as 26100140 for fried. The batter/breading 99995000 has two different moisture contents
  df %>% 
    dplyr::filter(ingredient_code %in% fndds_fai_raw$fndds_food_code) %>% 
    dplyr::group_by(fndds_food_code, fndds_main_food_description, wweia_category_number, wweia_category_description, ingredient_code, ingredient_description, retention_code) %>% 
    dplyr::summarise(ingredient_weight_g = sum(ingredient_weight_g),
                     moisture_change_percent = mean(moisture_change_percent, na.rm = TRUE),
                     seq_num=1) %>% 
    dplyr::ungroup() %>% 
    dplyr::left_join(fndds_fai_raw %>% dplyr::select(fndds_food_code, ingredient_code, ingredient_description, ingredient_weight_g, moisture_change_percent) %>% distinct(), by=c("ingredient_code" = "fndds_food_code")) %>%
    dplyr::group_by(fndds_food_code, fndds_main_food_description, ingredient_code) %>% 
    dplyr::mutate(ingredient_weight_g = (ingredient_weight_g.y * ingredient_weight_g.x)/sum(ingredient_weight_g.y)) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-ingredient_code) %>%
    dplyr::rename(ingredient_code=ingredient_code.y, ingredient_description=ingredient_description.y, moisture_change_percent=moisture_change_percent.y )  %>% 
    dplyr::select(dplyr::all_of(colnames(fndds_fai_raw)))
}
traverse_foods_1 <- unnest_one_level_of_foodingredients(fndds_fai_raw)
traverse_foods_2 <- unnest_one_level_of_foodingredients(traverse_foods_1)
traverse_foods_3 <- unnest_one_level_of_foodingredients(traverse_foods_2)
traverse_foods_4 <- unnest_one_level_of_foodingredients(traverse_foods_3)
traverse_foods_5 <- unnest_one_level_of_foodingredients(traverse_foods_4)
traverse_foods_6 <- unnest_one_level_of_foodingredients(traverse_foods_5)
traverse_foods_7 <- unnest_one_level_of_foodingredients(traverse_foods_6)

# create a distinct dataset by merging all together then culling any rows where ingredient is not an ingredient
fndds_fai <- dplyr::bind_rows(fndds_fai_raw, traverse_foods_1, traverse_foods_2, traverse_foods_3, traverse_foods_4,traverse_foods_5,traverse_foods_6,traverse_foods_7) %>% 
  dplyr::filter(!ingredient_code %in% fndds_fai_raw$fndds_food_code) %>% 
  dplyr::arrange(fndds_food_code, desc(ingredient_weight_g))

# sanity check portion size
raw_portions <- fndds_fai_raw %>% dplyr::group_by(fndds_food_code) %>% dplyr::summarize(portion=round(sum(ingredient_weight_g),4))
unnested_portions <- fndds_fai %>% dplyr::group_by(fndds_food_code) %>% dplyr::summarize(portion=round(sum(ingredient_weight_g),4))

# anti-joins should result in now rows in both directions
testthat::expect_equal(nrow(dplyr::anti_join(unnested_portions, raw_portions)), 0)
testthat::expect_equal(nrow(dplyr::anti_join(raw_portions, unnested_portions)), 0)


# portions 
fndds_paw <- readxl::read_xlsx(names(files)[2], skip = 1) %>% janitor::clean_names() %>% 
  rename(
    fndds_food_code=food_code,
    fndds_portion_weight_g = portion_weight_g,
    fndds_main_food_description=main_food_description,
    fndds_portion_description=portion_description)

# here is some sample data for experimenting with
# we have to remove MRNs, and scramble some dates.
dev_data <- clean_diet_file("Intake Analysis Report.xlsx") %>% 
  mutate(mrn=as.numeric(as.factor(mrn))) %>%
  rename(date_intake = meal_date)
random_dates <- sample(seq(as.Date("1834-01-01"), as.Date("2045-01-01"), by = "day"), length(unique(dev_data$date_intake)), replace = TRUE)
dev_data$date_intake <- factor(dev_data$date_intake, labels=random_dates)

# No longer needed after importing to REDcap
## Here is the current unit table:
# prefix_vdb <- ifelse(Sys.info()['sysname'] == 'Darwin',   '/Volumes/vandenBrinkLab/',  '//rtssdc/vandenBrinkLab/')
# path_study_folder <- file.path(prefix_vdb, '_clinical_research_team_folder/diet_study/testing_diet_data_processing/food_codes/')
# path_unit_table <- file.path(path_study_folder, "148_both_batches_UNIT_table_EN_exclu.csv")
# custom_food <- read.csv(path_unit_table) %>% janitor::clean_names() %>% 
#   rename(raw_portion_unit = unit) %>% 
#   mutate(raw_to_fndds_unit_matcher=NA,
#          fndds_portion_unit=NA,
#          fndds_portion_weight_g=NA) %>% 
#     select(food_nsc, food_code, raw_portion_unit, raw_to_fndds_unit_matcher, fndds_portion_unit, fndds_portion_weight_g)


fndds_summary <- readxl::read_xlsx(names(files)[5], skip = 1, col_types = "text") %>% janitor::clean_names() %>% 
  rename(
    fndds_food_code=food_code,
    fndds_main_food_description=main_food_description)


## Get unittable from redcap
dotenv::load_dot_env()
unittable <- get_redcap_unit_table()

redcap_config_options  <- list(
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



usethis::use_data(fndds_fab, fndds_inv, fndds_fai, fndds_paw, fndds_summary, dev_data, unittable, redcap_config_options, internal = TRUE, overwrite = TRUE)
