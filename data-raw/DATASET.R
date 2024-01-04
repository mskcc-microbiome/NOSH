# prepare data
library(magrittr)
files <- list("foodandbev.xlsx"="https://www.ars.usda.gov/ARSUserFiles/80400530/apps/2019-2020%20FNDDS%20At%20A%20Glance%20-%20Foods%20and%20Beverages.xlsx",
              "portionsandweights.xlsx"="https://www.ars.usda.gov/ARSUserFiles/80400530/apps/2019-2020%20FNDDS%20At%20A%20Glance%20-%20Portions%20and%20Weights.xlsx",
              "foodandingredients.xlsx"="https://www.ars.usda.gov/ARSUserFiles/80400530/apps/2019-2020%20FNDDS%20At%20A%20Glance%20-%20FNDDS%20Ingredients.xlsx",
              "nutrients.xlxs"="https://www.ars.usda.gov/ARSUserFiles/80400530/apps/2019-2020%20FNDDS%20At%20A%20Glance%20-%20Ingredient%20Nutrient%20Values.xlsx"
)
for (i in 1:length(files)){
  if (!file.exists(names(files)[i])){
    download.file(files[[i]], names(files)[i])
  }
}

fndds_fab <- readxl::read_xlsx(names(files)[1], skip = 1, col_types = "text") %>% janitor::clean_names()
# ingredient nutrietns
fndds_inv <- readxl::read_xlsx(names(files)[4], skip = 1) %>% janitor::clean_names()
# food and ingredienates
fndds_fai <- readxl::read_xlsx(names(files)[3], skip = 1, col_types = "text") %>% janitor::clean_names()
# portions 
fndds_paw <- readxl::read_xlsx(names(files)[2], skip = 1) %>% janitor::clean_names() #%>%

# here is some sample data for experimenting with
# we have to remove MRNs, and scramble some dates.
dev_data <- clean_diet_file("Intake Analysis Report.xlsx", unit_table = load_unit_table()) %>% 
  mutate(mrn=as.numeric(as.factor(mrn))) 
random_dates <- sample(seq(as.Date("1834-01-01"), as.Date("2045-01-01"), by = "day"), length(unique(dev_data$date_intake)), replace = TRUE)
dev_data$date_intake <- factor(dev_data$date_intake, labels=random_dates)


## Here is the current unit table:
prefix_vdb <- ifelse(Sys.info()['sysname'] == 'Darwin',   '/Volumes/vandenBrinkLab/',  '//rtssdc/vandenBrinkLab/')
path_study_folder <- file.path(prefix_vdb, '_clinical_research_team_folder/diet_study/testing_diet_data_processing/food_codes/')
path_unit_table <- file.path(path_study_folder, "148_both_batches_UNIT_table_EN_exclu.csv")
custom_food <- read.csv(path_unit_table) %>% clean_names() %>% 
  rename(raw_portion_unit = unit) %>% 
  mutate(raw_to_fndds_unit_matcher=NA,
         fndds_portion_unit=NA,
         fndds_portion_weight_g=NA) %>% 
    select(food_nsc, food_code, raw_portion_unit, raw_to_fndds_unit_matcher, fndds_portion_unit, fndds_portion_weight_g)
usethis::use_data(fndds_fab, fndds_inv, fndds_fai, fndds_paw, dev_data, custom_food, internal = TRUE, overwrite = FALSE)
