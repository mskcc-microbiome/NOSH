test_that("meal_tabulation works", {
  pt_data_full_merge <- merge_meals_and_units(dev_data = dev_data, unittable = unittable, fndds_summary = fndds_summary)
  nutrient_list <- c("energy_kcal", "cryptoxanthin_beta_mcg", "thiamin_mg")
  consumed <- tabulate_pt_nutrition(pt_data_full_merge, mrn=3, nutrient_list=nutrient_list,
                                    dt_start="1914-06-21", dt_end="1914-06-21")
  testthat::equals(consumed[1, "consumed_value"] , 703.5)
  
})
