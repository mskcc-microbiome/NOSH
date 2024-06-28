test_that("parsing computrition works", {
  refdf <- structure(
    list(
      mrn = "00001234",
      meal_date = structure(15864, class = "Date"),
      meal = "Breakfast 1     (Menu: Regular)",
      raw_food_id = structure(1L, levels = "Sandwich, peantu butter and jellyfish", class = "factor"),
      serving_size = 1,
      raw_food_serving_unit = "each",
      computrition_portion_consumed=NA,
      amt_eaten = structure(
        NA_integer_,
        levels = c("Missing",
                   "0", "0.25", "0.33", "0.5", "0.66", "0.75", "1"),
        class = "factor"
      ),
      id = "00001234_2013-06-08_Breakfast 1     (Menu: Regular)_Sandwich, peantu butter and jellyfish"
    ),
    row.names = c(NA,
                  -1L),
    class = c("tbl_df", "tbl", "data.frame")
  )
  #expect_equal(refdf, clean_diet_file(system.file("extdata", package = "NOSH") ))
  expect_equal(
    refdf,
    clean_diet_file(system.file("testdata", "test_computrition_export.xlsx", package="NOSH"))
  )
})
test_that("parsing computrition works, correct regex for excluding daily averages", {
  refdf <- structure(
    list(
      mrn = "00001234",
      meal_date = structure(15864, class = "Date"),
      meal = "Breakfast 1     (Menu: Regular)",
      raw_food_id = structure(1L, levels = "Soup, Beef Broth 6 oz (0)", class = "factor"),
      serving_size = 1,
      raw_food_serving_unit = "each",
      computrition_portion_consumed=NA,
      amt_eaten = structure(
        NA_integer_,
        levels = c("Missing",
                   "0", "0.25", "0.33", "0.5", "0.66", "0.75", "1"),
        class = "factor"
      ),
      id = "00001234_2013-06-08_Breakfast 1     (Menu: Regular)_Soup, Beef Broth 6 oz (0)"
    ),
    row.names = c(NA,
                  -1L),
    class = c("tbl_df", "tbl", "data.frame")
  )
  #expect_equal(refdf, clean_diet_file(system.file("extdata", package = "NOSH") ))
  expect_equal(
    refdf,
    clean_diet_file(system.file("testdata", "test_computrition_export_regex.xlsx", package="NOSH"))
  )
})

test_that("parse computrition xls with/without portions", {
  ref <- c("1 serv", "1 pkt", "1 each", "6 ounce", "4 ounce", "4 ounce")
  positive_test <- clean_diet_file(system.file("testdata", "computrition_test_with_portions.xls", package="NOSH"))
  testthat::expect_equal(positive_test$computrition_portion_consumed, ref) 
  negative_test <- clean_diet_file(system.file("testdata", "test_computrition_export.xlsx", package="NOSH"))
  testthat::expect_equal(negative_test$computrition_portion_consumed, NA) 
})

test_that("parse computrition xls missing some cols but probably ok", {
  tmp <- clean_diet_file(system.file("testdata", "test_computrition_export_missing_columns.xlsx", package="NOSH"))
  testthat::expect_equal(nrow(tmp), 1)
})
test_that("mean data from meal-less patients",{
  #  pull_diet_redcap(unique(as.integer(mrn))) |> dput
  redcap_pull <- structure(list(record_id = 15, redcap_event_name = "baseline_arm_1", 
                 redcap_repeat_instrument = NA, redcap_repeat_instance = NA, 
                 eb_mrn = "00001234", meal_date = NA, meal = NA, raw_food_id = NA, 
                 raw_food_serving_unit = NA_character_, serving_size = NA, 
                 amt_eaten = NA, upload_date = NA, uploader = NA), row.names = c(NA, -1L), class = "data.frame")
  cleaned_redcap_pull <- clean_diet_redcap(redcap_pull)
  testthat::expect_equal(1, nrow(cleaned_redcap_pull))
  testthat::expect_true("00001234" %in% cleaned_redcap_pull$eb_mrn)
}
)


test_that("we assign valid repeat instrument instances for diet data", {
  tmp <- data.frame(
    record_id=rep(c(1,3,4), each=4),
    eb_mrn=rep(c(1,3,4), each=4),
    redcap_repeat_instance=c(NA, NA, NA, NA,
                             1,2,3,4,
                             1,NA, NA, NA)
  )
  redcap_dtls <- get_patients_latest_record(tmp)
  desired_repeat_instances <- c(1:4, 5:8, 2:5)
  
  # this should error because we don't have one of the required columns
  expect_error(populate_redcap_repeat_instance(tmp))
  tmp$redcap_repeat_instrument <- "computrition_data"
  result <-   tmp %>% 
    left_join(redcap_dtls %>% rename("latest_computrition_repeat_instance"=redcap_repeat_instance), by = "eb_mrn") %>%
    dplyr::mutate(redcap_repeat_instance = NA,
                  redcap_repeat_instrument= "computrition_data") %>% 
    populate_redcap_repeat_instance() 
      expect_equal(desired_repeat_instances, result$redcap_repeat_instance)
})
test_that("parsing computrition retains mrn character type", {
  refdf <- structure(
    list(
      mrn = "00001234",
      meal_date = structure(15864, class = "Date"),
      meal = "Breakfast 1     (Menu: Regular)",
      raw_food_id = structure(1L, levels = "Sandwich, peantu butter and jellyfish", class = "factor"),
      serving_size = 1,
      raw_food_serving_unit = "each",
      computrition_portion_consumed=NA,
      amt_eaten = structure(
        NA_integer_,
        levels = c("Missing",
                   "0", "0.25", "0.33", "0.5", "0.66", "0.75", "1"),
        class = "factor"
      ),
      id = "00001234_2013-06-08_Breakfast 1     (Menu: Regular)_Sandwich, peantu butter and jellyfish"
    ),
    row.names = c(NA,
                  -1L),
    class = c("tbl_df", "tbl", "data.frame")
  )
  #expect_equal(refdf, clean_diet_file(system.file("extdata", package = "NOSH") ))
  expect_equal(
    refdf,
    clean_diet_file(system.file("testdata", "test_computrition_export.xlsx", package="NOSH"))
  )
})

