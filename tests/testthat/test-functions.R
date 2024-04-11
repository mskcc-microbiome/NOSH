test_that("parsing computrition works", {
  refdf <- structure(
    list(
      mrn = 1234L,
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
      id = "1234_2013-06-08_Breakfast 1     (Menu: Regular)_Sandwich, peantu butter and jellyfish"
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

test_that("parse computrition xls with/without portions", {
  ref <- c("1 serv", "1 pkt", "6 ounce", "4 ounce", "4 ounce")
  positive_test <- clean_diet_file(system.file("testdata", "computrition_test_with_portions.xls", package="NOSH"))
  testthat::expect_equal(positive_test$computrition_portion_consumed, ref) 
  negative_test <- clean_diet_file(system.file("testdata", "test_computrition_export.xlsx", package="NOSH"))
  testthat::expect_equal(negative_test$computrition_portion_consumed, NA) 
})

test_that("parse computrition xls missing some cols but probably ok", {
  tmp <- clean_diet_file(system.file("testdata", "test_computrition_export_missing_columns.xlsx", package="NOSH"))
  testthat::expect_equal(nrow(tmp), 1)
})

