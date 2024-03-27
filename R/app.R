#' NOSH Shiny App
#'
#' @name NOSH
#' @rdname nosh
#' @export
#' @import shiny
#' @return A shiny dashboard for all your beautiful food data
NOSH <- function(tbl_width=1200, tbl_height=500, ...){
  if (interactive()) dotenv::load_dot_env()
  custom_food <<- get_redcap_unit_table()
  incomplete_data <- get_meal_entries_lacking_fndds_match()
  ui <- navbarPage(
    title = "Diet data processing",
    tabPanel(
      title = "Computrition data entry",
      fluidPage(
        mainPanel(
          width = tbl_width, height = tbl_height,
          fluidRow(
            column(mod_loadfile_ui('uploadfile'), width = 2),
            column(mod_dietdata_submitter_ui('uploadfile'), width = 2)
            ),
          mod_showfile_ui('uploadfile', tbl_width=tbl_width, tbl_height=tbl_height)
       )
      )
    ),
    tabPanel(
      title = "Menu item matching",
      fluidPage(
        shinyjs::useShinyjs(),
        mod_matchFNDDS_foodentry_ui("foodmatch", incomplete_data),
        mod_matchFNDDS_portionentry_ui("foodmatch", incomplete_data),
        mod_matchFNDDS_submitter_ui("foodmatch"),
        mod_matchFNDDS_ui("foodmatch")
      )
    ),
    tabPanel(
      title = "meal outcomes",
      fluidPage(
        shinyjs::useShinyjs(),
        mod_mrn_select_ui("patient"), 
        mod_nutrient_select_ui("patient"), 
        mod_dashboard_ui("patient"),
        mod_user_ui("patient"),
        
      )
    )
  )
  server = function(input, output, session) {
    mod_loadfile_server("uploadfile")
    mod_matchFNDDS_server("foodmatch", df = incomplete_data)
    mod_dashboard_server("patient")
  }
  shinyApp(ui, server)
}






