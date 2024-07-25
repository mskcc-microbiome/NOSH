#' NOSH Shiny App
#'
#' @name NOSH
#' @rdname nosh 
#' @export
#' @import shiny
#' @return A shiny dashboard for all your beautiful food data
NOSH <- function(tbl_width=1200, tbl_height=500, ...){
  if (interactive()) dotenv::load_dot_env()
  init_data <- get_meal_entries()
  unannotated_food <-   get_meal_entries_lacking_fndds_match(
    dplyr::bind_rows(
      init_data$df %>% dplyr::select(raw_food_id, raw_food_serving_unit),
      get_redcap_unit_table())
  )
  ui <- navbarPage(
    title = "Nutrition Optimization for Science and Health",
    id = "tabs",
    tabPanel(
      title = "Upload Computrition Data",
      fluidPage(
        mainPanel(
          width = tbl_width, height = tbl_height,
          fluidRow(
            mod_instructions_ui("uploadfile"),
            column(mod_loadfile_ui('uploadfile'), width = 2),
            column(mod_dietdata_submitter_ui('uploadfile'), width = 2)
            ),
          mod_showfile_ui('uploadfile', tbl_width=tbl_width, tbl_height=tbl_height)
       )
      )
    ),
    tabPanel(
      title = "Review Unit Table",
      fluidPage(
        shinyjs::useShinyjs(),
        mod_instructions_ui("foodmatch"),
        mod_matchFNDDS_foodentry_ui("foodmatch", unannotated_food),
        mod_matchFNDDS_portionentry_ui("foodmatch", unannotated_food),
        mod_matchFNDDS_submitter_ui("foodmatch"),
        mod_matchFNDDS_ui("foodmatch")
      )
    ),
    tabPanel(
      title = "Data Overview",
      fluidPage(
        shinyjs::useShinyjs(),
        mod_datacompleteness_ui("dashboard"),
#        mod_nutrient_select_ui("dashboard"),
        mod_summary_table_ui("dashboard"),
        mod_meal_histogram_ui("dashboard")
      )
    ),
    tabPanel(
      title = "Patient Meal Explorer",
      fluidPage(
        shinyjs::useShinyjs(),
        mod_datacompleteness_ui("patient"),
        mod_mrn_select_ui("patient"),
        mod_bymeal_checkbox_io("patient"),
        mod_nutrient_select_ui("patient"),
        mod_patientdash_ui("patient")
        #mod_user_ui("patient"),
        
      )
    )
  )
  server = function(input, output, session) {
    if(Sys.getenv("NOSH_USER_TYPE") %in% c("", "BASIC")){
      print(paste("NOSH_USER_TYPE is", Sys.getenv("NOSH_USER_TYPE"), "; Disabling data upload and unit table upload"))
      hideTab(inputId = "tabs", target = "Upload Computrition Data")
      hideTab(inputId = "tabs", target = "Review Unit Table")
    }
    rv <- reactiveValues(current_redcap_diet_data=init_data$df, 
                         patients=init_data$patients)
    # computrition upload
    mod_loadfile_server("uploadfile", rv)
    # fndds matcher
    mod_matchFNDDS_server("foodmatch", df = unannotated_food)
    # data overview
    mod_dashboard_server("dashboard", rv)
    # 
    mod_patientdash_server("patient", rv)
  }
  shinyApp(ui, server)
}






