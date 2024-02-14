library(shiny)
tbl_width = 1200
tbl_height = 500

# TODO: give this a catchy name.  
# - CRUNCH (computrition revitalized: )
# - MUNCH (msk universal noms and computrition helper)
# snac (super nice access to computrition)
# dorito (diet observations registered in tables, obviously)

dietapp <- function(...){
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
         ),
          mod_showfile_ui('uploadfile')
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
      )
    )
  )
  server = function(input, output, session) {
    mod_loadfile_server("uploadfile")
    mod_matchFNDDS_server("foodmatch", df = incomplete_data)
  }
  shinyApp(ui, server)
}






# app <- function(...){
#   ui <- fluidPage(
#     mainPanel(      width = tbl_width, height = tbl_height,
#       fluidRow(
#         column(mod_loadfile_ui('uploadfile'), width = 2),
#       ),
#       mod_showfile_ui("uploadfile")
#     )
#   )
#   
#   server = function(input, output, session) {
#     mod_loadfile_server("uploadfile")
#   }
#   shinyApp(ui, server)
# }
