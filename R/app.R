library(shiny)
tbl_width = 1200
tbl_height = 500

# TODO: give this a catchy name.  
# - CRUNCH (computrition revitalized: )
# - MUNCH (msk universal noms and computrition helper)
# snac (super nice access to computrition)
# dorito (diet observations registered in tables, obviously)

dietapp <- function(...){
  ui <- fluidPage(
    mainPanel(      width = tbl_width, height = tbl_height,
      fluidRow(
        column(mod_loadfile_ui('uploadfile'), width = 2),
      ),
      mod_showfile_ui("uploadfile")
    )
  )
  
  server = function(input, output, session) {
    mod_loadfile_server("uploadfile")
  }
  shinyApp(ui, server)
}


