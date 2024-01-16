mod_loadfile_ui <- function(id) {
  fileInput(NS(id, 'upload'), 'Upload your diet data file')
}
mod_showfile_ui <- function(id) {
  rhandsontable::rHandsontableOutput(NS(id, 'diet_file'), width = tbl_width, height = tbl_height)
}
mod_loadfile_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    raw_file <- reactive({
      req(input$upload)
      ext <- clean_diet_file(input$upload$datapath)
      print(ext)
      
    })
    output$diet_file <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(raw_file()) %>% 
        rhandsontable::hot_cols(fixedColumnsLeft = 2)
    })
  })
  
}
mod_loadfile_demo <- function() {
  
  ui <- fluidPage(mod_loadfile_ui("x"), mod_showfile_ui("x"))
  server <- function(input, output, session) {
    mod_loadfile_server("x")
  }
  shinyApp(ui, server)
  
}
mod_loadfile_demo()
