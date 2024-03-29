
mod_loadfile_ui <- function(id) {
  fileInput(NS(id, 'upload'), 'Upload your diet data file')
}
mod_showfile_ui <- function(id,  tbl_width=1200, tbl_height=500) {
  rhandsontable::rHandsontableOutput(NS(id, 'diet_file'), width = tbl_width, height = tbl_height)
}
mod_download_progress_ui <- function(id){
  ns <- NS(id)
  downloadButton(ns("download_data"), label = "Download")
}

mod_dietdata_submitter_ui <- function(id) {
  ns <- NS(id)
  actionButton(
    outputId = ns("computrition_to_redcap"),
    label = "Write completed diet data to REDcap")
}

mod_loadfile_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    raw_file <- reactive({
      req(input$upload)
      ext <- clean_diet_file(input$upload$datapath)
      
      print(paste("number of rows in table before:",  nrow(ext)))
      
      redcap_current <- pull_diet_redcap(unique(as.integer(ext$mrn))) %>%
        clean_diet_redcap()
      
      ext <- filter(ext, !id %in% redcap_current$id) %>%
        select(-id)
      
      print(paste("number of rows in table after:",  nrow(ext)))
      
      ext
      #clean_diet_file(input$upload$datapath)
      
    })
    
    current_data <- reactive(get(input$diet_file))
    
    output$diet_file <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(raw_file()) %>% 
        rhandsontable::hot_cols(fixedColumnsLeft = 2)
    })

    output$download_data <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        write.xlsx(current_data(), file)
      })
    
    
    observeEvent(input$computrition_to_redcap, {
      # req(input$diet_file)
      diet_table <- hot_to_r(input$diet_file)
      
      print(head(diet_table))
      
      diet_table <- diet_table %>%
        filter(!is.na(amt_eaten))
      
      print(head(diet_table %>% filter(!is.na(amt_eaten))))
      
      push_to_redcap(diet_table)
      
      # raw_file <- filter(raw_file, !id %in% redcap_current$id) %>%
      #   select(-id)
      # 
      # print(paste("number of rows in table after data entry:",  nrow(raw_file)))
      
      session$reload()
    })
    
  })
  
}

mod_loadfile_demo <- function() {
  
  ui <- fluidPage(
    mod_loadfile_ui("x"),
    mod_showfile_ui("x"),
    mod_dietdata_submitter_ui("x"),
    mod_download_progress_ui("x"))
  server <- function(input, output, session) {
    mod_loadfile_server("x")
  }
  shinyApp(ui, server)
  
}

