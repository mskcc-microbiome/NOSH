mod_instructions_ui <- function(id) {
  uiOutput(NS(id, 'instructions'),)
}
mod_loadfile_ui <- function(id) {
  fileInput(NS(id, 'upload'), 'Upload your diet data file')
}
mod_showfile_ui <- function(id,  tbl_width=1200, tbl_height=500) {
  rhandsontable::rHandsontableOutput(NS(id, 'diet_file'), width = tbl_width, height = tbl_height)
}


mod_dietdata_submitter_ui <- function(id) {
  actionButton(
    inputId = NS(id, "computrition_to_redcap"),
    label = "Write completed diet data to REDcap")
}

mod_loadfile_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    raw_file <- reactive({
      req(input$upload)
      if (is.na(readxl:::format_from_signature(input$upload$datapath))){
        showNotification("Computrition exports malformed  Excel files; this can be 'repaired' by opening this file in Excel first, and re-saving.")
      }
      req(!is.na(readxl:::format_from_signature(input$upload$datapath)))
      ext <- clean_diet_file(input$upload$datapath)
      
      print(paste("number of rows in uploaded table:",  nrow(ext)))
      
      # redcap_current <- pull_diet_redcap(unique(as.integer(ext$mrn))) %>%
      #   clean_diet_redcap()
      print(paste("number of rows after filtering against data in redcap:",  nrow(ext)))
      ext <- dplyr::filter(ext, mrn %in% rv$current_redcap_diet_data$mrn)
      print(paste("number of rows after removing mrns missing from redcap",  nrow(ext)))
      if(nrow(ext) == 0){
        showNotification("No enterable data found; this is likely due to this patient not being registered in REDCap; please register this patients first.")
      }
      ext
      
    })
    output$diet_file <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(
        raw_file() %>% dplyr::filter(!id %in% rv$current_redcap_diet_data$id) %>%
          dplyr::select(-id)
        
        ) %>% 
        rhandsontable::hot_cols(fixedColumnsLeft = 2)
    })
    
    output$instructions <- renderUI({
      includeMarkdown(system.file("md", 'upload_instructions.md', package="NOSH"))
#      HTML(markdown::markdownToHTML(system.file("md", 'upload_instructions.md', package="NOSH")))
    })
    observeEvent(input$computrition_to_redcap, {
      req(input$diet_file)
      diet_table_raw <- rhandsontable::hot_to_r(input$diet_file)
      diet_table <- diet_table_raw %>%
        filter(!is.na(amt_eaten))
      if (nrow(diet_table) > 0 ){
        showNotification(paste("Uploading ", nrow(diet_table), "rows of data"))
        #print(head(diet_table))
        
        
        #print(head(diet_table %>% filter(!is.na(amt_eaten))))
        records_written_output <- push_to_redcap(diet_table, session)
        showNotification(paste(records_written_output$res$outcome_message, 
                               "consisting of", records_written_output$nrows, "meal items"))
        
        # raw_file <- filter(raw_file, !id %in% redcap_current$id) %>%
        #   select(-id)
        # 
        # print(paste("number of rows in table after data entry:",  nrow(raw_file)))
        rv$current_redcap_diet_data <- get_meal_entries()
        #session$reload()
      } else{
        showNotification("No data to write; please upload and annotate some diet data first")
      }
    })
    
  })
  
}
mod_loadfile_demo <- function() {
  
  ui <- fluidPage(
    mod_instructions_ui("x"),
    mod_loadfile_ui("x"),
    mod_showfile_ui("x"),
    mod_dietdata_submitter_ui("x")
    )
  server <- function(input, output, session) {
    rv <- reactiveValues(current_redcap_diet_data=get_meal_entries() )
    mod_loadfile_server("x", rv)
  }
  shinyApp(ui, server)
  
}


