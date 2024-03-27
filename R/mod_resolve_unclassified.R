get_redcap_unit_table <- function(){
  col_types = readr::cols(
    record_id = readr::col_double(),
    raw_food_id = readr::col_character(),
    raw_food_serving_unit = readr::col_character(),
    fndds_food_code = readr::col_double(),
    raw_to_fndds_unit_matcher = readr::col_double(),
    fndds_portion_description = readr::col_character(),
    fndds_portion_weight_g = readr::col_double(),
    unit_table_complete = readr::col_double()
  )
  REDCapR::redcap_read(verbose = TRUE,batch_size = 500,
                       col_types=col_types,
                       redcap_uri = Sys.getenv("UNITTABLE_REDCAP_URI"),
                       token = Sys.getenv("UNITTABLE_REDCAP_TOKEN"),
  )$data 
  
}

get_unclassified_meal_entries <- function(){
  col_types = readr::cols(
    .default = readr::col_character(),
    record_id = readr::col_double(),
  )
  REDCapR::redcap_read(records = NULL,
                       col_types=col_types,
                       verbose = TRUE,batch_size = 1000,
                       redcap_uri = Sys.getenv("DIETDATA_REDCAP_URI"),
                       token = Sys.getenv("DIETDATA_REDCAP_TOKEN"),
  )$data  %>%
    tidyr::fill(record_id) %>% 
    dplyr::filter(!is.na(raw_food_id)) %>% 
    dplyr::select(record_id, meal_date, meal, raw_food_id, raw_food_serving_unit) %>%
    dplyr::distinct()
  
}

get_meal_entries_lacking_fndds_match <- function(){
  custom_foods_and_codes <- custom_food[!complete.cases(custom_food), ] %>%
    mutate(fndds_portion_weight_g=as.numeric(fndds_portion_weight_g))%>%
    select(raw_food_id, fndds_food_code, raw_food_serving_unit, raw_to_fndds_unit_matcher, fndds_portion_description, fndds_portion_weight_g) %>%
    distinct() %>%
    left_join(fndds_paw %>% select(fndds_food_code, fndds_main_food_description) %>% distinct(), by="fndds_food_code")
  
  # in some cases (311 in the initial dataset) it appears that the unit table contained fndds codes such as 12350210 that are no longer in use.
  # these will end up with NA's in the fndds_portion_weight_g column and fndds_main_food_description column.  For these, we remove the fndds food code
  # to prompt the user to use a valid one
  custom_foods_and_codes <- custom_foods_and_codes %>% 
    dplyr::mutate(fndds_food_code = ifelse(is.na(fndds_main_food_description) & is.na(fndds_portion_weight_g), NA,fndds_food_code )) %>% 
    dplyr::mutate(fndds_portion_description = ifelse(is.na(fndds_main_food_description) & is.na(fndds_portion_weight_g), NA, fndds_portion_description )) %>% 
    dplyr::mutate(raw_to_fndds_unit_matcher = ifelse(is.na(fndds_main_food_description) & is.na(fndds_portion_weight_g), NA, raw_to_fndds_unit_matcher ))
  
  #do a 2 part join -- first (above) to match the food code with the name (above), then to match up the entries with portions
  #otherwise, we have foods/codes matched but no description due to a missing portion.
  df <- full_join(
    custom_foods_and_codes,
    fndds_paw %>% select(fndds_food_code,  fndds_main_food_description, fndds_portion_description, fndds_portion_weight_g),
    by=c("fndds_food_code", "fndds_main_food_description", "fndds_portion_description", "fndds_portion_weight_g")
  ) %>%
    distinct() %>%
    filter(if_any(everything(), is.na)) %>%
    mutate(food_code_desc = paste(fndds_food_code, fndds_main_food_description))
  df
  
}


save_new_unit_entries_to_redcap <- function(raw_food_id,raw_food_serving_unit, fndds_food_code, raw_to_fndds_unit_matcher,  fndds_portion_description, fndds_portion_weight_g){
  # TODO: implement actual call to new unites redcap
  argg <- c(as.list(environment()))
  if (FALSE) print(argg) # for debugging
  new_entry <- data.frame(
    "raw_food_id" = raw_food_id,
    "raw_food_serving_unit" = raw_food_serving_unit,
    "fndds_food_code" = fndds_food_code,
    "raw_to_fndds_unit_matcher"=raw_to_fndds_unit_matcher,
    "fndds_portion_description" = fndds_portion_description,
    "fndds_portion_weight_g" = fndds_portion_weight_g,
    "unit_table_complete" = 2)
  old_entry <- custom_food %>% 
    filter(raw_food_id == new_entry$raw_food_id[1]) %>% 
    filter(raw_food_serving_unit == new_entry$raw_food_serving_unit[1])
  if (FALSE){
    print("old entry")
    print(old_entry)
  }
  if (nrow(old_entry) > 1){
    print("Warning: fix unit table in redcap manually; multiple entries found with the same raw food id and raw food serving unit!")
  } else if (nrow(old_entry) == 0){
    print("adding entirely new food entry")
    new_entry$record_id = max(custom_food$record_id) + 1
  } else{
    new_entry$record_id = old_entry$record_id
  }
  REDCapR::redcap_write_oneshot(new_entry,
                                redcap_uri = Sys.getenv("UNITTABLE_REDCAP_URI"),
                                token = Sys.getenv("UNITTABLE_REDCAP_TOKEN"),
  )
  print("New food/foodcode/portion  saved to redcap:")
  print(new_entry)
}

mod_matchFNDDS_ui <- function(id) {
  DT::DTOutput(NS(id, "matchtable"))
}

mod_matchFNDDS_foodentry_ui <- function(id, df) {
  
  fluidRow(
    column(width=2,
           shiny::selectizeInput(NS(id, "raw_food_id"),
                                 choices=NULL,multiple = FALSE,
                                 label = "raw_food_id",   options = list(create = TRUE)),
    ),
    column(width=2,
           shiny::selectizeInput(NS(id, "raw_food_serving_unit"),
                                 choices=NULL,multiple = FALSE,
                                 label = "raw_food_serving_unit",   options = list(create = TRUE)),
    ),
    column(width=2,
           shiny::selectizeInput(NS(id, "food_code_desc"),multiple = FALSE,
                                 choices=NULL,
                                 label = "FNDDS Code and description")
    ),
    # these are select not selectize,as we generally don't want these manually changed.
    column(width=2,
           shiny::selectInput(NS(id, "fndds_food_code"),multiple = FALSE,
                              choices=NULL,selectize = FALSE,
                              label = "FNDDS Food Code")
    ),
    column(width=2,
           shiny::selectInput(NS(id, "fndds_main_food_description"),multiple = FALSE,
                              choices=NULL,selectize = FALSE,
                              label = "FNDDS Description")
    ),
  )
}
mod_matchFNDDS_portionentry_ui <- function(id, df) {
  fluidRow(
    column(width=2,
           shiny::numericInput(NS(id, "raw_to_fndds_unit_matcher"),min = 0, max = 1000, value = 0,
                               label = "Food NSC Units")
    ),
    column(1, "equals"),
    column(width=2,
           
           shiny::selectizeInput(NS(id, "fndds_portion_description"),multiple = FALSE,
                                 choices=NULL,
                                 label = "Portion Size",   options = list(create = TRUE))),
    column(width=2,
           shiny::numericInput(NS(id, "fndds_portion_weight_g"),min = 0, max = 1000, value = 0,
                               label = "portion weight (g)")),
    column(width=2, p(strong("Total Grams")), shiny::textOutput(NS(id, "calculated_grams"))),
    
  )
}
mod_matchFNDDS_submitter_ui <- function(id) {
  actionButton(
    inputId = NS(id, "submit_to_redcap"),
    label = "Write unit entry to REDcap")
}

mod_matchFNDDS_server <- function(id, df) {
  moduleServer(id, function(input, output, session) {
    this_food_code <- reactive({
      print(input$fndds_food_code)
      ifelse(is.null(input$fndds_food_code), NA , input$fndds_food_code)
    })
    reference_subset <- reactive({
      ref <- df %>% filter(fndds_food_code == this_food_code())
      print(ref)
      ref
    })
    output$matchtable <-   DT::renderDataTable(df, selection="single")
    observe({
      # this is the clicked selection
      x <- input$matchtable_rows_selected
      if(is.null(x)){
        x = 0
      }
      thisentry = df[x, ]
      # this is the reference database (fndds + custom) subsetted to listed criteria
      #reference_subset <- df 
      # if food code present or entered
      #if (x > 0){
      #  if(!is.na(thisentry$fndds_food_code)) reference_subset <- reference_subset %>% filter(fndds_food_code == thisentry$fndds_food_code)
      #  if(!is.na(thisentry$fndds_main_food_description)) reference_subset <- reference_subset %>% filter(fndds_main_food_description == thisentry$fndds_main_food_description)
      #}
      
      updateSelectizeInput(
        session, "raw_food_id",
        choices =unique(df$raw_food_id),
        selected =  thisentry$raw_food_id, server=TRUE)
      updateSelectizeInput(
        session, "raw_food_serving_unit",
        choices =unique(df$raw_food_serving_unit),
        selected =  thisentry$raw_food_serving_unit, server=TRUE)
      updateSelectizeInput(
        session, "food_code_desc",
        choices =unique(df$food_code_desc),
        selected =  thisentry$food_code_desc, server=TRUE)
      updateSelectizeInput(
        session, "fndds_main_food_description",
        choices =unique(df$fndds_main_food_description),
        selected =  thisentry$fndds_main_food_description, server=TRUE)
      updateNumericInput(
        session, "fndds_portion_weight_g",
        value =  thisentry$fndds_portion_weight_g)
    })
    
    observeEvent(input$food_code_desc, {
      if(!is.null(input$food_code_desc) & input$food_code_desc != ""){
        newdf <- df[df$food_code_desc == input$food_code_desc, ]
        #output$main_food_description <- renderText(newdf[1, "main_food_description"])
        #output$food_code <- renderText()
        updateSelectInput(
          session, "fndds_food_code",
          choices = newdf[1, "fndds_food_code"], #unique(df$food_code),
          selected =  newdf[1, "fndds_food_code"],
        )
        shinyjs::disable("fndds_food_code") 
        updateSelectInput(
          session, "fndds_main_food_description",
          choices =newdf[1, "fndds_main_food_description"], #unique(df$main_food_description),
          selected =  newdf[1, "fndds_main_food_description"],
        )
        shinyjs::disable("fndds_main_food_description")
        updateSelectizeInput(
          session, "fndds_portion_description",
          choices =unique(newdf$fndds_portion_description),
          selected =  newdf$fndds_portion_description, server=TRUE)
      } else{
        shinyjs::enable("fndds_food_code") 
        shinyjs::enable("fndds_main_food_description") 
        output$fndds_main_food_description <- renderText(NA)
        output$fndds_food_code <- renderText(NA)
        
      }
    })
    observeEvent(input$fndds_portion_description, {
      if(!is.null(input$fndds_portion_description) & input$fndds_portion_description != "" & input$food_code_desc != ""){
        newdf <- df %>% 
          filter(food_code_desc == input$food_code_desc) %>% 
          filter(!is.na(food_code_desc)) %>%
          filter(!is.na(fndds_portion_description)) %>% 
          filter(fndds_portion_description == input$fndds_portion_description)
        shiny::updateNumericInput(session, inputId = "fndds_portion_weight_g", value=newdf[1, "fndds_portion_weight_g"])
      } else{
        shiny::updateNumericInput(session, inputId = "fndds_portion_weight_g", value=NA)
      }
    })
    observeEvent(input$fndds_portion_weight_g, {
      if(!is.null(input$fndds_portion_weight_g)){
        output$calculated_grams <- renderText(input$fndds_portion_weight_g * input$raw_to_fndds_unit_matcher)
      }
    })
    observeEvent(input$submit_to_redcap, {
      
      save_new_unit_entries_to_redcap(
        raw_food_id=input$raw_food_id, 
        raw_food_serving_unit=input$raw_food_serving_unit,
        fndds_food_code=input$fndds_food_code,
        raw_to_fndds_unit_matcher=input$raw_to_fndds_unit_matcher,
        fndds_portion_description=input$fndds_portion_description, 
        fndds_portion_weight_g=input$fndds_portion_weight_g)
      
      session$reload()
    })
  })
}
mod_matchFNDDS_demo <- function() {
  unit_table_data <- get_redcap_unit_table()
  custom_food <<- dplyr::bind_rows(get_unclassified_meal_entries() ,
                                   unit_table_data)
  incomplete_data <- get_meal_entries_lacking_fndds_match()
  ui <- fluidPage(
    # make sure this is enable in the ui, not in the script itself!
    shinyjs::useShinyjs(),
    mod_matchFNDDS_foodentry_ui("x", incomplete_data),
    mod_matchFNDDS_portionentry_ui("x", incomplete_data),
    mod_matchFNDDS_submitter_ui("x"),
    mod_matchFNDDS_ui("x")
  )
  server <- function(input, output, session) {
    mod_matchFNDDS_server("x", df = incomplete_data)
  }
  shinyApp(ui, server)
  
}
#mod_matchFNDDS_demo() 

