mod_nutrient_select_ui <- function(id) {
  selectInput(NS(id, 'nutrients'), label = "Select Nutrient to Display", choices = colnames(fndds_summary)[5:ncol(fndds_summary)], selected = "protein_g", multiple = TRUE)
}
mod_dashboard_ui <- function(id) {
  plotOutput(NS(id, 'summary'))
}


mod_summary_table_ui <- function(id) {
  # NS: name space (module shiny )
  tableOutput(NS(id, 'summary_table'))
}

mod_meal_histogram_ui <- function(id) {
  plotOutput(NS(id, 'meal_histogram'))
}



mod_dashboard_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    pt_data_full_merge <- merge_meals_and_units(dev_data = dev_data, unittable = unittable, fndds_summary = fndds_summary)
    
    # reactive function to send the user input to ggplot
    get_plot_data <- reactive({
      tabulate_pt_nutrition(pt_data_full_merge, mrn=3, nutrient_list=input$nutrients,
                            dt_start="1914-06-21", dt_end="1914-06-21") %>% 
        dplyr::group_by(meal, date_intake, nutrient) %>% 
        dplyr::summarize(daily_total=sum(consumed_value))
    })
    output$summary <- renderPlot({ 
      ggplot2::ggplot(get_plot_data() , ggplot2::aes(x=interaction(date_intake, meal), color=nutrient, y=daily_total)) + ggplot2::geom_point()
    }
    )

    
    output$summary_table <- renderTable({ 
      tibble(title = 'Number of patients', number = pt_data_full_merge %>% distinct(mrn) %>% nrow)
    }
    )
    
    output$meal_histogram <- renderPlot({ 
      
      ggplot(pt_data_full_merge , aes(x=date_intake)) + geom_bar()
    }
    )
    

#    output$user <- renderText(paste("Session$user =", session$user, "; Sys.getenv(USER) =", Sys.getenv("USER"),  "; Sys.info()[user] =", Sys.info()["user"],  "; Sys.getenv(LOGNAME) =", Sys.getenv("LOGNAME")) )

  })
}


mod_dashboard_demo <- function() {

  ui <- fluidPage(mod_nutrient_select_ui("taco"), mod_dashboard_ui("taco"), mod_summary_table_ui("taco"), mod_meal_histogram_ui("taco"))

  server <- function(input, output, session) {
    mod_dashboard_server("taco")
  }
  shinyApp(ui, server)
  
}












#pt_data_full_merge %>% summary



#mod_dashboard_demo()

