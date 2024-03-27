mod_nutrient_select_ui <- function(id) {
  selectInput(NS(id, 'nutrients'), label = "Select Nutrient to Display", choices = colnames(fndds_summary)[5:ncol(fndds_summary)], selected = "protein_g", multiple = TRUE)
}


mod_mrn_select_ui <- function(id) {
  selectInput(NS(id, 'mrn'), label = "mrn", choices = unique(dev_data$mrn))
}
mod_patientdash_ui <- function(id) {
  plotOutput(NS(id, 'summary'))
}
mod_patientdash_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    pt_data_full_merge <- merge_meals_and_units(dev_data = dev_data, unittable = unittable, fndds_summary = fndds_summary)
    get_plot_data <- reactive({
      tabulate_pt_nutrition(pt_data_full_merge, mrn=input$mrn, nutrient_list=input$nutrients,
                            dt_start="1114-06-21", dt_end="2914-06-21") %>% 
        group_by(meal, date_intake, nutrient) %>% 
        summarize(daily_total=sum(consumed_value))
    })
    output$summary <- renderPlot({ 
      ggplot(get_plot_data() , aes(x=interaction(date_intake, meal), color=nutrient, y=daily_total)) + geom_point()
    }
    )
  })
}


mod_patientdash_demo <- function() {
  ui <- fluidPage(mod_mrn_select_ui("patient"), mod_nutrient_select_ui("patient"), mod_dashboard_ui("patient"))
  server <- function(input, output, session) {
    mod_dashboard_server("patient")
  }
  shinyApp(ui, server)
  
}
mod_patientdash_demo()
