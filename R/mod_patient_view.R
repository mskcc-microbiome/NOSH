mod_mrn_select_ui <- function(id) {
  selectInput(NS(id, 'mrn'), label = "mrn", choices = NULL)
}
mod_patientdash_ui <- function(id) {
  plotOutput(NS(id, 'summary'))
}
mod_patientdash_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    pt_data_full_merge <- merge_meals_and_units(dev_data = get_meal_entries(), unittable = unittable, fndds_summary = fndds_summary)
    print(pt_data_full_merge)
    updateSelectInput(
      session, "mrn",
      choices =unique(pt_data_full_merge$mrn),
      selected =  pt_data_full_merge$mrn[1])
    
    get_plot_data <- reactive({
      tabulate_pt_nutrition(pt_data_full_merge, mrn=input$mrn, nutrient_list=input$nutrients,
                            dt_start="1114-06-21", dt_end="2914-06-21") %>% 
        group_by(meal, meal_date, nutrient) %>% 
        summarize(daily_total=sum(consumed_value))
    })
    output$summary <- renderPlot({ 
      ggplot2::ggplot(get_plot_data() , ggplot2::aes(x=interaction(meal_date, meal), color=nutrient, y=daily_total)) + 
        ggplot2::geom_point() +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle=45, hjust=1))
    }
    )
  })
}


mod_patientdash_demo <- function() {
  ui <- fluidPage(
    mod_mrn_select_ui("patient"),
    mod_nutrient_select_ui("patient"),
    mod_patientdash_ui("patient")
  )
  server <- function(input, output, session) {
    mod_patientdash_server("patient")
  }
  shinyApp(ui, server)
  
}
# mod_patientdash_demo()
