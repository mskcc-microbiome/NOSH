mod_mrn_select_ui <- function(id) {
  selectInput(NS(id, 'mrn'), label = "mrn", choices = NULL)
}
mod_bymeal_checkbox_io <- function(id){
  checkboxInput(NS(id, 'bymeal'), label = "Split by Meal", value = FALSE)
}
mod_datacompleteness_ui <- function(id){
  verbatimTextOutput(NS(id, 'patient_completeness'))
}
mod_patientdash_ui <- function(id) {
  plotOutput(NS(id, 'summary'))
}
mod_patientdash_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    pt_data_full_merge <- reactive({
      merge_meals_and_units(dev_data = rv$current_redcap_diet_data, unittable = unittable, fndds_summary = fndds_summary)
    })
    observe({
      output$patient_completeness<- renderText(pt_data_full_merge()$status)
    })
    observe({
      updateSelectInput(
      session, "mrn",
      choices =unique(pt_data_full_merge()$df$mrn),
      selected =  pt_data_full_merge()$df$mrn[1])
    })
    
    get_plot_data <- reactive({
      tabulate_pt_nutrition(pt_data_full_merge()$df, mrn=input$mrn, nutrient_list=input$nutrients,
                            dt_start="1114-06-21", dt_end="2914-06-21") %>% 
        group_by(meal, meal_date, nutrient) %>% 
        summarize(daily_total=sum(consumed_value))
    })
    output$summary <- renderPlot({ 
      p <- ggplot2::ggplot(get_plot_data() , ggplot2::aes(x=meal_date, color=nutrient, fill=nutrient, y=daily_total)) + 
        ggplot2::geom_bar(stat="identity") + #ggplot2::geom_point() +
        ggplot2::theme_bw() 
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle=45, hjust=1))
      if (input$bymeal){
        p <- p + ggplot2::facet_grid(meal~.,space = "free")
      }
        p
    }
    )
  })
}


mod_patientdash_demo <- function() {
  ui <- fluidPage(
    mod_datacompleteness_ui("patient"),
    mod_mrn_select_ui("patient"),
    mod_bymeal_checkbox_io("patient"),
    mod_nutrient_select_ui("patient"),
    mod_patientdash_ui("patient")
  )
  server <- function(input, output, session) {
    init_data <- get_meal_entries()
    rv <- reactiveValues(current_redcap_diet_data=init_data$df,
                         patients=init_data$patients)
    mod_patientdash_server("patient", rv)
  }
  shinyApp(ui, server)
  
}
# mod_patientdash_demo()
