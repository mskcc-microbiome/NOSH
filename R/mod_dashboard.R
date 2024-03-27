mod_nutrient_select_ui <- function(id) {
  selectInput(NS(id, 'nutrients'), label = "Select Nutrient to Display", choices = colnames(fndds_summary)[5:ncol(fndds_summary)], selected = "protein_g", multiple = TRUE)
}
mod_dashboard_ui <- function(id) {
  plotOutput(NS(id, 'summary'))
}
mod_user_ui <- function(id) {
  verbatimTextOutput(NS(id, 'user'))
}
mod_dashboard_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    pt_data_full_merge <- merge_meals_and_units(dev_data = dev_data, unittable = unittable, fndds_summary = fndds_summary)
    get_plot_data <- reactive({
      tabulate_pt_nutrition(pt_data_full_merge, mrn=3, nutrient_list=input$nutrients,
                            dt_start="1914-06-21", dt_end="1914-06-21") %>% 
        group_by(meal, date_intake, nutrient) %>% 
        summarize(daily_total=sum(consumed_value))
    })
    output$summary <- renderPlot({ 
      ggplot2::ggplot(get_plot_data() , ggplot2::aes(x=interaction(date_intake, meal), color=nutrient, y=daily_total)) + ggplot2::geom_point()
    }
    )
    output$user <- renderText(paste("Session$user =", session$user, "; Sys.getenv(USER) =", Sys.getenv("USER"),  "; Sys.info()[user] =", Sys.info()["user"],  "; Sys.getenv(LOGNAME) =", Sys.getenv("LOGNAME")) )
  })
}


mod_dashboard_demo <- function() {
  ui <- fluidPage(
    mod_nutrient_select_ui("taco"), 
    mod_dashboard_ui("taco"), 
    mod_user_ui("taco"),
  )
  server <- function(input, output, session) {
    mod_dashboard_server("taco")
  }
  shinyApp(ui, server)
  
}
#mod_dashboard_demo()
