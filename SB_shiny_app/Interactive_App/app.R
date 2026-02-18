source("packages.R")

# Load and prepare data
df <- readr::read_csv(
  "https://raw.githubusercontent.com/Aquatic-Microbiomes-Lab/Aquatic-Microbiomes-Lab/refs/heads/main/Sandusky_Bay_Monitoring/SB_all_years_12.10.csv"
)

df <- df %>%
  pivot_longer(cols = 4:26, names_to = "Env", values_to = "Concentration") %>%
  tidyr::separate(col = Env, sep = "_", into = c("Env", "Unit"))

df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
min_date <- min(df$Date)
max_date <- max(df$Date)

# UI
ui <- fluidPage(
  theme = bs_theme(preset = "lumen"),
  headerPanel("Interactive Sandusky Bay Data Viewer"),
  
  card(
    height = 'auto', 
    full_screen = TRUE,
    card_image(
      src = "https://raw.githubusercontent.com/Aquatic-Microbiomes-Lab/Aquatic-Microbiomes-Lab/50e276a4f24119018af80cbefac597fa81b2f62d/Sandusky_Bay_Monitoring/SB%20Map/MapWithInset.png"
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("Env", "Environmental variable",
                         choices = unique(df$Env), selected = unique(df$Env)),
      hr(),
      checkboxGroupInput("Site", "Sampling site",
                         choices = unique(df$Site), selected = unique(df$Site)),
      hr(),
      actionButton("clear_all", "Deselect all sites and parameters"),
      dateRangeInput("range", "Select dates", start = min_date, end = max_date),
      hr(),
      radioButtons("location", "Color by:",
                   choices = c("Sampling Site", "Inner/Outer Bay"), selected = "Sampling Site"),
      hr(),
      checkboxInput("plot_both", "Check to plot data together", value = FALSE),
      hr(),
      actionButton("run_plot", "Create plot"),
      downloadButton("data_download", "Download .csv file of selected data")
    ),
    mainPanel(
      plotlyOutput("plot1", width = 900),
      plotlyOutput("plot2", width = 900),
      plotlyOutput("plot3", width = 900),
      plotlyOutput("plot4", width = 900),
      plotlyOutput("plot5", width = 900),
      plotlyOutput("plot6", width = 900),
      plotlyOutput("plot7", width = 900),
      DT::dataTableOutput("table", width = 900)
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Clear all inputs
  observeEvent(input$clear_all, {
    updateCheckboxGroupInput(session, "Env", selected = character(0))
    updateCheckboxGroupInput(session, "Site", selected = character(0))
    updateDateRangeInput(session, "range", start = min_date, end = max_date)
    updateCheckboxInput(session, "plot_both", value = FALSE)
  })
  
  # Reactive filtered datasets
  filter_data <- function(unit = NULL) {
    df_filtered <- df %>%
      filter(Site %in% input$Site, Env %in% input$Env,
             Date >= input$range[1], Date <= input$range[2])
    if (!is.null(unit)) df_filtered <- df_filtered %>% filter(Unit == unit)
    df_filtered
  }
  
  all_filtered <- eventReactive(input$run_plot, filter_data(), ignoreNULL = FALSE)
  umol_filtered <- eventReactive(input$run_plot, filter_data("umol/L"), ignoreNULL = FALSE)
  ug_filtered <- eventReactive(input$run_plot, filter_data("ug/L"), ignoreNULL = FALSE)
  ratio_filtered <- eventReactive(input$run_plot, filter_data("ratio"), ignoreNULL = FALSE)
  pH_filtered <- eventReactive(input$run_plot, filter_data("ph"), ignoreNULL = FALSE)
  temp_filtered <- eventReactive(input$run_plot, filter_data("C"), ignoreNULL = FALSE)
  do_filtered <- eventReactive(input$run_plot, filter_data("mg/L"), ignoreNULL = FALSE)
  cond_filtered <- eventReactive(input$run_plot, filter_data("uS/cm2"), ignoreNULL = FALSE)
  both_filtered <- eventReactive(input$run_plot, {
    df %>%
      filter(Site %in% input$Site, Env %in% input$Env,
             Date >= input$range[1], Date <= input$range[2]) %>%
      filter(Unit %in% c("ug/L", "umol/L"))
  }, ignoreNULL = FALSE)
  
  # Function to create plots
  plot_fun <- function(data, y_label, title) {
    validate(need(nrow(data) > 0, "Data not selected"))
    gg <- ggplot(data, aes(x = Date, y = Concentration, shape = Env, linetype = Site)) +
      geom_point() + geom_line() +
      ggtitle(title) + ylab(y_label) +
      theme_minimal()
    
    if (input$location == "Sampling Site") gg <- gg + aes(color = Site)
    else gg <- gg + aes(color = Location)
    
    ggplotly(gg)
  }
  
  output$plot1 <- renderPlotly({
    if (input$plot_both) plot_fun(both_filtered(), "Concentration (µmol/L or µg/L)", "Nutrients")
    else plot_fun(umol_filtered(), "Concentration (µmol/L)", "Nutrients")
  })
  
  output$plot2 <- renderPlotly({
    if (input$plot_both) {
      ggplotly(ggplot() + theme_void())
    } else plot_fun(ug_filtered(), "Concentration (µg/L)", "Measurements in µg/L")
  })
  
  output$plot3 <- renderPlotly({ plot_fun(ratio_filtered(), "TN:TP", "TN:TP") })
  output$plot4 <- renderPlotly({ plot_fun(pH_filtered(), "pH", "pH") })
  output$plot5 <- renderPlotly({ plot_fun(temp_filtered(), "Temperature (ºC)", "Air/Water Temperature") })
  output$plot6 <- renderPlotly({ plot_fun(do_filtered(), "Concentration (mg/L)", "Dissolved Oxygen") })
  output$plot7 <- renderPlotly({ plot_fun(cond_filtered(), "Conductivity (µS/cm2)", "Conductivity") })
  
  # Table
  output$table <- DT::renderDataTable({ all_filtered() })
  
  # Download
  output$data_download <- downloadHandler(
    filename = function() paste0("sandusky-bay-", Sys.Date(), ".csv"),
    content = function(file) write.csv(all_filtered(), file)
  )
}

# Run the app
shinyApp(ui = ui, server = server)
