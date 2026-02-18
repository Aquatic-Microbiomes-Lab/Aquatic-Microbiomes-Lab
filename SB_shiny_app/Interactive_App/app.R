library(shiny)
library(bslib)
library(tidyverse)
library(reshape2)
library(plotly)
library(DT)

# Extra commands for manipulating data
# df <- readr::read_csv("/Users/Katelyn/Desktop/SB_2023_Nutrients.csv") # date MUST be in yyyy-mm-dd
# df <- df %>%
#   pivot_longer(cols = 3:12, names_to = "Env", values_to = "Concentration") # make long
# df <- tidyr::separate(data = df, col = Env, sep = "_", into = c("Env", "Unit"))

# Prepare dataframes ----
df <- readr::read_csv("https://raw.githubusercontent.com/katelynbrown/shiny_test/refs/heads/main/SB_2024_all_test.csv") # date MUST be in yyyy-mm-dd
df <- df %>%
  pivot_longer(cols = 4:26, names_to = "Env", values_to = "Concentration") # make long
df <- tidyr::separate(data = df, col = Env, sep = "_", into = c("Env", "Unit"))

df$Date <- as.Date(df$Date, format = "%Y-%m-%d") # dates are tricky in R
min_date = min(df$Date)
max_date = max(df$Date)

# Define UI ----
ui <- fluidPage(theme = bs_theme(preset = "lumen"),
                headerPanel("Interactive Sandusky Bay Data Viewer"),
                sidebarLayout(
                  sidebarPanel(
                    # check box of all unique environmental (numeric) parameters
                    checkboxGroupInput("Env", "Environmental variable",
                                       choices = unique(df$Env), 
                                       selected = NULL
                    ),
                    hr(style = "border-top: 1px solid #000000;"),
                    # check box of all unique sampling site names
                    checkboxGroupInput("Site", "Sampling site",
                                       choices = unique(df$Site), 
                                       selected = NULL
                    ), 
                    hr(style = "border-top: 1px solid #000000;"),
                    # date range box, but only allows for selection within dates that have been sampled
                    dateRangeInput("range", "Select dates",
                                   start = min_date,
                                   end = max_date
                    ), 
                    hr(style = "border-top: 1px solid #000000;"),
                    # choices for different colors
                    radioButtons("location", "Color by:",
                                 choices = c("Sampling Site", "Inner/Outer Bay"), 
                                 selected = "Sampling Site"
                    ),
                    hr(style = "border-top: 1px solid #000000;"),
                    # checkbox to plot ug/L (many env parameters) and umol/L (nutrients) values together
                    checkboxInput("plot_both", "Check to plot data together", value = FALSE, width = NULL
                    ),
                    hr(style = "border-top: 1px solid #000000;"),
                    # button to create plot
                    actionButton(
                      inputId = "run_plot",
                      label = "Create plot"
                    ),
                    # button to download csv
                    downloadButton(
                      outputId = "data_download",
                      label = "Download .csv file of selected data"
                    ) 
                  ),
                  mainPanel(
                    plotlyOutput("plot1", width = 900), # nutrients (umol) only OR umol+ug/L
                    plotlyOutput("plot2", width = 900), # ug/L only OR blank
                    plotlyOutput("plot3", width = 900), # TN:TP ratio
                    plotlyOutput("plot4", width = 900), # pH
                    plotlyOutput("plot5", width = 900), # temperature (C)
                    plotlyOutput("plot6", width = 900), # DO (mg/L)
                    plotlyOutput("plot7", width = 900), # conductivity (uS/cm2)
                    dataTableOutput("table", width = 900)
                    # each desired output requires one of these that are referenced below, they will not render without these
                  )
                )
)

# Define server logic ----
server <- function(input, output) {
  all_filtered <- eventReactive(input$run_plot, {
    df %>%
      filter(Site %in% input$Site, Env %in% input$Env, Date>=input$range[1] & Date<=input$range[2])
    # when the create plot button is hit (input$run_plot), site, parameter, and date are filtered according to selections
    # specifically this is for the table to make all filtered data appear in the table
  })
  umol_filtered <- eventReactive(input$run_plot, {
    df %>%
      filter(Site %in% input$Site, Env %in% input$Env, Date>=input$range[1] & Date<=input$range[2]) %>%
      group_by(Unit) %>%
      filter(Unit == "umol/L")
    # when the create plot button is hit (input$run_plot), site, parameter, and date are filtered according to selections
    # includes extra filtering by unit for a plot limited to umol/L
  })
  ratio_filtered <- eventReactive(input$run_plot, {
    df %>%
      filter(Site %in% input$Site, Env %in% input$Env, Date>=input$range[1] & Date<=input$range[2]) %>%
      group_by(Unit) %>%
      filter(Unit == "ratio")
    # when the create plot button is hit (input$run_plot), site, parameter, and date are filtered according to selections
    # includes extra filtering by unit for a plot limited to TN:TP
  })
  ug_filtered <- eventReactive(input$run_plot, {
    df %>%
      filter(Site %in% input$Site, Env %in% input$Env, Date>=input$range[1] & Date<=input$range[2]) %>%
      group_by(Unit) %>%
      filter(Unit == "ug/L")
    # when the create plot button is hit (input$run_plot), site, parameter, and date are filtered according to selections
    # includes extra filtering by unit for a plot limited to ug/L
  })
  both_filtered <- eventReactive(input$run_plot, {
    df %>%
      filter(Site %in% input$Site, Env %in% input$Env, Date>=input$range[1] & Date<=input$range[2]) %>%
      group_by(Unit) %>%
      filter(Unit == "ug/L" | Unit == "umol/L")
    # when the create plot button is hit (input$run_plot), site, parameter, and date are filtered according to selections
    # includes extra filtering by unit for ug/L and umol/L combined plot
  })
  pH_filtered <- eventReactive(input$run_plot, {
    df %>%
      filter(Site %in% input$Site, Env %in% input$Env, Date>=input$range[1] & Date<=input$range[2]) %>%
      group_by(Unit) %>%
      filter(Unit == "ph")
    # when the create plot button is hit (input$run_plot), site, parameter, and date are filtered according to selections
  })
  temp_filtered <- eventReactive(input$run_plot, {
    df %>%
      filter(Site %in% input$Site, Env %in% input$Env, Date>=input$range[1] & Date<=input$range[2]) %>%
      group_by(Unit) %>%
      filter(Unit == "C")
    # when the create plot button is hit (input$run_plot), site, parameter, and date are filtered according to selections
    # includes extra filtering by unit for ug/L and umol/L combined plot
  })
  do_filtered <- eventReactive(input$run_plot, {
    df %>%
      filter(Site %in% input$Site, Env %in% input$Env, Date>=input$range[1] & Date<=input$range[2]) %>%
      group_by(Unit) %>%
      filter(Unit == "mg/L")
    # when the create plot button is hit (input$run_plot), site, parameter, and date are filtered according to selections
    # includes extra filtering by unit for ug/L and umol/L combined plot
  })
  cond_filtered <- eventReactive(input$run_plot, {
    df %>%
      filter(Site %in% input$Site, Env %in% input$Env, Date>=input$range[1] & Date<=input$range[2]) %>%
      group_by(Unit) %>%
      filter(Unit == "uS/cm2")
    # when the create plot button is hit (input$run_plot), site, parameter, and date are filtered according to selections
    # includes extra filtering by unit for ug/L and umol/L combined plot
  })
  
  # large if else statements below on panels 1 & 2 to account for different plots   
  # first output plot - nutrients or nutrients + other parameters
  if (!is.null(both_filtered)){ # if the dataframe is not empty
    output$plot1 <- renderPlotly({ 
      if (input$plot_both == TRUE) { # if checked yes to plotting data together this plot will happen in panel 1
        validate(need(nrow(both_filtered())>0,'Data not selected')) # message instead of error for empty dataframe
        ggplotly( # makes interactive
          ggplot(both_filtered(), aes(x = Date, y = Concentration, shape = Env, linetype = Unit)) +
            geom_point() +
            geom_line() +
            ggtitle("Nutrients") +
            ylab("Concentration (µmol/L or µg/L)") +
            theme(panel.background = element_rect(fill = "white"),
                  panel.grid = element_blank(),
                  panel.grid.major = element_line(color = "light grey"),                             
                  panel.border = element_rect(color = "black", fill = NA, size = 1),
                  plot.title = element_text(size = 17),
                  text = element_text(color = "black"),
                  axis.text = element_text(size = 12),
                  axis.title = element_text(size = 13),
                  legend.text = element_text(size = 12)) +
            # allows input of "color by:" choice to work
            list (if (input$location == "Sampling Site") aes(color = Site),
                  if (input$location == "Inner/Outer Bay") aes(color = Location)))
      }
      else { # if checked no to plot together, this will output nutrient data alone in panel 1
        if (!is.null(umol_filtered)){
          validate(need(nrow(umol_filtered())>0,'Data not selected')) # message instead of error for empty dataframe
          ggplotly( # makes interactive
            ggplot(umol_filtered(), aes(x = Date, y = Concentration, shape = Env, linetype = Site)) +
              geom_point() +
              geom_line() +
              ggtitle("Nutrients") +
              ylab("Concentration (µmol/L)") +
              theme(panel.background = element_rect(fill = "white"),
                    panel.grid = element_blank(),
                    panel.grid.major = element_line(color = "light grey"),                             
                    panel.border = element_rect(color = "black", fill = NA, size = 1),
                    plot.title = element_text(size = 17),
                    text = element_text(color = "black"),
                    axis.text = element_text(size = 12),
                    axis.title = element_text(size = 13),
                    legend.text = element_text(size = 12)) +
              # allows input of "color by:" choice to work
              list (if (input$location == "Sampling Site") aes(color = Site),
                    if (input$location == "Inner/Outer Bay") aes(color = Location)))
        }
      }
    })
  }
  # second output plot - other parameters or blank
  if (!is.null(ratio_filtered)){
    output$plot2 <- renderPlotly({
      if (input$plot_both == TRUE) { # if checked to plot together, makes blank plot in panel 2
        validate(need(nrow(ug_filtered())>0,'Data not selected'))
        ggplotly(
          ggplot(ug_filtered(), aes(x = Date, y = Concentration)) + # idk how to make the panel go away so i got rid of everything on the plot instead
            theme(panel.background = element_rect(fill = "white"),
                  panel.grid = element_blank(),                             
                  panel.border = element_blank(),
                  axis.ticks = element_blank(),
                  text = element_blank()))
      }
      else { # if not checked to plot together, plots parameters in ug/L in panel 2
        if (!is.null(ug_filtered)){
          validate(need(nrow(ug_filtered())>0,'Data not selected'))
          ggplotly(
            ggplot(ug_filtered(), aes(x = Date, y = Concentration, color = Site, shape = Env, linetype = Site)) +
              geom_point() +
              geom_line() +
              ggtitle("Measurements in µg/L") +
              ylab("Concentration (ug/L)") +
              theme(panel.background = element_rect(fill = "white"),
                    panel.grid = element_blank(),
                    panel.grid.major = element_line(color = "light grey"),                             
                    panel.border = element_rect(color = "black", fill = NA, size = 1),
                    plot.title = element_text(size = 17),
                    text = element_text(color = "black"),
                    axis.text = element_text(size = 12),
                    axis.title = element_text(size = 13),
                    legend.text = element_text(size = 12)) +
              list (if (input$location == "Sampling Site") aes(color = Site),
                    if (input$location == "Inner/Outer Bay") aes(color = Location)))  
        }
      }
    })
  }
  # third output plot - TN:TP, does not change
  if (!is.null(ratio_filtered)){
    output$plot3 <- renderPlotly({
      validate(need(nrow(ratio_filtered())>0,'TN:TP not selected'))
      ggplotly(
        ggplot(ratio_filtered(), aes(x = Date, y = Concentration, color = Site, shape = Env, linetype = Site)) +
          geom_point() +
          geom_line() +
          ggtitle("TN:TP") +
          ylab("TN:TP") +
          theme(panel.background = element_rect(fill = "white"),
                panel.grid = element_blank(),
                panel.grid.major = element_line(color = "light grey"),                             
                panel.border = element_rect(color = "black", fill = NA, size = 1),
                plot.title = element_text(size = 17),
                text = element_text(color = "black"),
                axis.text = element_text(size = 12),
                axis.title = element_text(size = 13),
                legend.text = element_text(size = 12)) +
          list (if (input$location == "Sampling Site") aes(color = Site),
                if (input$location == "Inner/Outer Bay") aes(color = Location)))
    })
  }
  # fourth output plot - pH, does not change
  if (!is.null(pH_filtered)){
    output$plot4 <- renderPlotly({
      validate(need(nrow(pH_filtered())>0,'pH not selected'))
      ggplotly(
        ggplot(pH_filtered(), aes(x = Date, y = Concentration, color = Site, shape = Env, linetype = Site)) +
          geom_point() +
          geom_line() +
          ggtitle("pH") +
          ylab("pH") +
          theme(panel.background = element_rect(fill = "white"),
                panel.grid = element_blank(),
                panel.grid.major = element_line(color = "light grey"),                             
                panel.border = element_rect(color = "black", fill = NA, size = 1),
                plot.title = element_text(size = 17),
                text = element_text(color = "black"),
                axis.text = element_text(size = 12),
                axis.title = element_text(size = 13),
                legend.text = element_text(size = 12)) +
          list (if (input$location == "Sampling Site") aes(color = Site),
                if (input$location == "Inner/Outer Bay") aes(color = Location)))
    })
  }
  # fifth output plot - temperature, does not change
  if (!is.null(temp_filtered)){
    output$plot5 <- renderPlotly({
      validate(need(nrow(temp_filtered())>0,'Temperature not selected'))
      ggplotly(
        ggplot(temp_filtered(), aes(x = Date, y = Concentration, color = Site, shape = Env, linetype = Site)) +
          geom_point() +
          geom_line() +
          ggtitle("Air/Water Temperature") +
          ylab("Temperature (ºC)") +
          theme(panel.background = element_rect(fill = "white"),
                panel.grid = element_blank(),
                panel.grid.major = element_line(color = "light grey"),                             
                panel.border = element_rect(color = "black", fill = NA, size = 1),
                plot.title = element_text(size = 17),
                text = element_text(color = "black"),
                axis.text = element_text(size = 12),
                axis.title = element_text(size = 13),
                legend.text = element_text(size = 12)) +
          list (if (input$location == "Sampling Site") aes(color = Site),
                if (input$location == "Inner/Outer Bay") aes(color = Location)))
    })
  }
  # sixth output plot - dissolved oxygen, does not change
  if (!is.null(do_filtered)){
    output$plot6 <- renderPlotly({
      validate(need(nrow(do_filtered())>0,'Dissolved oxygen not selected'))
      ggplotly(
        ggplot(do_filtered(), aes(x = Date, y = Concentration, color = Site, shape = Env, linetype = Site)) +
          geom_point() +
          geom_line() +
          ggtitle("Dissolved Oxygen") +
          ylab("Concentration (mg/L)") +
          theme(panel.background = element_rect(fill = "white"),
                panel.grid = element_blank(),
                panel.grid.major = element_line(color = "light grey"),                             
                panel.border = element_rect(color = "black", fill = NA, size = 1),
                plot.title = element_text(size = 17),
                text = element_text(color = "black"),
                axis.text = element_text(size = 12),
                axis.title = element_text(size = 13),
                legend.text = element_text(size = 12)) +
          list (if (input$location == "Sampling Site") aes(color = Site),
                if (input$location == "Inner/Outer Bay") aes(color = Location)))
    })
  }
  # seventh output plot - conductivity, does not change
  if (!is.null(cond_filtered)){
    output$plot7 <- renderPlotly({
      validate(need(nrow(cond_filtered())>0,'Conductivity not selected'))
      ggplotly(
        ggplot(cond_filtered(), aes(x = Date, y = Concentration, color = Site, shape = Env, linetype = Site)) +
          geom_point() +
          geom_line() +
          ggtitle("Conductivity") +
          ylab("Conductivity (µS/cm2)") +
          theme(panel.background = element_rect(fill = "white"),
                panel.grid = element_blank(),
                panel.grid.major = element_line(color = "light grey"),                             
                panel.border = element_rect(color = "black", fill = NA, size = 1),
                plot.title = element_text(size = 17),
                text = element_text(color = "black"),
                axis.text = element_text(size = 12),
                axis.title = element_text(size = 13),
                legend.text = element_text(size = 12)) +
          list (if (input$location == "Sampling Site") aes(color = Site),
                if (input$location == "Inner/Outer Bay") aes(color = Location)))
    })
  }
  # displays a table with all filtered data
  output$table <- renderDataTable({
    all_filtered()
  })
  # function to download csv files directly
  output$data_download <- downloadHandler(
    filename = function() { 
      paste("sandusky-bay-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(all_filtered(), file)
    })
}

# Run the app ----
shinyApp(ui = ui, server = server)
# rsconnect::deployApp('/Users/Katelyn/Desktop/Rshiny test/App-1/app/', appName="SBDataViewer")