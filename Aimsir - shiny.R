library(shiny)
library(tidyverse)

# Load and prep data (replace with your actual file path)
AllDunsany <- read.csv("dly1375.csv", skip = 23)
AllDunsany$date <- as.Date(AllDunsany$date, format = "%d-%b-%Y")
AllDunsany <- AllDunsany %>%
  mutate(month = month(date), year = year(date), day = day(date),
         wdspkm = wdsp * 1.85200,
         hmkm = hm * 1.85200,
         hgkm = hg * 1.85200)

time_labeller <- function(m, y) {
  month_names <- c("January", "February", "March", "April", "May", "June",
                   "July", "August", "September", "October", "November", "December")
  month_text <- if(length(m) == 1) month_names[m] else paste0(month_names[min(m)], "-", month_names[max(m)])
  year_text <- if(length(y) == 1) as.character(y) else paste0(min(y), "-", max(y))
  paste(month_text, year_text)
}

ui <- fluidPage(
  titlePanel("Irish Weather Data Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("location", "Location:", choices = "Dunsany"),
      checkboxGroupInput("variables", "Weather Parameters:",
                         choices = c("Max Temperature (°C)" = "maxtp",
                                     "Min Temperature (°C)" = "mintp",
                                     "Grass Min Temperature (°C)" = "gmin",
                                     "Soil Temperature (°C)" = "soil",
                                     "Rainfall (mm)" = "rain",
                                     "Pressure (hpa)" = "cbl",
                                     "Wind Speed (km/h)" = "wdspkm",
                                     "Max Wind Speed (km/h)" = "hmkm",
                                     "Wind Direction (°)" = "ddhm",
                                     "Highest Gust (km/h)" = "hgkm",
                                     "Evapotranspiration (mm)" = "pe",
                                     "Evaporation (mm)" = "evap",
                                     "SMD Well Drained (mm)" = "smd_wd",
                                     "SMD Moderately Drained (mm)" = "smd_md",
                                     "SMD Poorly Drained (mm)" = "smd_pd",
                                     "Solar Radiation (J/cm²)" = "glorad"),
                         selected = c("maxtp", "rain")),
      selectInput("months", "Month(s):", 
                  choices = setNames(1:12, month.name),
                  selected = 1, multiple = TRUE),
      sliderInput("years", "Year Range:", 
                  min = 2006, max = 2026, value = c(2006, 2026), sep = ""),
      radioButtons("time_scale", "Time Scale:",
                   choices = c("Daily" = "day", 
                               "Monthly Average" = "month",
                               "Yearly Average" = "year")),
      radioButtons("graph_type", "Graph Type:",
                   choices = c("Bar Chart" = "bar", "Line Graph" = "line"))
    ),
    mainPanel(plotOutput("weather_plot", height = "600px"))
  )
)

server <- function(input, output) {
  output$weather_plot <- renderPlot({
    req(input$variables, input$months, input$years)
    
    m <- as.numeric(input$months)
    y <- seq(input$years[1], input$years[2])
    
    # Variable labels
    var_labels <- c(
      maxtp = "Max Temperature (°C)", mintp = "Min Temperature (°C)",
      gmin = "Grass Min Temperature (°C)", soil = "Soil Temperature (°C)",
      rain = "Rainfall (mm)", cbl = "Pressure (hpa)",
      wdspkm = "Wind Speed (km/h)", hmkm = "Max Wind Speed (km/h)",
      ddhm = "Wind Direction (°)", hgkm = "Highest Gust (km/h)",
      pe = "Evapotranspiration (mm)", evap = "Evaporation (mm)",
      smd_wd = "SMD Well Drained (mm)", smd_md = "SMD Moderately Drained (mm)",
      smd_pd = "SMD Poorly Drained (mm)", glorad = "Solar Radiation (J/cm²)"
    )
    
    # Filter data
    filtered_data <- AllDunsany %>%
      filter(month %in% m, year %in% y)
    
    # Prepare data based on time scale
    if(input$time_scale == "day") {
      plot_data <- filtered_data %>%
        select(date, all_of(input$variables)) %>%
        pivot_longer(cols = all_of(input$variables),
                     names_to = "variable", values_to = "value")
      x_var <- "date"
      x_label <- "Date"
    } else if(input$time_scale == "month") {
      plot_data <- filtered_data %>%
        mutate(month_year = as.Date(paste(year, month, "01", sep = "-"))) %>%
        group_by(month_year) %>%
        summarise(across(all_of(input$variables), ~mean(.x, na.rm = TRUE))) %>%
        pivot_longer(cols = all_of(input$variables),
                     names_to = "variable", values_to = "value")
      x_var <- "month_year"
      x_label <- "Month"
    } else {
      plot_data <- filtered_data %>%
        group_by(year) %>%
        summarise(across(all_of(input$variables), ~mean(.x, na.rm = TRUE))) %>%
        pivot_longer(cols = all_of(input$variables),
                     names_to = "variable", values_to = "value")
      x_var <- "year"
      x_label <- "Year"
    }
    
    # Create plot
    p <- ggplot(plot_data, aes(x = .data[[x_var]], y = value, 
                               colour = variable, fill = variable))
    
    if(input$graph_type == "bar") {
      p <- p + geom_col() +
        facet_wrap(~variable, scales = "free_y",
                   labeller = as_labeller(var_labels))
    } else {
      p <- p + geom_line(linewidth = 1)
    }
    
    # Colors
    colors <- c(maxtp = "#F8766D", mintp = "#E76BF3", gmin = "#A58AFF",
                soil = "#00B0F6", rain = "#00BFC4", cbl = "#00C19A",
                wdspkm = "#C77CFF", hmkm = "#FF61C3", ddhm = "#00BA38",
                hgkm = "#00BF7D", pe = "#00C1AB", evap = "#00BD5C",
                smd_wd = "#AEA200", smd_md = "#B385FF", smd_pd = "#EF67EB",
                glorad = "#CD9600")
    
    p + scale_colour_manual(values = colors, labels = var_labels) +
      scale_fill_manual(values = colors, labels = var_labels) +
      labs(x = x_label, 
           title = paste(time_labeller(m, y), "Weather Data,", input$location)) +
      theme(legend.title = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(hjust = 0.5),
            legend.position = if(input$graph_type == "bar") "none" else "top")
  })
}

shinyApp(ui = ui, server = server)