#install.packages('dygraph')
#install.packages("readxl")
#install.packages("ggplot2")
#install.packages("tidyr")


library(shiny)
library(bslib)
#library(readxl)
#library(dygraphs)
#library(ggplot2) 
#library(tidyr)
#library(dplyr)
#library(ggiraph)
library(plotly)


#load availability data 
availability_data <- read.csv("availability.csv")

# #load consumption data 
consumption_data <- read.csv("consumption.csv")

# #load water stress data 
stress_data <- read.csv("stress.csv")

# #load access data 
access_data <- read.csv("access.csv")


# Define UI for app that draws a histogram ----
ui <- page_sidebar(
  title = "Water Security Under Climate Risk in Small Island Developing States (SIDS): Interactive Figures for Hydro-SMR Indicators",
  fillable = TRUE, 
  sidebar = sidebar(
    position = "right",
    
    checkboxGroupInput("countries",
                       label = tags$span(tags$strong("Select Countries to Display"), tags$em("\n")), 
                       choices = list("Antigua and Barbuda", "Bahamas", "Barbados", "Belize", "Cuba", "Dominica", "Dominican Republic", "Grenada", "Guyana", "Haiti", "Jamaica", "Puerto Rico", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", "Suriname", "Trinidad and Tobago"), 
                       selected = list("Antigua and Barbuda", "Bahamas", "Barbados", "Belize", "Cuba", "Dominica", "Dominican Republic", "Grenada", "Guyana", "Haiti", "Jamaica", "Puerto Rico", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", "Suriname", "Trinidad and Tobago"),
                       inline = FALSE
    ),
    
  ),
  
  card(
    card_header("Introduction"),
    #height = "20vh",
    "The following interactive graphs correspond to Figures 3-6. To view data in more detail, please select the countries of interest from the menu on the right. All data is sourced from FAO AQUASTAT (2026).  "
  ),
  navset_card_underline(
    full_screen = TRUE,
    nav_panel(
      title = "Availability",
      plotlyOutput("availability_plot", width = "100%", height = "100%")
      
    ),
    nav_panel(
      title = "Consumption",
      plotlyOutput("consumption_plot", width = "100%", height = "100%")
      
    ),
    nav_panel(
      title = "Water Stress",
      plotlyOutput("water_stress_plot", width = "100%", height = "100%")
      
    ),
    
    nav_panel(
      title = "Access",
      plotlyOutput("access_plot", width = "100%", height = "100%")
    )
  )
  
  
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  
  
  # #Configure availability plot 
  output$availability_plot <-renderPlotly({
    #filtered_availability_data <- filter(availability_data, Country %in% c(input$countries))
    
    availability_plot <- plot_ly(
      data = subset(availability_data, Country %in% c(input$countries)),
      x = ~Year,
      y = ~availability, 
      color = ~Country,
      mode = "lines", 
      text = ~paste("Country: ", Country,"<br>", "Year: ", Year, "<br>",  "Availability: ", sprintf("%.2f",availability)), 
      hoverinfo = "text",
      showlegend =  TRUE) %>%
        layout (
          xaxis = list(
            text = "Year"
          ),
          yaxis = list(
            text = "Availability (m<sup>3</sup>/capita)"
          )
        )
    
  })
  # 
  # #Configure consumption plot 
  output$consumption_plot <-renderPlotly({
    #filtered_consumption_data <- filter(consumption_data, Country %in% c(input$countries))
    plot_ly(
      data = subset(consumption_data, Country %in% c(input$countries)),
      x = ~Year,
      y = ~consumption, 
      color = ~Country,
      mode = "lines", 
      text = ~paste("Country: ", Country,"<br>", "Year: ", Year, "<br>",  "Consumption: ", sprintf("%.2f",consumption)), 
      hoverinfo = "text",
      showlegend = TRUE
    )
    
  })
  # #Configure water stress plot 
  output$water_stress_plot <-renderPlotly({
    
    plot_ly(
      data = subset(stress_data, Country %in% c(input$countries)),
      x = ~Year,
      y = ~stress, 
      color = ~Country,
      mode = "lines", 
      text = ~paste("Country: ", Country,"<br>", "Year: ", Year, "<br>",  "Water Stress: ", sprintf("%.2f",stress)), 
      hoverinfo = "text",
      showlegend = TRUE
    )
    
    
  })
  # 
  # #Configure water access plot 
  output$access_plot <-renderPlotly({
    plot_ly(
      data = subset(access_data, Country %in% c(input$countries)),
      x = ~Year,
      y = ~access, 
      color = ~Country,
      mode = "lines", 
      text = ~paste("Country: ", Country,"<br>", "Year: ", Year, "<br>",  "Access: ", sprintf("%.2f",access)), 
      hoverinfo = "text",
      showlegend = TRUE
    )
 
  })
  
}

shinyApp(ui = ui, server = server) 


