#transdef GmbH shiny example
#data source https://zindi.africa/competitions/traffic-jam-predicting-peoples-movement-into-nairobi/data

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)


###initialie user interface
ui <- 
  dashboardPage(
    dashboardHeader(title = HTML("Thorsten testet"), 
                    disable = FALSE, 
                    titleWidth  = 600,
                    dropdownMenu(type = "tasks", badgeStatus = "danger",
                                 taskItem(value = 40, color = "green",
                                          "Check out this shiny app"),
                                 taskItem(value = 20, color = "yellow",
                                          "Invite me for a job interview")
  #                               taskItem(value = 100, color = "red",
 #                                         "Win-win situation")
                    ),
                    dropdownMenu(type = "notifications", badgeStatus = "success", icon = icon("share-alt"),
                                 messageItem(
                                   from = 'GitHub',
                                   message = "GitHub account",
                                   icon = icon("github"),
                                   href = "https://github.com/ThorstenGunkel/bus_shiny"
                                 ),
                                 messageItem(
                                   from = 'LinkedIn',
                                   message = "LinkedIn account",
                                   icon = icon("linkedin"),
                                   href = "https://www.linkedin.com/in/thorsten-gunkel-5824801a9/" 
                                 )
                    )
    ),
    
    
### Sidebar
    dashboardSidebar(
      sidebarUserPanel("Bus ticket example", 
                       subtitle = a(href = "#", icon("circle", class = "text-success"), "Dashboard works")
      ),
   
      # sidebarSearchForm(label = "Enter a number", "searchText", "searchButton"),

      sidebarMenu(
        # Setting id makes input$tabs give the tabName of currently-selected tab
        id = "tabs",
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Descriptive text", icon = icon("th"), tabName = "descr", badgeLabel = "Text!",
                 badgeColor = "green"),
        menuItem("Charts", icon = icon("bar-chart-o"),
                 menuSubItem("Histogram", tabName = "sub_hist"),
                 menuSubItem("Line plot (grouped)", tabName = "sub_ts"),
                 menuSubItem("Boxplot", tabName = "sub_boxplot")
                 
        )
      )
    ),
    
    
#### Body 
    dashboardBody(
      tabItems(
        tabItem(tabName = "dashboard",
                # Boxes need to be put in a row (or column)
                fluidPage(
                  
                  #idee: boxen einführen, mit überschrift input output
                  
    #              box(title = "Histogram", status = "primary", plotOutput("plot2", height = 250)),
                 
                  
                  
                  titlePanel("Contains interactive time series"),
                  column(4,  
                         wellPanel(h3("Inputs"), br(),
                           sliderInput(inputId = "ts_slider_input", "Time available:",
                                       min = min(rides$travel_date) , 
                                       max =  max(rides$travel_date),
                                       value = c( as.Date("2017-12-17"), as.Date("2018-03-17"))
                                       ),
                           
                           selectInput(inputId = "ts_select_input", 
                                       label = "Choose payment method",
                                       choices = c("Mpesa", "Cash"),
                                       selected = "Mpesa"
                           )
                           
                           # ,
                           # box( #dit is neu
                           #   title = "Inputs", status = "warning",
                           #   "Box content here", br(), "More box content",
                           #   sliderInput("slider", "Slider input:", 1, 100, 50),
                           #   textInput("text", "Text input:")
                           # )
                         )
                  ),
                  column(8, plotlyOutput("tsplot"))
                ),
        ),

        tabItem("descr",
                h1(paste0("Descriptive statistics about the dataset")) ,
                paste0("The dataset consists of sold tickets to Nairobi for 14 bus routes. 
                       It contains ", nrow(rides), " rows and ", ncol(rides), " variables."),
                br(),
                paste0("An explanation of the dataset and the variables are found "),
                tagList(a("here.", href="https://github.com/ThorstenGunkel/bus_shiny")),
                br(),
                
                paste0("The earliest date is ", min(rides$travel_date),
                       " and the last date is ", max(rides$travel_date), "."),
                br(),
                paste0("In October there is only ",  
                       nrow(rides[rides$travel_date < as.Date("2017-11-01"),]),
                       " data point. Therefore the data seems incomplete. 
                       If this turns out to be an outlier, it might make sense to remove it
                       for further data processing when it comes to trend projections and
                       use of aggregated data (e.g. months)"
                       )
                ),
        
        tabItem("sub_hist",
                h1( "Histogram"),
                plotOutput("hist1_seats")
        ),
        
        tabItem("sub_ts",
                h1("Time series with groups and trend"),
                br(),
                plotOutput("ts_grouped", height = 400),
                br(),
                h4("Another time series plot but this time with trend"),
                plotOutput("ts_bus_trend", height = 400),
                "Since there is a day with only one sold ticket, the computed trend is quite steep",
                br(),
                "For a thorough data analysis, one has to decide wether the data for november is correct"
        ),
        
        tabItem("sub_boxplot",
                 h1( "Even more graphs! This time: Boxplot"),
                plotOutput("boxplot")
        )
      )
    )
  )







### Initialise server function
server <- function(input, output) {
  

  ###importing the data
  
  #excluding seat number and vehicle capacity
  #some IDs are duplicates i.e. whenever more than one seat is booked
  #include a new variable for number of people per ride_id
  
  rides <- 
    read.csv("train_revised.csv") %>%
    select(-seat_number, -payment_receipt, -max_capacity) %>%
    group_by(ride_id) %>%
    mutate(seats_booked = n())
  
  #remove duplicated ride ids so it results  one line per ride
  rides <- rides[!duplicated(rides$ride_id),] #6249
  
  #correct the data format
  rides$travel_date <- as.Date(rides$travel_date, "%d-%m-%y")
  
  
  
  ###Graphs
  
  #Histogramm: Distribution of booked seats overall
  output$hist1_seats <- renderPlot({
    rides %>%
      ggplot(aes(x = seats_booked, fill = car_type )) + 
      geom_histogram( color="grey", alpha=0.4, position = 'identity', binwidth = 1.2) +
      scale_fill_manual(values=c("red", "blue")) +
      theme_minimal() +
      labs(fill="Species")  + 
      labs(x = "Seats booked per ride", y = "Number of times occured") +
      ggtitle("Distribution of booked seats") + 
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  #hier gucken: variable inkonsistent etc. 
  output$ts_grouped <- renderPlot({
    rides %>%
      group_by(car_type, twoweeks = floor_date(travel_date, "days")) %>%
      summarise( twoweekssum = sum(seats_booked), .groups = "keep") %>%
      ggplot(aes(y = twoweekssum, x = twoweeks, color = car_type)) +
      geom_line(alpha = 0.5) +
      theme_minimal() +
      labs(x = "Variable X", y = "n") +
      ggtitle("scatter of X")
  })
  
  #
  #time series for car_type = bus only and project the data for a few more weeks
  output$ts_bus_trend <- renderPlot({
    rides %>%
      filter(car_type == "Bus") %>%
      group_by(car_type, travel_date = floor_date(travel_date, "week")) %>%
      summarise( daysum = sum(seats_booked), .groups = "keep") %>% 
      ggplot(aes(y = daysum, x = travel_date, color = car_type)) +
      geom_line(alpha = 1.5) +
      stat_smooth(method="lm",fullrange=FALSE) + 
   #   stat_smooth(method="loess",fullrange=TRUE) + 
      theme_minimal() +
      labs(x = "Time", y = "Tickets sold") +
      guides(col = FALSE) + 
      ggtitle("Weekly ticket sales  with trend")
  })
  
  
  
  #htime series with slider  
  
  options(dplyr.summarise.inform = FALSE)
  
  output$tsplot <- renderPlotly({
 #   ts_interact <-
    rides %>%
      filter(travel_date > input$ts_slider_input[1] & travel_date <  input$ts_slider_input[2] ) %>%
      filter(payment_method == input$ts_select_input) %>%
      group_by(travel_date) %>%
      summarise(seats_sum = sum(seats_booked), payment_method) %>%
      ggplot(aes(x = travel_date, y = seats_sum)) + 
      geom_line(color = "#006400", size = .5) +
      theme_minimal() + 
      labs(x = "Date", y = "Tickets sold per day") + 
      ggtitle("Development of sold tickets", 
              subtitle = "Data Source: https://zindi.africa/") + 
      theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5)) +
      theme(text = element_text(size = 10)) 
      ggplotly()
  })
  
  
  #function to removethe last three characters for better labeling
  remove_day <- function(string) {
    gsub(".{3}$", "", string)
  }
  
  #Graph boxplot
  output$boxplot <- renderPlot({
    rides %>%
      group_by(monat = floor_date(travel_date, "month")) %>%
      ggplot(aes(y = seats_booked, x = car_type)) + 
      geom_boxplot(color = "black", varwidth = FALSE, width = 0.6, fill = "lightgrey") +  
      facet_grid(. ~ monat, labeller = labeller(monat = remove_day) ) + 
      labs(x = NULL,  y = "Tickets sold") + 
      ggtitle("Distribution of number of tickets sold per day, shown by month", 
              subtitle = "Data Source: https://zindi.africa/") + 
      theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5)) +
      theme(text = element_text(size = 15)) 
    })
}


shinyApp(ui, server)
rm(ui, server)

#https://shiny.rstudio.com/gallery/nz-trade-dash.html

# hier noch schauen:
#https://www.r-graph-gallery.com/histogram_several_group.html
# http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
