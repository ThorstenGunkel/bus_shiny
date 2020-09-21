#transdef GmbH shiny Beispiel

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(lubridate)


ui <- 
  dashboardPage(
    dashboardHeader(title = HTML("Thorsten testet"), 
                    disable = FALSE, 
                    titleWidth  = 600,
                    dropdownMenu(type = "tasks", badgeStatus = "danger",
                                 taskItem(value = 40, color = "green",
                                          "Check out this shiny app"),
                                 taskItem(value = 20, color = "yellow",
                                          "Invite me for a job interview"),
                                 taskItem(value = 100, color = "red",
                                          "Thorsten.Gunkel@posteo.de")
                    ),
                    dropdownMenu(type = "messages", badgeStatus = "success", icon = icon("share-alt"),
                                 messageItem("Support Team",
                                             "This is the content of another message.",
                                             time = "2 hours"
                                 ),
                                 messageItem(
                                   from = 'GitHub',
                                   message = "this contains no message",
                                   icon = icon("github"),
                                   href = "https://github.com/ThorstenGunkel/bus_shiny"
                                 ),
                                 messageItem(
                                   from = 'LinkedIn',
                                   message = "Linkedinaccount",
                                   icon = icon("linkedin"),
                                   href = "http://www.linkedin.com/shareArticle?mini=true&url=" 
                                 )
                    )
    ),
    
    
    
    dashboardSidebar(
      sidebarUserPanel("works", 
                       subtitle = a(href = "#", icon("circle", class = "text-success"), "Dashboard works")
      ),
      sidebarSearchForm(label = "Enter a number", "searchText", "searchButton"),
      
      sidebarMenu(
        # Setting id makes input$tabs give the tabName of currently-selected tab
        id = "tabs",
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Descriptive stuff_text", icon = icon("th"), tabName = "descr", badgeLabel = "new",
                 badgeColor = "green"),
        menuItem("Charts", icon = icon("bar-chart-o"),
                 menuSubItem("Histogram", tabName = "subitem1"),
                 menuSubItem("Line plot (grouped)", tabName = "subitem2"),
                 menuSubItem("Boxplot", tabName = "sub_boxplot")
                 
        )
      )
    ),
    
    
    
    #body halt für die verschiedenen Tabs..
    dashboardBody(
      tabItems(
        tabItem(tabName = "dashboard",
                ## contents for the dashboard tab
                div(p("Dashboard tab content")),
                h1(paste0("time series diesdas ", 50)) ,
       #         fluidRow(
        #          valueBoxOutput("ExTotBox"), #%>% withSpinner(type=4),
         #         valueBoxOutput("ImTotBox"),
          #        valueBoxOutput("BlTotBox")
           #     ),
                
                # Boxes need to be put in a row (or column)
                fluidPage(
                  titlePanel("This is a time series"),
                  #           fluidRow()
                  column(4,  wellPanel(
                    sliderInput(inputId = "slider2", "Time available:",
                                min = min(rides$travel_date) , max =  max(rides$travel_date),
                                value = c( as.Date("2017-12-17"), as.Date("2018-03-17") )
                    )
                  )
                  ),
                  column(8, plotOutput("tsplot"))
                ),
                h5(paste0("gucken ob hier hin schreiben kann"))
                
        ),
        
        #hier oben drüber siehts so aus als ob zwei fludrow verschachtelt sind, das mit controls2 ist aber niht eingerückt      
        
        tabItem("descr",
                h1(paste0("Descriptive statistics about the dataset ","rides")) ,
                paste0("It contains ", nrow(rides), " rows and ", ncol(rides), "variables"),
                br(),
                paste0("The available variables are: ", names(rides[,1])),
                paste0("ipsum")
        ),
        
        
        tabItem("subitem1",
                h1( "Sub-item 1 tab content"),
                plotOutput("hist1_seats")
        ),
        tabItem("subitem2",
                h1("Time series with groups and moving average"),
                "test",
                br(),
                plotOutput("ts_grouped", height = 400),

#                fluidPage((
#                  box(plotOutput("ts_grouped", height = 400))
#                )),
                br(),
                "test",
                "noch ein Bild Zeitreihe, mit Moving average",
                plotOutput("ts_bus_projection", height = 400)
        ),
        tabItem("sub_boxplot",
                 h1( "Even more graphs! This time: Boxplot"),
                plotOutput("boxplot")
        )
      )
    )
  )








server <- function(input, output) {
  
  #getwd()
  
  #importing the dataset, 
  #excluding seat number and vehicle capacity
  #some IDs are duplicates i.e. whenever more than one seat is booked
  #include a new variable for number of people per ride_id
  
  rides <- read.csv("train_revised.csv") %>%
    select(-seat_number, -payment_receipt, -max_capacity) %>%
    group_by(ride_id) %>%
    mutate(seats_booked = n())
  
  rides <- rides[!duplicated(rides$ride_id),] #6249
  
  #correct the data format
  rides$travel_date <- as.Date(rides$travel_date, "%d-%m-%y")
  
#######graphs
  
  #Histogramm: Distribution of booked_sitze overall
  #passt so.
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
  output$ts_bus_projection <- renderPlot({
    rides %>%
      group_by(car_type, travel_date = floor_date(travel_date, "7days")) %>%
      summarise( daysum = sum(seats_booked), .groups = "keep") %>%
      ggplot(aes(y = daysum, x = travel_date, color = car_type)) +
      geom_line(alpha = .5) +
      theme_minimal() +
      labs(x = "Time", y = "Tickets sold") +
      ggtitle("Time series with projection")
  })
  
  
  
  #htime series with slider  
  output$tsplot <- renderPlot({
    rides %>%
      filter(travel_date > input$slider2[1] & travel_date <  input$slider2[2] ) %>%
      group_by(travel_date) %>%
      summarise(seats_sum = sum(seats_booked), payment_method) %>%
      ggplot(aes(x = travel_date, y = seats_sum)) + 
      geom_line(color = "#006400", size = .5) +
      theme_minimal() + 
      labs(x = "Date", y = "Tickets sold per day") + 
      ggtitle("Development of sold Bus Tickets to Nairobi for 14 bus routes", 
              subtitle = "Data Source: https://zindi.africa/") + 
      theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5)) +
      theme(text = element_text(size = 15)) 
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
    
    
    # rides %>%
    #   group_by(monat = floor_date(travel_date, "month")) %>%
    #   ggplot(aes(y = seats_booked, x = car_type)) + 
    #   geom_boxplot(color = "black") +  
    #   facet_grid(. ~ monat, labeller = labeller(monat = remove_day) )
    })
}


shinyApp(ui, server)
rm(ui, server)
#to do: descriptive: names of variables
#df gibt am Anfang problee, weil es wohl ein efunktion densitifyfunction gib, ie df reserviert hat

#idee daher: https://heartbeat.fritz.ai/predicting-bus-ticket-sales-using-machine-learning-dd2fcfe15392
#https://shiny.rstudio.com/gallery/nz-trade-dash.html
