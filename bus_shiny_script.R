#transdef GmbH shiny Beispiel

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(palmerpenguins)
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
                                   from = 'Twitter',
                                   message = "",
                                   icon = icon("twitter"),
                                   href = "https://twitter.com/intent/tweet?url=http%3A%2F%2Ftradeintelligence.mbie.govt.nz&text=New%20Zealand%20Trade%20Intelligence%20Dashboard"
                                 ),
                                 messageItem(
                                   from = 'LinkedIn',
                                   message = "",
                                   icon = icon("linkedin"),
                                   href = "http://www.linkedin.com/shareArticle?mini=true&url=" 
                                 )
                    )
    ),
    
    
    
    dashboardSidebar(
      sidebarUserPanel("Beispiel", 
                       subtitle = a(href = "#", icon("circle", class = "text-success"), "Online")
      ),
      sidebarSearchForm(label = "Enter a number", "searchText", "searchButton"),
      
      sidebarMenu(
        # Setting id makes input$tabs give the tabName of currently-selected tab
        id = "tabs",
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Descriptive stuff_text", icon = icon("th"), tabName = "descr", badgeLabel = "new",
                 badgeColor = "green"),
        menuItem("Charts", icon = icon("bar-chart-o"),
                 menuSubItem("Sub-item 1", tabName = "subitem1"),
                 menuSubItem("Time series with grouped colors", tabName = "subitem2")
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
                fluidRow(
                  valueBoxOutput("ExTotBox"), #%>% withSpinner(type=4),
                  valueBoxOutput("ImTotBox"),
                  valueBoxOutput("BlTotBox")
                ),
                
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
                paste0("The available variables are: ", names(rides[,1]))
        ),
        
        
        tabItem("subitem1",
                h1( "Sub-item 1 tab content"),
                fluidPage((
                  box(plotOutput("hist1_seats", height = 400))
                  
                ))
        ),
        tabItem("subitem2",
                h1("Zeitreihe aber aktuell noch pingus. dann ts_grouped"),
                fluidPage((
                  box(plotOutput("ts_grouped", height = 400))
                ))
        )
      )
    )
  )








server <- function(input, output) {
  
  #getwd()
  rides <- read.csv("train_revised.csv") %>%
    select(-seat_number, -payment_receipt, -max_capacity) %>%
    group_by(ride_id) %>%
    mutate(seats_booked = n())
  
  #correct the data format
  rides$travel_date <- as.Date(rides$travel_date, "%d-%m-%y")
  
  
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
}


shinyApp(ui, server)
rm(ui, server)
#to do: descriptive: names of variables
#df gibt am Anfang problee, weil es wohl ein efunktion densitifyfunction gib, ie df reserviert hat

#idee daher: https://heartbeat.fritz.ai/predicting-bus-ticket-sales-using-machine-learning-dd2fcfe15392
#https://shiny.rstudio.com/gallery/nz-trade-dash.html
