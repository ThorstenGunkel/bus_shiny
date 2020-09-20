
#transdef GmbH shiny Beispiel

#https://shiny.rstudio.com/gallery/nz-trade-dash.html


library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(palmerpenguins)

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
                 menuSubItem("Sub-item 2", tabName = "subitem2")
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
                  box(plotOutput("plot1_ggplot_pen", height = 400))
                ))
        ),
        tabItem("subitem2",
                h1("Zeitreihe"),
                fluidPage((
                  box(plotOutput("plot1_ggplot_ts", height = 400))
                ))
        )
      )
    )
  )








server <- function(input, output) {
  
  rides <- read.csv("/home/qwerty/Documents/R/transdev_bewerbung/zindi_bus/train_revised.csv") %>%
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
  
  #time series per week?
  
  
  
  #alt:
  df <- penguins
  
  output$plot1_penguin <- renderPlot({
    df_hist <- df[seq_len(input$slider2),]
    hist(df_hist$body_mass_g)
  })
  
  output$plot1_ggplot_pen <- renderPlot({
    df %>%
      filter(species == "Adelie" | species == "Chinstrap") %>%
      ggplot(aes(x = body_mass_g, fill = species )) + 
      geom_histogram( color="grey", alpha=0.4, position = 'identity') +
      scale_fill_manual(values=c("#69b3a2", "#404080")) +
      theme_minimal() +
      labs(fill="Species")  + 
      labs(x = "Häufigkeit", y = "Gewicht in Gramm") +
      ggtitle("Verteilung des Gewichts zweier Pinguinklassen")
  })
  
  
  
  
  output$plot1_ggplot_ts <- renderPlot({
    df %>%
      #      filter(species == "Adelie" | species == "Chinstrap") %>%
      ggplot(aes(x = bill_depth_mm,y = bill_length_mm, color = species )) + 
      geom_point() +
      # geom_histogram( color="grey", alpha=0.4, position = 'identity') +
      # scale_fill_manual(values=c("#69b3a2", "#404080")) 
      theme_minimal() +
      #       labs(fill="Species")  + 
      labs(x = "Variable X", y = "n") +
      ggtitle("scatter of X")
  })
  
  #iher zum vergleich
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  
  #htime series with slider  
  output$tsplot <- renderPlot({
    # rides %>%
    #   filter(travel_date > input$slider2[1] & travel_date <  input$slider2[2] ) %>%
    #   group_by(travel_date) %>%
    #   summarise(seats_sum = sum(seats_booked)) %>%
    #   ggplot(aes(x = travel_date, y = seats_sum)) + 
    #   geom_line(color = "#006400", size = .5)
    # 
    
    
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
