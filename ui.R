#ui.R

###initialize user interface
ui <- 
  dashboardPage(
    dashboardHeader(title = HTML("Shiny framework example"), 
                    disable = FALSE, 
                    titleWidth  = 600,
                    dropdownMenu(type = "tasks", badgeStatus = "danger",
                                 taskItem(value = 30, color = "green",
                                          "Check out this shiny app"),
                                 taskItem(value = 50, color = "yellow",
                                          "Invite me for a job interview")
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
                fluidPage(
                  titlePanel("Contains interactive time series"),
                  column(4,  
                         wellPanel(
                           h3("Inputs"),
                           br(),
                           sliderInput(inputId = "ts_slider_input", "Time available:",
                                       min = min(rides$travel_date) , 
                                       max =  max(rides$travel_date),
                                       value = c( as.Date("2017-12-17"), as.Date("2018-03-17") )
                           ),
                           
                           selectInput(inputId = "ts_select_input", 
                                       label = "Choose payment method",
                                       choices = c("Mpesa", "Cash"),
                                       selected = "Mpesa"
                           )   
                         )
                  ),
                  column(8, plotlyOutput("tsplot") )
                ), ###
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
                       " and the last date is ", max(rides$travel_date), "."
                ),
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