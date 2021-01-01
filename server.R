### Initialize server function
server <- function(input, output) {
  
  ###Graphs
  
  #Histogramm: Distribution of booked seats overall
  
  #actually "renderPlot" is not yet needed, as long as no interactive elements are included.
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
      theme(text = element_text(size = 10)) +
      labs(x = "Variable X", y = "n") +
      ggtitle("Sold bus tickets to Nairobi per day") 
  })
  
  
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
      theme(text = element_text(size = 10)) +
      labs(x = "Time", y = "Tickets sold") +
      guides(col = FALSE) + 
      ggtitle("Weekly ticket sales  with trend")  
  })
  
  
  #time series with slider  
  options(dplyr.summarise.inform = FALSE)
  
  output$tsplot <- renderPlotly({
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
  
  
  #Graph boxplot
  
  #function to remove the last three characters for better labeling
  #should better be outside of server.R in global.R, but in this case is quite small,so nevermind
  remove_day <- function(string) {
    gsub(".{3}$", "", string)
  }
  
  output$boxplot <- 
    renderPlot({
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
  
  #introducing kmeans
  #calculating kmeans - must be the same as in the plot
  k_number = 3
  
  #calculate test dataset only for kmeans example
  kmeans_xmpl <- 
  rides %>% 
    group_by(day = floor_date(travel_date, "day")) %>%
    summarise(seats_pday  = sum(seats_booked), .groups = "keep" )
    
  kmeans(kmeans_xmpl$seats_pday, 2)  
  
  
  #kmeans plot
  output$kmeans <- 
    renderPlot({
      rides %>%
        group_by(day = floor_date(travel_date, "day")) %>%
        summarise( seats_pday  = sum(seats_booked), .groups = "keep") %>%
        
        ggplot(aes(x = day, y=seats_pday)) +
        geom_point(color = kmeans(kmeans_xmpl$seats_pday, 5)$cluster) + #for testing: explicit numerical input
#        geom_point(color = kmeans(kmeans_xmpl$seats_pday, input$input_kmean_group)$cluster) #variable as kmeans input
       labs(x = NULL,  y = "Tickets sold") + 
        ggtitle("kmeans example - Distribution of number of tickets sold per day", 
                subtitle = "Data Source: https://zindi.africa/") + 
        theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5)) +
        theme(text = element_text(size = 15)) 
    })
}