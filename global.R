#transdef GmbH shiny example
#data source https://zindi.africa/competitions/traffic-jam-predicting-peoples-movement-into-nairobi/data

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(rmarkdown)

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

#runApp()