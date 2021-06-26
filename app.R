#shiny montecarlo simulation ----
library(tidyverse)
library(magrittr)
library(shiny)
library(DT)

ui <- fluidPage(
  # Application title
  titlePanel("Restaurant Reservations Simulation"),
  # sidebar for inputs
  sidebarLayout(
    sidebarPanel(
 
      numericInput("iterations",
                   "Number of days to simulate:",
                   min = 1,
                   max = Inf,
                   value = 20000),
      numericInput("ralphdeparttime",
                   "Ralph Departs From Work:",
                   value = 17.5,
                   step=0.25),
      numericInput("bobdeparttime",
                   "Neighbor Bob Departs From Work:",
                   value = 17.5,
                   step=0.25),
      numericInput("commutemean",
                   "Ralph & Bob Average Commute:",
                   min=1,
                   value=30),
      numericInput("commutesd",
                   "Ralph & Bob Commute Standard Deviation:",
                   min=1,
                   value=10),
     
      numericInput("walktimemean",
                   "Average Walk Time to Restaurant:",
                   min=1,
                   value=10),
      numericInput("walktimesd",
                   "Walk Time to Restaurant Standard Deviation:",
                   min=1,
                   value=2), width=3),
    # Show a plot of the output tables and histograms
    mainPanel(
      DT::DTOutput('freqtable'),
      DT::DTOutput("resultstable"),
      plotOutput('arrivalhistogram'),
      plotOutput('ralphcommute_histogram'),
      plotOutput('bobcommute_histogram')
      
      
    )
  ) )

server <- function(input, output) {
  #pull them all together...
  outdf_r <- reactive({
    ralph_depart_time <- rep(input$ralphdeparttime,input$iterations)
    ralph_commute_duration_minutes <-   rnorm(n=input$iterations, #removed pmax 15
                                              mean=input$commutemean,
                                              sd=input$commutesd)
    bob_depart_time <-  rep(input$bobdeparttime,input$iterations)
    bob_commute_duration_minutes <- rnorm(n=input$iterations,
                                          mean=input$commutemean,
                                          sd=input$commutesd) #removed pmax 15
    #we walk to the restaurant, pretty close!
    walk_time_duration_minutes <-  rnorm(n=input$iterations, #removed pmin(pmax(5, ,20
                                         mean=input$walktimemean,
                                         sd=input$walktimesd)
    outdf <- 
      cbind.data.frame(
        ralph_depart_time,
        ralph_commute_duration_minutes,
        bob_depart_time,
        bob_commute_duration_minutes,
        walk_time_duration_minutes) %>%
      dplyr::mutate(Ralph_Arrival_Home_Time = ralph_depart_time + (ralph_commute_duration_minutes/60),
                    bob_Arrival_Home_Time = bob_depart_time + (bob_commute_duration_minutes/60),) %>%
      dplyr::mutate(Later_Arrival_Home = pmax(Ralph_Arrival_Home_Time,bob_Arrival_Home_Time)) %>%
      dplyr::mutate(Arrival_Restaurant = Later_Arrival_Home + walk_time_duration_minutes/60) %>%
      dplyr::mutate(ralph_commute_duration_minutes=plyr::round_any(ralph_commute_duration_minutes,.01),
                    Ralph_Arrival_Home_Time = plyr::round_any(Ralph_Arrival_Home_Time,.01),
                    bob_commute_duration_minutes=plyr::round_any(bob_commute_duration_minutes,.01),
                    bob_Arrival_Home_Time = plyr::round_any(bob_Arrival_Home_Time,.01),
                    Later_Arrival_Home  = plyr::round_any(Later_Arrival_Home,0.01),
                    walk_time_duration_minutes=plyr::round_any(walk_time_duration_minutes,0.01),
                    Arrival_Restaurant = plyr::round_any(Arrival_Restaurant,0.01)) %>%
      dplyr::select(ralph_depart_time,ralph_commute_duration_minutes,Ralph_Arrival_Home_Time,
                    bob_depart_time,bob_commute_duration_minutes,bob_Arrival_Home_Time,
                    Later_Arrival_Home,
                    walk_time_duration_minutes,
                    Arrival_Restaurant
      ) %>%
      dplyr::mutate(MadeItInTime = dplyr::if_else(Arrival_Restaurant <= 18.25,"Yes","No")) 
  })
  
  output$resultstable <- DT::renderDT( {
    
    outdf_prettified <- outdf_r() %>%
      dplyr::mutate(ralph_depart_time = paste(floor(ralph_depart_time),":",
                                              dplyr::if_else(plyr::round_any((ralph_depart_time %%1) *60,1)<10,"0",""),             
                                              plyr::round_any((ralph_depart_time %%1) *60,1),sep='')) %>%
      dplyr::mutate(Ralph_Arrival_Home_Time = paste(floor(Ralph_Arrival_Home_Time),":",
                                                    dplyr::if_else(plyr::round_any((Ralph_Arrival_Home_Time %%1) *60,1)<10,"0",""),             
                                                    plyr::round_any((Ralph_Arrival_Home_Time %%1) *60,1),sep='')) %>%
      dplyr::mutate(bob_depart_time = paste(floor(bob_depart_time),":",
                                            dplyr::if_else(plyr::round_any((bob_depart_time %%1) *60,1)<10,"0",""),             
                                            plyr::round_any((bob_depart_time %%1) *60,1),sep='')) %>%
      dplyr::mutate(bob_Arrival_Home_Time = paste(floor(bob_Arrival_Home_Time),":",
                                                  dplyr::if_else(plyr::round_any((bob_Arrival_Home_Time %%1) *60,1)<10,"0",""),             
                                                  plyr::round_any((bob_Arrival_Home_Time %%1) *60,1),sep='')) %>%
      dplyr::mutate(Later_Arrival_Home = paste(floor(Later_Arrival_Home),":",
                                               dplyr::if_else(plyr::round_any((Later_Arrival_Home %%1) *60,1)<10,"0",""),             
                                               plyr::round_any((Later_Arrival_Home %%1) *60,1),sep='')) %>%
      dplyr::mutate(Arrival_Restaurant = paste(floor(Arrival_Restaurant),":",
                                               dplyr::if_else(plyr::round_any((Arrival_Restaurant %%1) *60,1)<10,"0",""),             
                                               plyr::round_any((Arrival_Restaurant %%1) *60,1),sep='')) %>%
      dplyr::rename("Ralph's Office Departure Time"=ralph_depart_time,
                    "Ralph's Commute (Minutes)"=ralph_commute_duration_minutes,
                    "Ralph's Arrival At Home" = Ralph_Arrival_Home_Time,
                    "Bob's Office Departure Time"=bob_depart_time,
                    "Bob's Commute (Minutes)"=bob_commute_duration_minutes,
                    "Bob's Arrival At Home" = bob_Arrival_Home_Time,
                    "Later Arrival" = Later_Arrival_Home,
                    "Walk Time to Restaurant (Minutes)" = walk_time_duration_minutes,
                    "Arrival Time At Restaurant"= Arrival_Restaurant,
                    "Made the 6:15pm Reservation?" =MadeItInTime
      ) 
    outdf_prettified
  })
  
  output$arrivalhistogram <- renderPlot({
    outdf_r() %>%
      ggplot(aes(x = Arrival_Restaurant,fill=MadeItInTime)) +
      geom_histogram(bins=100)+scale_x_continuous(breaks=seq(0,24,(1/12))) + ggtitle("Arrival Time at Restaurant")
  })
  
  output$ralphcommute_histogram <- renderPlot({
    outdf_r() %>%
      ggplot(aes(x = (ralph_commute_duration_minutes),
                 fill= ralph_commute_duration_minutes<=30)) +
      geom_histogram(bins=100) + ggtitle("Ralph's Commute Time")+
      scale_x_continuous(breaks=seq(-100,100,5)) + xlab("Ralph's Commute Time")
  })
  
  output$bobcommute_histogram <- renderPlot({
    outdf_r() %>%
      ggplot(aes(x = (bob_commute_duration_minutes),
                 fill= bob_commute_duration_minutes<=30)) +
      geom_histogram(bins=100) + ggtitle("Bob's Commute Time") +
      scale_x_continuous(breaks=seq(-100,100,5)) + xlab("Bob's Commute Time")
  })
  
  
  output$freqtable <- DT::renderDataTable({ outdf_r() %>% dplyr::group_by(MadeItInTime) %>% 
      summarise(Count=n()) %>% 
      dplyr::mutate(Percent= scales::percent(Count/sum(Count))) %>%
      dplyr::rename('Made It In Time?'= MadeItInTime) })
}
# Run the application 
shinyApp(ui = ui, server = server)
