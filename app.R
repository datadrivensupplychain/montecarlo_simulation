#shiny montecarlo simulation ----
library(tidyverse)
library(magrittr)
library(shiny)
library(shinyTime)
library(DT)

ui <- fluidPage(
  # Application title
  titlePanel("Restaurant Reservations Simulation"),
  # sidebar for inputs
  sidebarLayout(
    sidebarPanel(
	width = 3,

 
      numericInput("iterations",
                   "Number of days to simulate:",
                   min = 1,
                   max = Inf,
                   value = 5000),
				   
      timeInput("ralphdeparttime_time",
                   "Ralph Departs From Work:",
                   value =  strptime("12:30:00", format="%T", tz="America/Chicago"), minute.steps=5),
                   
	
	    timeInput("bobdeparttime_time",
	          "Bob Departs From Work:",
	          value =  strptime("12:30:00", format="%T", tz="America/Chicago"), minute.steps=5),
				   
				   
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
                   value=2)

				   
				   ),
    # Show a plot of the output tables and histograms
    mainPanel(
      DT::DTOutput('freqtable'),
      DT::DTOutput("resultstable"),
      plotOutput('arrivalhistogram'),
      plotOutput('ralphcommute_histogram'),
      plotOutput('bobcommute_histogram'),
      plotOutput('walktime_histogram'),
      plotOutput('arrivaltime_cdf'),
      plotOutput('arrivaltime_cdf_madeitintime'),
      plotOutput('arrivaltime_cdf_didntmakeitintime')
	 
      
    )
  ) )

server <- function(input, output) {
  #pull them all together...
  outdf_r <- reactive({
  
    #convert the time inputs into fractional hours
    ralphdeparttime_frachours <- lubridate::hour(input$ralphdeparttime_time) + lubridate::minute(input$ralphdeparttime_time)/60
    bobdeparttime_frachours <- lubridate::hour(input$bobdeparttime_time) + lubridate::minute(input$bobdeparttime_time)/60

      #we depart from our respective offices at (possibly) different times.
  #our commute times are drawn from the same distribution

	ralph_depart_time <-rep(ralphdeparttime_frachours,input$iterations)
	
    ralph_commute_duration_minutes <-   rnorm(n=input$iterations,
                                              mean=input$commutemean,
                                              sd=input$commutesd)
											  
	bob_depart_time <- rep(bobdeparttime_frachours,input$iterations)
	
    bob_commute_duration_minutes <- rnorm(n=input$iterations,
                                          mean=input$commutemean,
                                          sd=input$commutesd)
										  
    #we walk to the restaurant, pretty close!
    walk_time_duration_minutes <-  rnorm(n=input$iterations,
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
                    bob_Arrival_Home_Time = bob_depart_time + (bob_commute_duration_minutes/60)) %>%
      dplyr::mutate(Later_Arrival_Home = pmax(Ralph_Arrival_Home_Time,bob_Arrival_Home_Time)) %>%
      dplyr::mutate(Arrival_Restaurant = Later_Arrival_Home + walk_time_duration_minutes/60) %>%
	  
					
      dplyr::select(ralph_depart_time,ralph_commute_duration_minutes,Ralph_Arrival_Home_Time,
                    bob_depart_time,bob_commute_duration_minutes,bob_Arrival_Home_Time,
                    Later_Arrival_Home,
                    walk_time_duration_minutes,
                    Arrival_Restaurant
      ) %>%
      dplyr::mutate(MadeItInTime = dplyr::if_else(Arrival_Restaurant <= 18.25,"Yes","No")) %>%
	  dplyr::ungroup() %>%
	  #find percentile of arrival time
	  dplyr::arrange(Arrival_Restaurant) %>%
	  dplyr::mutate(rownum = dplyr::row_number()) %>%
	  dplyr::mutate(ArrivalTime_Percentile = rownum/max(rownum)) %>%
	  #find percentile of arrival time, by made it / didn't make it group
	  dplyr::group_by(MadeItInTime) %>%
	  dplyr::mutate(rownum_MadeitGroup = dplyr::row_number()) %>%
	  dplyr::mutate(ArrivalTime_MadeItGroup_Percentile = rownum_MadeitGroup/max(rownum_MadeitGroup)) %>%
	  
	  #rearrange at random
	  dplyr::ungroup() %>%
	  dplyr::sample_frac(size=1) 
  })
  
  output$quantile_ralph <- DT::renderDT({
  ralphquantile <- as.data.frame(quantile(outdf_r()$ralph_commute_duration_minutes,seq(0,1,0.01)))
  ralphquantile })
  
    output$combo_quantile <- DT::renderDT({
  combo_quantile <- cbind.data.frame(
  ralphquantile = quantile(outdf_r()$ralph_commute_duration_minutes,seq(0,1,0.01)),
  bobquantile = quantile(outdf_r()$bob_commute_duration_minutes,seq(0,1,0.01)),
  stringsAsFactors=FALSE)
  
  combo_quantile })
  
  
  output$resultstable <- DT::renderDT( {
    
    outdf_prettified <- outdf_r() %>%
      dplyr::mutate(
	  
	  ralph_depart_time_present = paste(floor(ralph_depart_time),":",
                                              dplyr::if_else(plyr::round_any((ralph_depart_time %%1) *60,1)<10,"0",""),             
                                              plyr::round_any((ralph_depart_time %%1) *60,1),sep=''), 
											  
	  ralph_commute_duration_minutes_present = plyr::round_any(ralph_commute_duration_minutes,0.1),

	  Ralph_Arrival_Home_Time_present = paste(floor(Ralph_Arrival_Home_Time),":",
                                                    dplyr::if_else(plyr::round_any((Ralph_Arrival_Home_Time %%1) *60,1)<10,"0",""),             
                                                    plyr::round_any((Ralph_Arrival_Home_Time %%1) *60,1),sep=''),
													
	  bob_depart_time_present = paste(floor(bob_depart_time),":",
                                            dplyr::if_else(plyr::round_any((bob_depart_time %%1) *60,1)<10,"0",""),             
                                            plyr::round_any((bob_depart_time %%1) *60,1),sep=''),
      
	  bob_commute_duration_minutes_present = plyr::round_any(bob_commute_duration_minutes,0.1),
	  
	  bob_Arrival_Home_Time_present = paste(floor(bob_Arrival_Home_Time),":",
                                                  dplyr::if_else(plyr::round_any((bob_Arrival_Home_Time %%1) *60,1)<10,"0",""),             
                                                  plyr::round_any((bob_Arrival_Home_Time %%1) *60,1),sep=''), 
												  
	  Later_Arrival_Home = paste(floor(Later_Arrival_Home),":",
                                               dplyr::if_else(plyr::round_any((Later_Arrival_Home %%1) *60,1)<10,"0",""),             
                                               plyr::round_any((Later_Arrival_Home %%1) *60,1),sep=''),
	
		walk_time_duration_minutes_present = plyr::round_any(walk_time_duration_minutes,0.1), 
		
	  Arrival_Restaurant = paste(floor(Arrival_Restaurant),":",
                                               dplyr::if_else(plyr::round_any((Arrival_Restaurant %%1) *60,1)<10,"0",""),             
                                               plyr::round_any((Arrival_Restaurant %%1) *60,1),sep='')  )  %>% 
      
      
      dplyr::select("Ralph's Office Departure Time"= ralph_depart_time_present, 
                    "Ralph's Commute (Minutes)"= ralph_commute_duration_minutes_present,
                    "Ralph's Arrival At Home" = Ralph_Arrival_Home_Time_present , 
                    "Bob's Office Departure Time"= bob_depart_time_present , 
                    "Bob's Commute (Minutes)"= bob_commute_duration_minutes_present , 
                    "Bob's Arrival At Home" = bob_Arrival_Home_Time_present, 
                    "Later Arrival" =  Later_Arrival_Home , 
                    "Walk Time to Restaurant (Minutes)" = walk_time_duration_minutes_present,
                    "Arrival Time At Restaurant"= Arrival_Restaurant,
                    "Made the 6:15pm Reservation?" =MadeItInTime)  #,
                    #rownum,ArrivalTime_Percentile,rownum_MadeitGroup,ArrivalTime_MadeItGroup_Percentile)
       
    outdf_prettified
  })
  
  output$arrivalhistogram <- renderPlot({
    outdf_r() %>%
      ggplot(aes(x = Arrival_Restaurant,fill=MadeItInTime)) +
      geom_histogram(bins=100)+scale_x_continuous(breaks=seq(0,24,(1/12))) + ggtitle("Arrival Time at Restaurant, X-Axis Breaks are 5-Minute Intervals")
	  
  })
  
  
  #cdf of all arrival times
  output$arrivaltime_cdf <- renderPlot({
    outdf_r() %>%
    
      ggplot(aes(x = Arrival_Restaurant,y=ArrivalTime_Percentile))+
      geom_line()+
      scale_x_continuous(breaks=seq(0,24,(1/12)))+
      scale_y_continuous(breaks=seq(0,1,0.02))+
      #vertical line at mean time
      geom_vline(xintercept = mean(outdf_r()$Arrival_Restaurant),
                 color='blue') +
      #vertical line at median time
      geom_vline(xintercept = median(outdf_r()$Arrival_Restaurant),
                 color='green') +
      ggtitle(paste(
        "CDF of Arrival Times. Mean (Blue) ",
        mean(outdf_r()$Arrival_Restaurant) %/%1 - 12,
        ":",
        ifelse(round((mean(outdf_r()$Arrival_Restaurant) %%1)*60)<10,"0",""),
        round(mean(outdf_r()$Arrival_Restaurant) %%1 *60),
        "pm & Median (Green) ",
        median(outdf_r()$Arrival_Restaurant) %/%1 - 12,
        ":",
        ifelse(round((median(outdf_r()$Arrival_Restaurant) %%1)*60)<10,"0",""),
        round(median(outdf_r()$Arrival_Restaurant) %%1 *60),
        "pm",sep=''))
    
  })
  
  
  
  output$arrivaltime_cdf_madeitintime <- renderPlot({
    outdf_r() %>%
      dplyr::filter(MadeItInTime=="Yes") %>%
      ggplot(aes(x = Arrival_Restaurant,y=ArrivalTime_MadeItGroup_Percentile))+
      geom_line()+
      scale_x_continuous(breaks=seq(0,24,(1/12)))+
      scale_y_continuous(breaks=seq(0,1,0.02))+
      #vertical line at mean time
      geom_vline(xintercept = mean((outdf_r() %>% dplyr::filter(MadeItInTime=="Yes"))$Arrival_Restaurant),
                 color='blue') +
      #vertical line at median time
      geom_vline(xintercept = median((outdf_r() %>% dplyr::filter(MadeItInTime=="Yes"))$Arrival_Restaurant),
                 color='green') +
      ggtitle(paste(
        "CDF of Arrival Times Before 6:15pm. Mean (Blue) ",
        mean((outdf_r() %>% dplyr::filter(MadeItInTime=="Yes"))$Arrival_Restaurant) %/%1 - 12,
        ":",
        ifelse(round((mean((outdf_r() %>% dplyr::filter(MadeItInTime=="Yes"))$Arrival_Restaurant) %%1)*60)<10,"0",""),
        round((mean((outdf_r() %>% dplyr::filter(MadeItInTime=="Yes"))$Arrival_Restaurant) %%1)*60),
        "pm & Median (Green) ",
        median((outdf_r() %>% dplyr::filter(MadeItInTime=="Yes"))$Arrival_Restaurant) %/% 1 - 12,
        ":",
        ifelse(round((median((outdf_r() %>% dplyr::filter(MadeItInTime=="Yes"))$Arrival_Restaurant) %%1)*60)<10,"0",""),
        round((median((outdf_r() %>% dplyr::filter(MadeItInTime=="Yes"))$Arrival_Restaurant) %%1)*60),
        "pm",sep=''))
    
  })
  
  output$arrivaltime_cdf_didntmakeitintime <- renderPlot({
    outdf_r() %>%
      dplyr::filter(MadeItInTime=="No") %>%
      ggplot(aes(x = Arrival_Restaurant,y=ArrivalTime_MadeItGroup_Percentile))+
      geom_line()+
      scale_x_continuous(breaks=seq(0,24,(1/12)))+
      scale_y_continuous(breaks=seq(0,1,0.02))+
      #vertical line at mean time
      geom_vline(xintercept = mean((outdf_r() %>% dplyr::filter(MadeItInTime=="No"))$Arrival_Restaurant),
                 color='blue') +
      #vertical line at median time
      geom_vline(xintercept = median((outdf_r() %>% dplyr::filter(MadeItInTime=="No"))$Arrival_Restaurant),
                 color='green') +
      ggtitle(paste(
        "CDF of Arrival Times After 6:15pm. Mean (Blue) ",
        mean((outdf_r() %>% dplyr::filter(MadeItInTime=="No"))$Arrival_Restaurant) %/%1 - 12,
        ":",
        ifelse(round((mean((outdf_r() %>% dplyr::filter(MadeItInTime=="No"))$Arrival_Restaurant) %%1)*60)<10,"0",""),
        round((mean((outdf_r() %>% dplyr::filter(MadeItInTime=="No"))$Arrival_Restaurant) %%1)*60),
        "pm & Median (Green) ",
        median((outdf_r() %>% dplyr::filter(MadeItInTime=="No"))$Arrival_Restaurant) %/% 1 - 12,
        ":",
        ifelse(round((median((outdf_r() %>% dplyr::filter(MadeItInTime=="No"))$Arrival_Restaurant) %%1)*60)<10,"0",""),
        round((median((outdf_r() %>% dplyr::filter(MadeItInTime=="No"))$Arrival_Restaurant) %%1)*60),
        "pm",sep=''))
    
  })
  
  
  
  
  output$ralphcommute_histogram <- renderPlot({
    outdf_r() %>%
      ggplot(aes(x = (ralph_commute_duration_minutes),
                 fill= dplyr::if_else(ralph_commute_duration_minutes>=input$commutemean,"Above Average","Below Average"))) +
      geom_histogram(bins=100) + 

  ggtitle("Ralph's Commute Time Histogram (1SD & 2SD Marked, 3SD Clipped)")+
      guides(fill=guide_legend(title="Commute Above or Below Average"))+
      xlab("Ralph's Commute Time") +
	  scale_x_continuous(breaks=seq(-100,100,2), limits = c((input$commutemean- 3*input$commutesd),(input$commutemean + 3*input$commutesd)))+
	  geom_vline(xintercept=(input$commutemean- input$commutesd),color="black") +
	  geom_vline(xintercept=(input$commutemean- 2*input$commutesd),color="black") +
	  geom_vline(xintercept=(input$commutemean+ input$commutesd),color="black") +
	  geom_vline(xintercept=(input$commutemean+ 2*input$commutesd),color="black") 
	  
  })
  
  output$bobcommute_histogram <- renderPlot({
  
  
    outdf_r() %>%
      ggplot(aes(x = bob_commute_duration_minutes,
                 fill= dplyr::if_else(bob_commute_duration_minutes>=input$commutemean,"Above Average","Below Average"))) +
      geom_histogram(bins=100) + 
	  ggtitle("Bob's Commute Time Histogram (1SD & 2SD Marked, 3SD Clipped)") +
      guides(fill=guide_legend(title="Commute Above or Below Average"))+
      xlab("Bob's Commute Time")+
	  scale_x_continuous(breaks=seq(-100,100,2), limits = c((input$commutemean- 3*input$commutesd),(input$commutemean + 3*input$commutesd)))+
	  geom_vline(xintercept=(input$commutemean- input$commutesd),color="black") +
	  geom_vline(xintercept=(input$commutemean- 2*input$commutesd),color="black") +
	  geom_vline(xintercept=(input$commutemean+ input$commutesd),color="black") +
	  geom_vline(xintercept=(input$commutemean+ 2*input$commutesd),color="black")
  })
  
  
  output$walktime_histogram <- renderPlot({
    outdf_r() %>%
      ggplot(aes(x = (walk_time_duration_minutes),
                 fill = dplyr::if_else(walk_time_duration_minutes>=input$walktimemean,"Above Average","Below Average")))+
      geom_histogram(bins=100) + ggtitle("Walk Time To Restaurant Histogram (1SD & 2SD Marked, 3SD Clipped)") +
      guides(fill=guide_legend(title="Walk Time Above or Below Average"))+
      xlab("Walk Time To Restaurant")+
	  scale_x_continuous(breaks=seq(-100,100,1), limits = c((input$walktimemean- 3*input$walktimesd),(input$walktimemean + 3*input$walktimesd)))+
	  geom_vline(xintercept=(input$walktimemean- input$walktimesd),color="black") +
	  geom_vline(xintercept=(input$walktimemean- 2*input$walktimesd),color="black") +
	  geom_vline(xintercept=(input$walktimemean+ input$walktimesd),color="black") +
	  geom_vline(xintercept=(input$walktimemean+ 2*input$walktimesd),color="black")
  })
  

  
  
  output$freqtable <- DT::renderDataTable({ outdf_r() %>% dplyr::group_by(MadeItInTime) %>% 
      summarise(Count=n()) %>% 
      dplyr::mutate(Percent= scales::percent(Count/sum(Count))) %>%
      dplyr::rename('Made It In Time?'= MadeItInTime) })
	  
  
}
# Run the application 
shinyApp(ui = ui, server = server)
##