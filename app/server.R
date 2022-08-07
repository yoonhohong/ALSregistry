# ALS Registry 

library(tidyverse)
source("helper.R")

server = function(input, output){
  
  # input$dx 
  selected_id1 = reactive({
    base %>%
      filter(Dx %in% input$dx) %>%
      select(Study_ID)
    })
  
  # input$range_date_enrollment
  selected_id2 = reactive({
    dx %>%
      filter(Date_enrollment >= input$range_date_enrollment[1]) %>%
      filter(Date_enrollment <= input$range_date_enrollment[2]) %>%
      select(Study_ID)
    })
    
  # input$range_date_course 
  selected_id3 = reactive({
    course %>%
      group_by(Study_ID) %>%
      filter(Date == max(Date)) %>%
      filter(Date >= input$range_date_course[1]) %>%
      filter(Date <= input$range_date_course[2]) %>%
      select(Study_ID)
    })
  
  selected_id4 = reactive({
    if (input$not_closed) { # not_closed == TRUE 
      course %>%
        anti_join(close, by = "Study_ID") %>%
        select(Study_ID)
    } else {
      course %>%
        select(Study_ID)
    }
  })
  
  # input$applyFilter 
  # input$filter 
  
  filterset = eventReactive(input$applyFilter, {
    req(input$filter)
    input$filter # dx, date_enrollment, date_visit, event
  })
  
  selected_id5 = reactive({
    req(filterset())
    base %>% 
      filter(
        conditional("dx" %in% filterset(), Study_ID %in% selected_id1()$Study_ID),
        conditional("date_enrollment" %in% filterset(), Study_ID %in% selected_id2()$Study_ID), 
        conditional("date_update" %in% filterset(), Study_ID %in% selected_id3()$Study_ID), 
        conditional("not_closed" %in% filterset(), Study_ID %in% selected_id4()$Study_ID)
        )})
  
  output$pt_list_tbl = renderDT({
    selected_id5()
    }, selection = "single", rownames = FALSE)
  
  #    download the filtered data
  output$filteredData = downloadHandler('snuALSregistry_filtered.csv',
                                        content = function(file) {
                                          temp_tbl = base %>%
                                            filter(Study_ID %in% selected_id5()$Study_ID)
                                          write.csv(temp_tbl, file, row.names = F)})
  
  selected_id6 = reactive({
    if (is.null(input$pt_list_tbl_rows_selected)) {
      return(NULL)
    } else {
      temp = base %>%
        filter(Study_ID %in% selected_id5()$Study_ID)
      temp[input$pt_list_tbl_rows_selected,]
    }
  })
  
  output$plot_alsfrs = renderPlot({
    req(selected_id6())
    temp = fu_alsfrs %>%
      filter(Study_ID %in% selected_id6()$Study_ID)
    temp %>%
      ggplot(aes(Visit_interval, ALSFRS, group = Study_ID)) +
      geom_line(color = "red") +
      geom_point(color = "red") +
      theme_light(base_size = 16) + 
      labs(x= "Time from enrollment (months)", y = "ALSFRS-R") + 
      scale_x_continuous(limits = c(0, max(12, max(temp$Visit_interval)))) + 
      scale_y_continuous(limits = c(0, 50))
  })
  
  output$plot_FVC = renderPlot({
    req(selected_id6())
    temp = fu_fvc %>%
      filter(Study_ID %in% selected_id6()$Study_ID)
    temp %>% 
      ggplot(aes(Visit_interval, FVC, group = Study_ID)) +
      geom_line(color = "red") +
      geom_point(color = "red") +
      theme_light(base_size = 16) +
      labs(x="Time from enrollment (months)", y="FVC (% of predicted)") + 
      scale_y_continuous(limits = c(0, max(100, max(temp$FVC)))) + 
      scale_x_continuous(limits = c(0, max(12, max(temp$Visit_interval))))
    })
  
  output$plot_wt = renderPlot({
    req(selected_id6())
    temp = fu_wt %>%
      filter(Study_ID %in% selected_id6()$Study_ID)
    temp %>%
      ggplot(aes(Visit_interval, Wt, group = Study_ID)) +
      geom_line(color ="red") + 
      geom_point(color = "red") + 
      theme_light(base_size = 16) + 
      labs(x="Time from enrollment (months)", y = "Weight") + 
      scale_x_continuous(limits = c(0, max(12, max(temp$Visit_interval)))) + 
      scale_y_continuous(limits = c(0, max(80, max(temp$Wt))))
    })
  
  output$date_onset = renderText({
    req(selected_id6())
    temp = dx[dx$Study_ID %in% selected_id6()$Study_ID,]
    paste("Onset: ", format(temp$Date_onset, "%Y-%m"))
  })
  
  output$date_dx = renderText({
    req(selected_id6())   
    temp = dx[dx$Study_ID %in% selected_id6()$Study_ID,]
    paste("Dx: ", format(temp$Date_dx, "%Y-%m"))
  })
  
  output$date_enrollment = renderText({
    req(selected_id6())
    temp = dx[dx$Study_ID %in% selected_id6()$Study_ID,]
    paste("Enrollment: ", format(temp$Date_enrollment, "%Y-%m"))
  })
  
  output$onset_region = renderText({
    req(selected_id6())
    temp = dx[dx$Study_ID %in% selected_id6()$Study_ID,]$Onset_region
    paste("Onset region: ", temp)
  })
  
  output$date_fu = renderText({
    req(selected_id6())
    temp = fu[fu$Study_ID %in% selected_id6()$Study_ID,]
    paste("FU visits: ", 
            paste(format(temp[order(temp$Date),]$Date, "%Y-%m-%d"), 
                  collapse = ", "))
  })
  
  output$gastrostomy = renderText({
    req(selected_id6())
    temp = event %>%
      filter(Study_ID %in% selected_id6()$Study_ID) %>%
      filter(Event == "Gastrostomy")
    if (dim(temp)[1] == 0) {
      paste("Gastrostomy: ", NA)
    } else {
      paste("Gastrostomy: ", format(temp$Date_event, "%Y-%m-%d"))
    }
  })
  
  output$NIV = renderText({
    req(selected_id6())
    temp = event %>%
      filter(Study_ID %in% selected_id6()$Study_ID) %>%
      filter(Event == "NIV")
    if (dim(temp)[1] == 0) {
      paste("NIV: ", NA)
    } else {
      paste("NIV: ", format(temp$Date_event, "%Y-%m-%d"))
    }
  })
  
  output$tracheostomy = renderText({
    req(selected_id6())
    temp = event %>%
      filter(Study_ID %in% selected_id6()$Study_ID) %>%
      filter(Event == "Tracheostomy")
    if (dim(temp)[1] == 0) {
      paste("Tracheostomy: ", NA)
    } else {
      paste("Tracheostomy: ", format(temp$Date_event, "%Y-%m-%d"))
    }
  })
  
  output$wc_ridden = renderText({
    req(selected_id6())
    temp = event %>%
      filter(Study_ID %in% selected_id6()$Study_ID) %>%
      filter(Event == "WC-ridden")
    if (dim(temp)[1] == 0) {
      paste("WC-ridden: ", NA)
    } else {
      paste("WC-ridden: ", format(temp$Date_event, "%Y-%m-%d"))
    }
  })
  
  output$death = renderText({
    req(selected_id6())
    temp = close %>%
      filter(Study_ID %in% selected_id6()$Study_ID) %>%        
      filter(Close_reason == "Death")
    if (dim(temp)[1] == 0) {
      paste("Death: ", NA)
    } else {
      paste("Death: ", format(temp$Date_close, "%Y-%m-%d"))
    }
  })
  
  output$lost_fu = renderText({
    req(selected_id6())
    temp = close %>%
      filter(Study_ID %in% selected_id6()$Study_ID) %>%
      filter(Close_reason == "Lost to f/u")
    if (dim(temp)[1] == 0) {
      paste("Lost to f/u: ", NA)
    } else {
      paste("Lost to f/u: ", format(temp$Date_close, "%Y-%m-%d"))
    }
  })
  
  output$refer = renderText({
    req(selected_id6())
    temp = close %>%
      filter(Study_ID %in% selected_id6()$Study_ID) %>%
      filter(Close_reason == "Refer")
    if (dim(temp)[1] == 0) {
      paste("Refer: ", NA)
    } else {
      paste("Refer: ", format(temp$Date_close, "%Y-%m-%d"))
    }
  })
}
  
