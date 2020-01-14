library(shiny)

library(DT)
library(dplyr)
library(ggplot2)
library(plotly)

source("global.R")

ui = fluidPage(
    titlePanel("SNU ALS Registry"), 
    fluidRow(
        column(4,  
            selectInput("dx", "Diagnosis", c("ALS", "PMA", "PLS", "PBP", 
                                             "FAS", "FLS", "SBMA", "BFA", 
                                             "MMN", "Others"), multiple = TRUE, 
                        selected = "ALS"), 
            dateInput("date_dx", "Diagnosis (since) # 5 years ago (default)", 
                      value = Sys.Date() - 365*5, format = "yyyy-mm-dd"), 
            sliderInput("fu_dur", "FU duration (>, months)", min = 0, max = 24, value = 12), 
            dateInput("date_fu_last", "Latest FU (since) # recent 4 year (default)", 
                      value = Sys.Date() - 365*4, format = "yyyy-mm-dd")
        ),
        column(5, 
               DTOutput('pt_list_tbl')
               )
        ), 
    
    fluidRow(
        column(4, 
               h4("DX & FU Hx"), 
               hr(),
               h6(textOutput("date_dx"), style="color:red"), 
               h6(textOutput("onset2dx")),  
               h6(textOutput("onset_region")), 
               h6(textOutput("fu_duration")), 
               h6(textOutput("date_fu_last"), style="color:red"),
               hr(), 
               h6(textOutput("gastrostomy")), 
               h6(textOutput("NIV")),
               h6(textOutput("tracheostomy")), 
               h6(textOutput("death")), 
               hr(), 
               h6(textOutput("Biobank"))
               ), 
        column(4, 
               h4("ALSFRS revised (total score)"),
               plotOutput("plot_alsfrs")),
        column(4,
               h4("FVC (% of predicted)"),
               plotOutput("plot_FVC"))
            )
    )


server = function(input, output){
    
    selected_id1 = reactive({
        intersect(
            intersect(
                intersect(subset(base, Dx %in% input$dx)$Study_ID, 
                          subset(fu, Visit_interval * 4 > input$fu_dur)$Study_ID), 
                subset(dx, Date_dx > input$date_dx)$Study_ID),
            subset(fu, Date_visit > input$date_fu_last)$Study_ID)
    })
    
    output$pt_list_tbl = renderDT({
        subset(base, Study_ID %in% selected_id1())
        }, selection = "single", rownames = FALSE)
    
    selected_id2 = reactive({
        temp = subset(base, Study_ID %in% selected_id1())
        s = input$pt_list_tbl_rows_selected
        if (is.null(s)) {
            return(NULL)
        }
        temp[s,]$Study_ID
    })
    
    output$plot_alsfrs = renderPlot({
        temp = fu %>%
            filter(!is.na(ALSFRS))
        temp1 = temp %>%
            filter(Study_ID %in% selected_id1())
        temp2 = temp %>%
            filter(Study_ID %in% selected_id2())
        ggplot() + 
            geom_line(data = temp1, 
                      aes(x=Visit_interval, y=ALSFRS, group = Study_ID), 
                   color="grey") + 
            geom_line(data = temp2, 
                      aes(x=Visit_interval, y=ALSFRS, group = Study_ID), color = "red") + 
            geom_point(data = temp2, 
                aes(x=Visit_interval, y=ALSFRS), color = "red") + 
            theme_light(base_size = 16) + 
            theme(legend.position = "none") +
            xlab("Time from enrollment (weeks)") +
            ylab("ALSFRS-R")
        })
    
    output$plot_FVC = renderPlot({
        temp = fu %>%
            filter(!is.na(FVC_percent))
        temp1 = temp %>%
            filter(Study_ID %in% selected_id1())
        temp2 = temp %>%
            filter(Study_ID %in% selected_id2())
        ggplot() + 
            geom_line(data = temp1, aes(x=Visit_interval, y=FVC_percent, group = Study_ID), 
                      color="grey") + 
            geom_line(data = temp2, 
                      aes(x=Visit_interval, y=FVC_percent, group = Study_ID), color = "red") + 
            geom_point(data = temp2, 
                       aes(x=Visit_interval, y=FVC_percent), color = "red") + 
            theme_light(base_size = 16) + 
            theme(legend.position = "none") +
            xlab("Time from enrollment (weeks)") +
            ylab("FVC (% of predicted)") + 
            ylim(c(0,max(temp1$FVC_percent)))
        })

    output$date_dx = renderText({
        temp = dx[dx$Study_ID %in% selected_id2(),]$Date_dx
        paste("Dx: ", format(temp, "%Y-%m"))
        })
    
    output$onset2dx = renderText({
        temp1 = dx[dx$Study_ID %in% selected_id2(),]
        temp2 = round(difftime(temp1$Date_dx, temp1$Date_onset, units = "weeks"))
        paste("Time from onset to Dx (weeks): ", temp2)
        })
    
    output$onset_region = renderText({
        temp = dx[dx$Study_ID %in% selected_id2(),]$Onset_region
        paste("Onset region: ", temp)
    })
    
    output$fu_duration = renderText({
        temp1 = fu[fu$Study_ID %in% selected_id2(),]
        temp2 = round(difftime(last(temp1$Date_visit), first(temp1$Date_visit),
                               units = "weeks"))
        paste("FU duration (weeks): ", temp2)
        })
    
    output$date_fu_last = renderText({
        temp1 = fu[fu$Study_ID %in% selected_id2(),]$Date_visit
        paste("Last fu: ", last(temp1))
    })

    output$gastrostomy = renderText({
        temp = event %>% 
            filter(Study_ID %in% selected_id2()) %>%
            filter(Event == "Gastrostomy")
        if (dim(temp)[1] == 0) {
            paste("Gastrostomy: ", NA)
        } else {
            paste("Gastrostomy: ", format(temp$Date_event, "%Y-%m-%d"))
        }
    })
    
    output$NIV = renderText({
        temp = event %>% 
            filter(Study_ID %in% selected_id2()) %>%
            filter(Event == "NIV")
        if (dim(temp)[1] == 0) {
            paste("NIV: ", NA)
        } else {
            paste("NIV: ", format(temp$Date_event, "%Y-%m-%d"))
        }
    })
    
    output$tracheostomy = renderText({
        temp = event %>% 
            filter(Study_ID %in% selected_id2()) %>%
            filter(Event == "Tracheostomy")
        if (dim(temp)[1] == 0) {
            paste("Tracheostomy: ", NA)
        } else {
            paste("Tracheostomy: ", format(temp$Date_event, "%Y-%m-%d"))
        }
    })
    
    output$death = renderText({
        temp = event[event$Study_ID %in% selected_id2(),]$Date_event
        temp = ifelse(is.null(temp), "NA", temp)
        paste("Death: ", temp)
    })
}

shinyApp(ui, server)

