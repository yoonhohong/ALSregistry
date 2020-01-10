library(shiny)

library(DT)
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
            dateInput("date_last_fu", "Latest FU (since) # recent 4 year (default)", 
                      value = Sys.Date() - 365*4, format = "yyyy-mm-dd"), 
            hr()
        ),
        column(6, 
               DTOutput('pt_list_tbl')
               )
    ), 
    
    fluidRow(
        column(6, 
               h4("ALSFRS revised (total score)"),
               plotOutput("plot_alsfrs")),
        column(6,
               h4("FVC (% of predicted)"),
               plotOutput("plot_FVC"))
            ), 
    
    fluidRow(
        hr(),
        column(4, 
               h5(textOutput("date_dx")), 
               h5(textOutput("onset2dx")),  
               h5(textOutput("fu_duration")), 
               h5(textOutput("onset_region"))
        ), 
        column(4, 
               h5(textOutput("gastrostomy")), 
               h5(textOutput("NIV")),
               h5(textOutput("tracheostomy")), 
               h5(textOutput("death"))
        ), 
        column(4, 
               h5(textOutput("Biobank"))
        )
    )
)
        

server = function(input, output){
    
    selected_id1 = reactive({
        intersect(
            intersect(
                intersect(subset(base, Dx %in% input$dx)$Study_ID, 
                          subset(fu, Visit_interval * 4 > input$fu_dur)$Study_ID), 
                subset(dx, Date_dx > input$date_dx)$Study_ID),
            subset(fu, Date_visit > input$date_last_fu)$Study_ID)
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
        paste("Date of Dx: ", format(temp, "%Y-%m"))
        })
    
    output$onset2dx = renderText({
        temp1 = dx[dx$Study_ID %in% selected_id2(),]
        temp2 = round(difftime(temp1$Date_dx, temp1$Date_onset, units = "weeks"))
        paste("Time from onset to Dx (weeks): ", temp2)
        })
    
    output$fu_duration = renderText({
        temp1 = fu[fu$Study_ID %in% selected_id2(),]
        temp2 = round(difftime(last(temp1$Date_visit), first(temp1$Date_visit),
                               units = "weeks"))
        paste("FU duration (weeks): ", temp2)
        })
    
    output$onset_region = renderText({
        temp = dx[dx$Study_ID %in% selected_id2(),]$Onset_region
        paste("Onset region: ", temp)
    })
    
    output$gastrostomy = renderText({
        temp = event[event$Study_ID %in% selected_id2(),]$Date_event
        temp = ifelse(is.null(temp), "NA", temp)
        paste("Gastrostomy (date): ", temp)
    })
    
    output$NIV = renderText({
        temp = event[event$Study_ID %in% selected_id2(),]$Date_event
        temp = ifelse(is.null(temp), "NA", temp)
        paste("NIV (date): ", temp)
    })
    
    output$tracheostomoy = renderText({
        temp = event[event$Study_ID %in% selected_id2(),]$Date_event
        temp = ifelse(is.null(temp), "NA", temp)
        paste("Tracheostomy (date): ", temp)
    })
    
    output$death = renderText({
        temp = event[event$Study_ID %in% selected_id2(),]$Date_event
        temp = ifelse(is.null(temp), "NA", temp)
        paste("Death (date): ", temp)
    })
    
}

shinyApp(ui, server)

