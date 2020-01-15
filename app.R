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
            dateInput("date_dx", "Diagnosis after", 
                      value = Sys.Date() - 365*5, format = "yyyy-mm-dd"), 
            numericInput("fu_dur", "FU duration (months) longer than", value = 12), 
            dateInput("date_fu_latest", "Latest FU after", 
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
               h5(textOutput("date_dx"), style="color:red"), 
               h5(textOutput("onset2dx")),  
               h5(textOutput("onset_region")), 
               h5(textOutput("fu_duration")), 
               h5(textOutput("date_fu_latest"), style="color:red"),
               hr(), 
               h5(textOutput("gastrostomy")), 
               h5(textOutput("NIV")),
               h5(textOutput("tracheostomy")), 
               hr(),
               h5(textOutput("death"), style="color:red"), 
               h5(textOutput("refer")), 
               h5(textOutput("lost_fu")), 
               hr(),
               h5(textOutput("Biobank"))
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
                          subset(fu, Visit_interval/4 > input$fu_dur)$Study_ID), 
                subset(dx, Date_dx > input$date_dx)$Study_ID),
            subset(fu, Date_visit > input$date_fu_latest)$Study_ID)
    })
    
    output$pt_list_tbl = renderDT({
        subset(base, Study_ID %in% selected_id1())
        }, selection = "single", rownames = FALSE)
    
    selected_id2 = reactive({
        temp = subset(base, Study_ID %in% selected_id1())
        s = input$pt_list_tbl_rows_selected
        if (is.null(s)) {
            return(NULL)
        } else {
            temp[s,]$Study_ID
            }
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
        if (is.null(selected_id2())) {
            print("Dx: ")
        } else {
            temp = dx[dx$Study_ID %in% selected_id2(),]
            paste("Dx: ", format(temp$Date_dx, "%Y.%m"))
            }
        })
    
    output$onset2dx = renderText({
        if (is.null(selected_id2())) {
            print("Time from onset to Dx (months): ")
        } else {
            temp1 = dx[dx$Study_ID %in% selected_id2(),]
            temp2 = round(difftime(temp1$Date_dx, temp1$Date_onset, units = "weeks"))
            paste("Time from onset to Dx (months): ", round(temp2/4))
            }
        })
    
    output$onset_region = renderText({
        if (is.null(selected_id2())) {
            print("Onset region: ")
        } else {
            temp = dx[dx$Study_ID %in% selected_id2(),]$Onset_region
            paste("Onset region: ", temp)
        }
        })
    
    output$fu_duration = renderText({
        if (is.null(selected_id2)) {
            print("FU duration (months):")
        } else {
            temp1 = fu[fu$Study_ID %in% selected_id2(),]
            temp2 = round(difftime(last(temp1$Date_visit), first(temp1$Date_visit),
                                   units = "weeks"))
            paste("FU duration (months): ", round(temp2/4))
            }
        })
    
    output$date_fu_latest = renderText({
        if (is.null(selected_id2())) {
            print("Latest FU: ")
        } else {
            temp = fu[fu$Study_ID %in% selected_id2(),]
            paste("Latest FU: ", format(last(temp$Date_visit), "%Y.%m.%d"))
            }
        })

    output$gastrostomy = renderText({
        if (is.null(selected_id2())) {
            print("Gastrostomy:")
        } else {
            temp = event %>% 
                filter(Study_ID %in% selected_id2()) %>%
                filter(Event == "Gastrostomy")
            if (dim(temp)[1] == 0) {
                paste("Gastrostomy: ", NA)
            } else {
                paste("Gastrostomy: ", format(temp$Date_event, "%Y.%m.%d"))
            }
        }
    })
    
    output$NIV = renderText({
        if (is.null(selected_id2())) {
            print("NIV:")
        } else {
            temp = event %>% 
                filter(Study_ID %in% selected_id2()) %>%
                filter(Event == "NIV")
            if (dim(temp)[1] == 0) {
                paste("NIV: ", NA)
            } else {
                paste("NIV: ", format(temp$Date_event, "%Y.%m.%d"))
            }
        }
    })
    
    output$tracheostomy = renderText({
        if (is.null(selected_id2())) {
            print("Tracheostomy:")
        } else {
            temp = event %>% 
                filter(Study_ID %in% selected_id2()) %>%
                filter(Event == "Tracheostomy")
            if (dim(temp)[1] == 0) {
                paste("Tracheostomy: ", NA)
            } else {
                paste("Tracheostomy: ", format(temp$Date_event, "%Y.%m.%d"))
            }
        }
    })
    
    output$death = renderText({
        if (is.null(selected_id2())) {
            print("Death:")
        } else {
            temp = close %>%
                filter(Study_ID %in% selected_id2()) %>%
                filter(Close_reason == "Death")
            if (dim(temp)[1] == 0) {
                paste("Death: ", NA)
            } else {
                paste("Death: ", format(temp$Date_close, "%Y.%m.%d"))
            }
        }
    })
    
    output$lost_fu = renderText({
        if (is.null(selected_id2())) {
            print("Lost to f/u:")
        } else {
            temp = close %>%
                filter(Study_ID %in% selected_id2()) %>%
                filter(Close_reason == "Lost to f/u")
            if (dim(temp)[1] == 0) {
                paste("Lost to f/u: ", NA)
            } else {
                paste("Lost to f/u: ", format(temp$Date_close, "%Y.%m.%d"))
            }
        }
    })
    
    output$refer = renderText({
        if (is.null(selected_id2())) {
            print("Refer:")
        } else {
            temp = close %>%
                filter(Study_ID %in% selected_id2()) %>%
                filter(Close_reason == "Refer")
            if (dim(temp)[1] == 0) {
                paste("Refer: ", NA)
            } else {
                paste("Refer: ", format(temp$Date_close, "%Y.%m.%d"))
            }
        }
    })
}

shinyApp(ui, server)

