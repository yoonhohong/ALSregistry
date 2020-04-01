library(shiny)

library(DT)
library(dplyr)
library(ggplot2)
library(plotly)

source("global.R")

ui = fluidPage(
    titlePanel(paste("SNU ALS Registry", date_update_registry, sep = " ")), 
    fluidRow(
        column(4,  
            selectInput("dx", "Diagnosis", c("ALS", "PMA", "PLS", "PBP", 
                                             "FAS", "FLS", "SBMA", "BFA", 
                                             "MMN", "Others"), multiple = TRUE, 
                        selected = "ALS"), 
            dateInput("date_dx", "Diagnosis after", 
                      value = date_update_registry - 365*5, format = "yyyy-mm-dd"), 
            dateInput("date_fu_latest", "Latest FU after", 
                      value = date_update_registry - 365*4, format = "yyyy-mm-dd"), 
            radioButtons("fu_dur", "FU duration (longer than)", 
                         c("0 or longer" = 0,
                           "12 wk (3 mo)" = 12, 
                           "24 wk (6 mo)" = 24, 
                           "48 wk (12 mo)" = 48, 
                           "96 wk (24 mo)" = 96), 
                         selected = 48), 
            radioButtons("time_from_latest_visit", "Time from the latest visit (longer than)", 
                         c("0 or longer" = 0, 
                           "12 wk (3 mo)" = 12,
                           "24 wk (6 mo)" = 24), 
                         selected = 24),
            selectInput("fu_status", "F/U status", 
                        c("Undefined", "Death_or_tracheostomy",
                          "Refer", "Lost_to_fu"), multiple = T, 
                        selected = "Undefined"), 
            selectInput("bio", "Biorepository", 
                        c("CSF", "SERUM", "PLASMA", "BUFFYCOAT", "URINE"), multiple = T, 
                        selected = "CSF")
        ),
        
        
        column(5, 
               hr(),
               DTOutput('pt_list_tbl')
               )
        ), 
    
    fluidRow(
      p(class = 'text-center', downloadButton('filteredData', 'Download Filtered Data'))
    ),
    hr(),
  
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
               h5(textOutput("SER")),
               h5(textOutput("PLA")), 
               h5(textOutput("BUF")),
               h5(textOutput("CSF")),
               h5(textOutput("URN"))
               ), 
        column(4, 
               h4("ALSFRS revised (total score)"),
               plotOutput("plot_alsfrs")),
        column(4,
               h4("FVC (% of predicted)"),
               plotOutput("plot_FVC"))
            )
    )

server = function(input, output, session){
    
    selected_id1 = reactive({
      intersect(
        intersect(
          intersect(
            intersect(
              intersect(
                intersect(subset(base, Dx %in% input$dx)$Study_ID, 
                          subset(fu, Visit_interval >= input$fu_dur)$Study_ID), 
                subset(dx, Date_dx > input$date_dx)$Study_ID),
              subset(fu, Date_visit > input$date_fu_latest)$Study_ID),
            subset(fu, Time_from_latest_visit >= input$time_from_latest_visit)$Study_ID),
          subset(close_with_latest_visit, Close_reason %in% input$fu_status)$Study_ID), 
        subset(bio, sample %in% input$bio)$Study_ID)
      })
    
    output$pt_list_tbl = renderDT({
        temp_tbl = subset(base, Study_ID %in% selected_id1())
        temp_tbl
        }, selection = "single", rownames = FALSE)
    
    # download the filtered data
    output$filteredData = downloadHandler('snuALSregistry_filtered.csv', 
                                          content = function(file) {
                                            temp_tbl = subset(base, Study_ID %in% selected_id1())
                                            write.csv(temp_tbl, file, row.names = F)})
    
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
    
    output$SER = renderText({
        if (is.null(selected_id2())) {
            print("SER:")
        } else {
            temp = ser_temp %>%
                filter(Study_ID %in% selected_id2()) 
            if (dim(temp)[1] == 0) {
                paste("SER: ", "None", sep = "")
            } else {
                paste("SER: ", 
                      paste(temp$Sample_count, collapse = "+"))
            }}
      })
    
    output$PLA = renderText({
      if (is.null(selected_id2())) {
        print("PLA:")
      } else {
        temp = pla_temp %>%
          filter(Study_ID %in% selected_id2()) 
        if (dim(temp)[1] == 0) {
          paste("PLA: ", "None", sep = "")
        } else {
          paste("PLA: ", 
                paste(temp$Sample_count, collapse = "+"))
        }}
    })
    
    output$BUF = renderText({
      if (is.null(selected_id2())) {
        print("BUF:")
      } else {
        temp = buf_temp %>%
          filter(Study_ID %in% selected_id2()) 
        if (dim(temp)[1] == 0) {
          paste("BUF: ", "None", sep = "")
        } else {
          paste("BUF: ", 
                paste(temp$Sample_count, collapse = "+"))
        }}
    })
    
    output$CSF = renderText({
      if (is.null(selected_id2())) {
        print("CSF:")
      } else {
        temp = csf_temp %>%
          filter(Study_ID %in% selected_id2()) 
        if (dim(temp)[1] == 0) {
          paste("CSF: ", "None", sep = "")
        } else {
          paste("CSF: ", 
                paste(temp$Sample_count, collapse = "+"))
        }}
    })
    
    output$URN = renderText({
      if (is.null(selected_id2())) {
        print("URN:")
      } else {
        temp = urn_temp %>%
          filter(Study_ID %in% selected_id2()) 
        if (dim(temp)[1] == 0) {
          paste("URN: ", "None", sep = "")
        } else {
          paste("URN: ", 
                paste(temp$Sample_count, collapse = "+"))
        }}
    })
}

shinyApp(ui, server)

