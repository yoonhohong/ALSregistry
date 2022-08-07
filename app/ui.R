library(shiny)
library(DT)
library(plotly)

date_update_registry = Sys.Date()

ui = fluidPage(
  
    titlePanel(paste("SNU ALS Registry", date_update_registry, sep = " ")), 
    
    fluidRow(

        column(3,
               wellPanel(
                 h4("Set filters"),
                 br(),
                 selectInput("dx", "Diagnosis", 
                             c("ALS", "PMA", "PLS", "PBP", 
                               "FAS", "FLS", "SBMA", "BFA", 
                               "MMN", "Others"), 
                             multiple = TRUE, 
                             selected = "ALS"), 
                 dateRangeInput("range_date_enrollment", "Enrollment", 
                                start = date_update_registry - as.difftime(365*5, units = "days"),
                                end = date_update_registry), 
                 dateRangeInput("range_date_course", "Latest course update", 
                                start = date_update_registry - as.difftime(365*5, units = "days"),
                                end = date_update_registry - as.difftime(30*6, units = "days")), 
                 checkboxInput("not_closed", "Not closed?"), 
                 checkboxGroupInput("filter", "Filters", 
                                    choices = c("Diagnosis" = "dx", 
                                                "Date of enrollment" = "date_enrollment", 
                                                "Date of latest course update" = "date_update",
                                                "Not closed" = "not_closed"), 
                                    selected = c("dx", "date_enrollment", "date_update", "not_closed")),
                 br(),
                 p(class = "text-center", 
                   actionButton("applyFilter", "Apply filters"))
                 ) # wellPanel 
        ), # column 
        
        column(4, offset = 0.3,
               h4("Filtered data"),
               DTOutput('pt_list_tbl'),
               br(),
               p(class = 'text-center', 
                 downloadButton('filteredData', 'Download Filtered Data'))
               ), 
                      
        column(3, offset = 0.6, 
               wellPanel(
                 h4("Patient summary"), 
                 hr(),
                 h5(textOutput("date_onset")),  
                 h5(textOutput("date_dx")),
                 h5(textOutput("date_enrollment")), 
                 h5(textOutput("date_fu")),
                 h5(textOutput("onset_region")), 
                 hr(), 
                 h5(textOutput("gastrostomy")), 
                 h5(textOutput("NIV")),
                 h5(textOutput("tracheostomy")),
                 h5(textOutput("wc_ridden")),
                 hr(),
                 h5(textOutput("death")), 
                 h5(textOutput("refer")), 
                 h5(textOutput("lost_fu"))
               ) # wellPanel 
               ) # column 
        ), # fluidRow 
        
    hr(),
    
    fluidRow(
      
      column(4,
             h4("ALSFRS revised (total score)"),
             plotOutput("plot_alsfrs")
             ),
      
      column(4, 
             h4("FVC (% of predicted)"),
             plotOutput("plot_FVC")
             ), 
      
      column(4, 
             h4("Weight"), 
             plotOutput("plot_wt"))
    ) # fluidRow 
  ) # fluidPage 

