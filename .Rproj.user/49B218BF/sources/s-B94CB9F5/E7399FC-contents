library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)
library(ggplot2)

# dashboard page
# dashboard header
# dashboard sidebar -> sidebar menu -> menu item (tab name)
# dashboard body -> tab items -> tab item -> fluid row -> column 
dashboardPage(skin="black",
              dashboardHeader(title=tags$em("ALS stages prediction app", 
                                            style="text-align:center;color:#006600;font-size:100%"),
                              titleWidth = 800), 
              dashboardSidebar(width = 250,
                               sidebarMenu(
                                 br(),
                                 menuItem(tags$em("Upload Test Data",style="font-size:120%"),icon=icon("upload"),tabName="data"),
                                 menuItem(tags$em("Download Predictions",style="font-size:120%"),icon=icon("download"),tabName="download")
                                 )
                               ),
              dashboardBody(
                tabItems(
                  tabItem(tabName="data",
                          tags$h4("With this prediction app, you can upload your data and get back survival predictions.
                                  The prediction is based on Cox proportional hazard regression model that was trained using the PROACT dataset. 
                                  Suppose you have an ALS patient's data and would like to know his/her prognosis in terms of survival, 
                                  you can use this app for the prediction.", 
                                  style="font-size:150%"),
                          
                          br(),

                          tags$h4("To predict using this model, upload test data in csv format by using the button below.", 
                                  style="font-size:150%"),
                          
                          tags$h4("Then, go to the", 
                                  tags$span("Download Survival Predictions",style="color:red"),
                                  tags$span("section in the sidebar to see the survival plot and download the survival predictions."), style="font-size:150%"),
                          
                          br(),
                          br(),
                          br(),
                          column(width = 4,
                                 fileInput('file1', 
                                           em('Upload test data in csv format ',style="text-align:center;color:blue;font-size:150%"), 
                                           multiple = FALSE,
                                           accept=c('.csv')),
                                 
                                 uiOutput("input_data_heading"),
                                 tableOutput("input_data_table"),
                                 
                                 br(),
                                 br(),
                                 br(),
                                 br()
                          ),
                          br()
                          
                  ),
                  
                  
                  tabItem(tabName="download",
                          fluidRow(
                            column(width = 8,
                                   tags$h4("After you upload a test dataset, 
                                   you can download the predictions in csv format by
                                    clicking the button below.", 
                                           style="font-size:150%")
                                   )
                          ),
                          br(),
                          fluidRow(
                            column(width = 7,
                                   downloadButton("downloadData", em('Download Predictions',style="text-align:center;color:blue;font-size:150%"))
                            )
                          ),
                          br(),
                          br(),
                          fluidRow(
                            column(width = 7,
                                   plotOutput('plot_predictions')
                            )
                          )
                  )
                )
              )
)
