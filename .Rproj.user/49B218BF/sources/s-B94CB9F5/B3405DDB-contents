library(tidyverse)
library(survival)
library(survminer)
library(shiny)
library(readr)
library(ggplot2)
library(plotly)

load("model_cox.rda") # fitcox, a cox model fitted to the whole PROACT dataset 
load("pec_cox.rda") # prediction error curves in a test dataset

shinyServer(function(input, output) {
  
  output$input_data_heading = renderUI({   # show only if data has been uploaded
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      tags$h4('Input data')
    }
  })
  
  input_data <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      input_data =  readr::read_csv(input$file1$datapath, col_names = TRUE)
      input_data
    }
  })
  
  output$input_data_table = renderTable({    # show sample of uploaded data
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      head(input_data())
    }
  })
  
  output$prediction_heading = renderUI({  # show only if data has been uploaded
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      tags$h4('Survival predictions')
    }
  })
  

  output$plot_predictions = renderPlot({   # the last 6 rows to show
    inFile <- input$file1

    if (is.null(inFile)){
      return(NULL)
    }else{
      plotPredictSurvProb(fitcox,newdata=input_data(),lty=1)
      }
  })

  # Downloadable csv of predictions ----
  
#   predictions = reactive({
#     inFile <- input$file1
#     
#     if (is.null(inFile)){
#       return(NULL)
#     }else{
#       cox_fit = survfit(model_cox, newdata = df_new())
#       df_new_prediction = as.data.frame(
#         summmary(cox_fit)$table[,c(7,8,9)])
#       df_new_prediction
#     }
#   })
#   
#   output$downloadData <- downloadHandler(
#     filename = function() {
#       paste("survival_predictions", ".csv", sep = "")
#     },
#     content = function(file) {
#       write.csv(df_new_prediction(), 
#         file, row.names = F)
#     })
})
  

