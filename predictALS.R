if (!requireNamespace("shiny")){
  install.packages("shiny")
}

if (!requireNamespace("readxl")){
  install.packages("readxl")
}

if (!requireNamespace("DT")){
  install.packages("DT")
}

if (!requireNamespace("ggplot2")){
  install.packages("ggplot2")
}

if (!requireNamespace("survival")){
  install.packages("survival")
}

if (!requireNamespace("survminer")){
  install.packages("survminer")
}

# 
# if (!requireNamespace("prodlim")){
#   install.packages("prodlim")
# }
# 
# if (!requireNamespace("foreach")){
#   install.packages("foreach")
# }
# 
# if (!requireNamespace("rms")){
#   install.packages("rms")
# }
# 
# if (!requireNamespace("timereg")){
#   install.packages("timereg")
# }
# 
# if (!requireNamespace("pec")){
#   install.packages("pec")
# }


library(shiny)
library(readxl)
library(DT)
library(ggplot2)
library(survival)
library(survminer)
# library(prodlim)
# library(foreach)
# library(rms)
# library(timereg)
# library(pec)

source("Predict_survival.R")
# load("fitcox.rda", .GlobalEnv) # fitcox, a cox model fitted to the whole PROACT dataset
# load("pec_cox.rda", .GlobalEnv) # prediction error curves in a test dataset


ui <- fluidPage(
  titlePanel("Individualized prediction of ALS"), 
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "file1", 
                label = "Upload data file",
                accept = c(".xls"))
    ), 
    mainPanel(
      tabsetPanel(
        tabPanel("Input data", br(), DTOutput("input_data_table")), 
        tabPanel("Survival prediction", br(), plotOutput("plot_predictions"))
        )
      )
    )
  )
                              

server <- function(input, output) {
  
  input_data <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      ext <- tools::file_ext(inFile$name)
      file.rename(inFile$datapath,
                  paste(inFile$datapath, ext, sep="."))
      input_dataset =  read_excel(paste(inFile$datapath, ext, sep = "."), 
                                  sheet = 1, 
                                  col_types = c("text", rep("numeric", 12))) 
      }
  })
  
  output$input_data_table = renderDT({    # show sample of uploaded data
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      input_data()
    }
  })
  

  output$plot_predictions = renderPlot({   # the last 6 rows to show
    inFile <- input$file1

    if (is.null(inFile)){
      return(NULL)
    }else{
      temp = input_data()
      newfit <- survfit(fitcox, newdata = temp)
      ggsurvplot(newfit, data = temp, conf.int = FALSE,
                 censor = FALSE, surv.median.line = "hv",
                 ggtheme = theme_minimal())
      }
  })
}

shinyApp(ui = ui, server = server)

