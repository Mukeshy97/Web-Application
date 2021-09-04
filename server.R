library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(DT)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(psych)
library(corrplot)
library(htmltools)
library(GGally)
library(stats)
library(plotly)
library(ggpubr)
library(htmlwidgets)
library(shinythemes)

# Define server for application for Data Visualization
server <- function(input, output, session) {
  # Get the upload file
  myData <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    if (input$fileType_Input == "1") {
      myData<-read.csv(inFile$datapath, header = 
                         TRUE,stringsAsFactors = FALSE)
    } else {
      myData<-read_xlsx(inFile$datapath)
    }
  })
  
  output$contents <- DT::renderDataTable({
    DT::datatable(myData())       
  })
  
  observe({
    data <- myData()
    updateSelectInput(session, 'y', choices = names(data))
  }) 
  #gets the y variable name, will be used to change the plot legends
  yVarName <- reactive({
    input$y
  }) 
  
  observe({
    data <- myData()
    updateSelectInput(session, 'x', choices = names(data))
  }) 
  #gets the x variable name, will be used to change the plot legends
  xVarName <- reactive({
    input$x
  }) 
  #################################
  observe({
    data <- myData()
    updateSelectInput(session, 'Y', choices = names(data))
  }) 
  #gets the Y variable name, will be used to change the plot legends
  YVarName <- reactive({
    input$Y
  }) 
  
  observe({
    data <- myData()
    updateSelectInput(session, 'X', choices = names(data))
  }) 
  #gets the X variable name, will be used to change the plot legends
  XVarName <- reactive({
    input$X
  }) 
  # draw a checkbox input variables of uploaded file for xvariables..
  output$xvariables <- renderUI({
    df <- myData()
    x<-colnames(df)
    pickerInput(inputId = 'xvariable',
                label = 'Select x-axis variable',
                choices = c(x[1:length(x)]), selected=x[2],
                options = list(`style` = "btn-info"),
                multiple = TRUE)
    
  })
  # draw a checkbox input variables of uploaded file for xvariables..
  output$yvariables <- renderUI({
    df <- myData()
    y<-colnames(df) 
    pickerInput(inputId = 'yvariable',
                label = 'Select y-axis variable',
                choices = c(y[1:length(y)]), selected=y[1],
                options = list(`style` = "btn-info"),
                multiple = FALSE)
    
  })
  output$Refresh1 <- renderText({
    toString(format(Sys.Date(), format = "%d %b %Y"))
  })
  output$summary <- renderPrint({
    df <- myData()
    df <- df[,input$select_variable]
    describeBy(df)
    
  })
  # This has not been placed anywhere; there is no uiOutput with id of checkbox in your ui code
  output$sumcheckbox <- renderUI({
    df <- myData()
    checkboxGroupInput("select_variable", "Select Feature variables for Summary:",
                       names(df), selected = names(df))
  })
  
  # This has not been placed anywhere; there is no uiOutput with id of select in your ui code
  output$select <- renderUI({
    df <- myData()
    selectInput("variable", "Variable:",names(df))
  })
  output$checkbox <- renderUI({
    df <- myData()
    checkboxGroupInput("select_var", "Select Feature variables:",
                       names(df), selected = names(df))
  })
  #Draw a Intrective histogram for Given data variables. 
  output$Plothist <- renderPlotly({
    df <- myData()
    df <- df[,input$variable]
    # draw the histogram with the specified number of bins
    plot_ly(x = ~df,
            type = "histogram", nbinsx = 18,
            histnorm = "probability",
            marker = list(color = viridis::viridis_pal(option = "C", direction = -1)(18))) %>% 
      layout(title = "Histogram of Data",
             yaxis = list(title = "Frequency",
                          zeroline = FALSE)
      )
    
  })
  # for Barchart
  output$plot <- renderPlot({
    df <- myData()
    ggplot(df, aes_string(x = input$x, y = input$y))+
      geom_bar(stat = "identity", fill="blue", nbinsx=15)
  })
  # For Scatter plot and Boxplot
  output$plot1 <- renderPlot({
    df <- myData()
    p <- ggplot(df, mapping = aes_string(x = input$x, y = input$y))
    if (input[["plot_type"]] == "scatter plot")
    {
      p + geom_point() + stat_smooth(method = "lm", col = "blue")
    }
    else
    {
      p + geom_boxplot()
    }
  }) 
  # For Correlation matrix and Correlation plot..
  output$Correlation <- renderPrint({
    df <- myData()
    df <- df[,input$select_var]
    round(cor(df),4)
  })
  output$Correlationplot <- renderPlot({
    df <- myData()
    df <- df[,input$select_var]
    theme_lato <- theme_minimal(base_size=8, base_family="Lato Light")
    ggpairs(df, lower = list(continuous = function(...) 
      ggally_smooth(..., colour="darkgreen", alpha = 0.3, size=0.4) + theme_lato), 
      diag = list(continuous = function(...) 
        ggally_barDiag(..., fill="purple") + theme_lato),
    ) +
      theme(
        strip.background = element_blank(), 
        strip.text = element_text(size=8, family="Lato Light"), 
        axis.line = element_line(colour = "grey"),
        panel.grid.minor = element_blank(),
      )
  })
  #For Simple and Multiple linear Regression model..
  lmModel <- reactive({
    df <- myData()
    x  <- input$xvariable
    y  <- input$yvariable
    x  <- as.numeric(df[[as.name(input$xvariable)]])
    y  <- as.numeric(df[[as.name(input$yvariable)]])
    current_formula <- paste0(input$yvariable, " ~ ", paste0(input$xvariable, collapse = " + "))
    current_formula <- as.formula(current_formula)
    model <- lm(current_formula, data = df, na.action=na.exclude)
    return(model)
  })
  output$lmSummary <- renderPrint({
    req(lmModel())
    summary(lmModel())
  })
  output$diagnosticPlot <- renderPlot({
    req(lmModel())
    par(mfrow = c(2,2))
    plot(lmModel())
  })
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(myData(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
    })
}