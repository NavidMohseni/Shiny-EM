library(shiny)
library(tidyverse)
library(shinythemes)

# Define the UI
ui <- fluidPage(
  theme = shinytheme("sandstone"),
  titlePanel("EM Algorithm - Gaussian Mixture Model"),
  sidebarLayout(
    sidebarPanel(
      fileInput("upload", "Upload Data", 
                accept = c(".csv", ".tsv"),
                multiple = FALSE),
      
      checkboxInput("header", "Header", TRUE),
      
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
      
      numericInput("column", "Pick Column", 
                   value=1, min=1, step=1),
      
      tags$hr(),
      
      selectInput("varchoice", 
                  "Choose the variable for which you want to check the normality", 
                  choices = NULL),
      
      tags$hr(),
      
      numericInput("numModes", "# of Modes:", 
                   value=1, min=1, max=10, step=1),
      
      numericInput("numEMStep", "EM-Step:", 
                   value=1, min=1, step=1),
      
      
      checkboxGroupInput("variables", "Show:",
                         c("Initial Est." = "init", 
                           "Final Est." = "final",
                           "Legend" = "legend"), 
                         inline = TRUE),
      tags$hr(),
      
      sliderInput("numBins", "Number of bins:", 
                  min = 1, max = 50, value = 30),
      
    ),
    mainPanel(
      tabsetPanel(
        # Tabset 1
        tabPanel("Data", 
                 tableOutput("dataTable")
        ),
        
        # Tabset 2
        tabPanel("Model Selection", 
                 tableOutput("bic")
        ),
        
        # Tabset 3
        tabPanel("Plot", 
                 plotOutput("hist")
        ),
        
        # Tabset 4
        tabPanel("Summary", 
                 tableOutput("summary")
        ),
        
        # Tabset 5
        tabPanel("Model Selection Plot", 
                 plotOutput("linePlot")
        ),
        
        # Tabset 6
        tabPanel("Test - QQ-Plot", 
                 plotOutput("qqPlot")),
        tabPanel("Test - Shapiro-Wilk Test", 
                 verbatimTextOutput("shapiroTest"))
      )
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  # Output for Tab 1
  data <- reactive({
    req(input$upload)
    load_file(input$upload$name, input$upload$datapath, input$header)
  })
  
  singleData <- reactive({
    dfTemp <- data.frame(data()[,input$column])
    names(dfTemp)[1] <- "x"
    dfTemp
  })
  
  emParameters <- reactive({
    create_EMParameters(singleData(),input$numModes)
  })
  
  allEMParameters <- reactive({
    dfout <- numeric(10)
    for (i in 1:10){
      dfA <- create_EMParameters(singleData(), i)
      log_likelihood_value <- as.numeric(dfA[nrow(dfA), ncol(dfA)])
      dfout[i] <- log_likelihood_value  
    }
    n <- 200
    aic_data <- tibble(Modes = as.numeric(seq(1, 10, 1)),
                       loglikelihood = dfout,
                       AIC = 2 * (Modes * 3) - 2 * loglikelihood,
                       BIC = (Modes * 3) * log(n) - 2 * loglikelihood)
    aic_data
  })
  
  output$dataTable <- renderTable({
    req(input$upload)
    
    tryCatch(
      {
        infile <- input$upload$datapath
        data <- read_delim(infile, 
                           col_names = input$header,
                           delim = input$sep,
                           show_col_types = FALSE)
      },
      error = function(e){
        stop(safeError(e))
      }
    )
    if(input$disp == "head"){
      return(head(data))
    }
    else {
      return(data)
    }
    
  })
  
  # Output for Tab 2
  output$bic <- renderTable({
    allEMParameters()
  })
  
  # Output for Tab 3
  output$hist <- renderPlot({
    print(create_plots(singleData(), input$numModes, 
                       emParameters(), input$numBins, 
                       input$numEMStep, input$variables))
  }, res = 96)
  
  # Output for Tab 4
  output$summary <- renderTable({
    print(emParameters()[1:ncol(emParameters())-1])
  })
  
  # Output for Tab 5
  
  output$linePlot <- renderPlot({
    req(allEMParameters())  # Ensure that the data is available
    
    data3 <- allEMParameters()  # Get the data
    
    # Create the plot
    ggplot(data3, aes(x = Modes)) +
      geom_line(aes(y = AIC, color = "AIC")) +  # Line for AIC
      geom_line(aes(y = BIC, color = "BIC")) +  # Line for BIC
      labs(x = "Modes", y = "Value", title = "AIC and BIC vs Modes") +
      scale_x_continuous(breaks = data3$Modes) + 
      scale_color_manual(values = c("AIC" = "blue", "BIC" = "red")) +  # Set line colors
      theme_minimal()
  })
  
  # Output for Tab 6
  data1 <- reactive({
    req(input$upload)
    inFile <- input$upload
    read_csv(inFile$datapath)
  })
  
  observe({
    df <- data1()
    updateSelectInput(session, "varchoice", choices = names(df))
  })
  
  var <- reactive({
    req(input$varchoice)
    df <- data1()
    df[[input$varchoice]]
  })
  
  output$qqPlot <- renderPlot({
    req(var())
    qqnorm(var(), main = paste("QQ plot of", input$varchoice))
    qqline(var())
  })
  
  output$shapiroTest <- renderPrint({
    shapiro.test(var())
  })
}

# Run the application
shinyApp(ui, server)
