# Install required packages (if not installed already)
if(!require(shiny)) install.packages("shiny")
if(!require(randomForest)) install.packages("randomForest")

library(shiny)
library(randomForest)

ui <- fluidPage(
  titlePanel("Random Forest Analysis Tool"),
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Upload CSV File", accept = ".csv"),
      numericInput("splitRatio", "Train-Test Split Ratio (%)", value = 70, min = 50, max = 90),
      actionButton("runAnalysis", "Run Analysis")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Visualization", plotOutput("plot")),
        tabPanel("Results", verbatimTextOutput("results"))
      )
    )
  )
)

server <- function(input, output) {
  reactiveData <- reactive({
    req(input$datafile)
    data <- read.csv(input$datafile$datapath)
    # Data Cleaning Example
    data <- na.omit(data)
    return(data)
  })
  
  analysisResult <- eventReactive(input$runAnalysis, {
    data <- reactiveData()
    set.seed(123)
    splitIndex <- floor(nrow(data) * input$splitRatio / 100)
    trainData <- data[1:splitIndex, ]
    testData <- data[(splitIndex + 1):nrow(data), ]
    model <- randomForest(area ~ ., data = trainData, ntree = 100)
    predictions <- predict(model, testData)
    
    results <- list(model = model, predictions = predictions, testData = testData)
    return(results)
  })
  
  output$plot <- renderPlot({
    results <- analysisResult()
    plot(results$predictions, results$testData$area, main = "Predictions vs Actual Values",
         xlab = "Predicted Area", ylab = "Actual Area")
  })
  
  output$results <- renderPrint({
    results <- analysisResult()
    cat("Random Forest Model Summary:\n")
    print(summary(results$model))
    cat("\nSuggestions for Analysis:\n")
    cat("Evaluate variable importance and consider fine-tuning parameters for better accuracy.\n")
  })
}

shinyApp(ui = ui, server = server)
