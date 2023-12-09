#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(ROCR)
library(popbio)
library(DT)
library(caret)
library(shinydashboard)





# Define UI for application that draws a histogram
ui <- dashboardPage(

    # Application title
  dashboardHeader(title = "Case Study 3: Logistic Regression for Heart Failure ", titleWidth = 500),
  dashboardSidebar(
    sidebarMenu(
    menuItem("Introduction", tabName = "intro"),
    menuItem("Data Table", tabName = "dataTable"),
    menuItem("Histogram", tabName = "hist"),
    menuItem("Density Plot", tabName = "cdplot"),
    menuItem("Logistic Regression Setup", tabName = "logRSetup",
             menuSubItem('Logistic Regression Setup',
                         tabName = 'logRSetup'),
             menuSubItem('Confidence Intervals',
                         tabName = 'cInt')),
    menuItem("Training/Test Set Setup & Prediction", tabName = "tSets"),
    menuItem("Vizualization", tabName = "viz"),
    menuItem("Model Evaluation", tabName = "eval")
    )
  ),
  dashboardBody(
        tabItems(
          tabItem(tabName = "intro", p("Hello! I have done a Logistic Regression analysis of the probability that a patient is diagnosed with heart failure. I chose a classification model, because in this case there are two out comes: Heart Failure or no Heart Failure. I chose the dataset from Kaggle under the following link: https://www.kaggle.com/datasets/fedesoriano/heart-failure-prediction"), p("The data in the data set includes: Age, Sex, Chest Pain Type, Resting BP, Cholesterol, Fasting Blood Sugar, Resting ECG, Max Heart Rate, Excercise Angina, Old Peak, ST Slope, Heart Disease."),p("My motivation for this analysis is that according to the CDC Heart Disease is the leading cause of death for men and women, with one person dying every 33 seconds in the United States. Thus predicition of Heart Failure is vital, and allows us to determine if the patient is at risk and what lifestyle changes must be implemented."), p("I analyzed the data using R and the following functions: glm and predict, as well as other visualization packages."),p("Continue through the tabs to see how the analysis was done, as well as figures depicting the data and results.")),          tabItem(tabName = "dataTable" , p("This panel displays a small section (50 of 918 entries) of the dataset data in a tabular format, you can order columns in ascending or descending order, as well as search of specific values using the search box."), dataTableOutput("table")),
          tabItem(tabName = "hist", p("This panel displays a histogram showing the distribution of the incidence of heart rate within the dataset, where 1 connotates that heart disease is found in the patient, and 0 connotates that heart disease was not found."), plotOutput("histogram")),
          tabItem(tabName = "cdplot", p("Plots the conditional densities showing the distribution of the rate of Heart Disease over age"),plotOutput("cdplot")),
          tabItem(tabName = "logRSetup", p("GLM stands for generalized linear model, which usually refers to conventional linear regression models for the continous response variable, in this case Heart Disease, over age. Here we see the output of running glm on our data."),verbatimTextOutput("logRegSum")),
          tabItem(tabName = "cInt", p("Confidence Interval Setup"), verbatimTextOutput("confidence")),
          tabItem(tabName = "tSets", p("Input Percentage Split"), sliderInput("percentage_slider", "Select a value which will determine the percentage split for train/test data sets:", min = 0, max = 1, value = .1), p("Below is the prediction for the test set, you can compare the 'Heart Disease' and 'Predicted' columns for the outcomes"), tableOutput("split")),
          tabItem(tabName = "viz", p("Visualizations of the Logistic Regression Model the first being the glm model prediciton of probablitiy of Heart Disease over age with points shown representing the age and the second with the age as a histogram"), plotOutput("viz"), plotOutput("logHist")),
          tabItem(tabName = "eval", p("ROC Curve: This curve shows the performance of a classification model at all classification thresholds. Users can adjust the train/test splits on the Training/Test Set-Up tab and see how it affects the performance of the model."), plotOutput("rocPlot"),p("Model Assesment (out of 1):"),verbatimTextOutput("modelAssesment"))
          )
      )
  
)

          

server <- function(input, output) {
  heart_data <- read.csv("heart.csv")
  HDLogreg = glm(HeartDisease ~ ., data=heart_data, family=binomial(link="logit"))
  set.seed(2023)
  
  values <- reactiveValues(
    splitData = NULL,
    trainData = NULL,
    testData = NULL,
    diseaseModel = NULL
  )
  
  # Display table
  output$table <- renderDataTable({
    head(heart_data,n=50)
  })
  
  # Display histogram
  output$histogram <- renderPlot({
    ggplot(heart_data, aes(x = HeartDisease)) +
      geom_histogram(binwidth = 1, fill = "blue", color = "black") +
      labs(title = "Heart Disease Distribution", x = "Heart Disease", y = "Frequency")
    
  })
  
  output$cdplot <- renderPlot({
    cdplot(factor(HeartDisease) ~ Age, data=heart_data, main="Prevalence of Heart Disease based on Age", ylab='Heart Disease (0 = Normal, 1 = Heart Disease)')
  })
  
  output$logRegSum <- renderPrint({
    summary(HDLogreg)
  })
  
  output$confidence <- renderPrint({
    confint(HDLogreg)
  })
  

  observeEvent(input$percentage_slider, {
    values$splitData <- caret::createDataPartition(heart_data$HeartDisease, p = input$percentage_slider, list=F, times=1)
    values$trainData <- heart_data[values$splitData,]
    values$testData <- heart_data[!row.names(heart_data) %in% row.names(values$trainData),]
    values$diseaseModel <- glm(HeartDisease ~ ., data = values$trainData, family=binomial(link="logit"))
  })

  
  output$split <- renderTable({
    values$testData$Predicted <- round(predict(values$diseaseModel, newdata = values$testData, type="response"))
    head(values$testData, n = 10)
  })
  
  output$viz <- renderPlot({
    plot(values$trainData$Age, values$trainData$HeartDisease, xlab="Age", ylab="P(HeartDisease)")
    trainLR = glm(HeartDisease ~ Age, data=values$trainData, family=binomial(link="logit"))
    curve(predict(trainLR,data.frame(Age=x),type="resp"),add=TRUE)
    points(values$trainData$Age,fitted(trainLR),pch=20)
  })
  
  output$logHist <- renderPlot({
    logi.hist.plot(values$trainData$Age, values$trainData$HeartDisease, boxp = FALSE, type = "hist", col = "gray")
  })
  
  output$rocPlot <- renderPlot({
    pred = predict(values$diseaseModel, values$testData, type="response")
    pObject = ROCR::prediction(pred, values$testData$HeartDisease )
    rocObj = ROCR::performance(pObject, measure="tpr", x.measure="fpr")
    aucObj = ROCR::performance(pObject, measure="auc")  
    plot(rocObj, main = paste("Area under the curve:", round(aucObj@y.values[[1]] ,4))) 
  })
  
  output$modelAssesment <- renderText({
    Phat = predict(values$diseaseModel,values$testData,type="response")
    prop.table(xtabs(~ HeartDisease, data=values$testData))
    thresh = 0.5
    facHat = cut(Phat, breaks=c(-Inf, thresh, Inf), labels=c(0, 1))
    cTab   = xtabs(~ HeartDisease + facHat, data=values$testData)
    addmargins(cTab)
    CCR = sum(diag(cTab)) / sum(cTab)
    CCR
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
