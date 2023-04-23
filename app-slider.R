############################################
# Data Professor                           #
# http://youtube.com/dataprofessor         #
# http://github.com/dataprofessor          #
# http://facebook.com/dataprofessor        #
# https://www.instagram.com/data.professor #
############################################

# Import libraries
library(shiny)
library(data.table)
library(randomForest)

# Read in the RF model
model <- readRDS("model.rds")

# Training set
TrainSet <- read.csv("training.csv", header = TRUE)
TrainSet <- TrainSet[,-1]


####################################
# User interface                   #
####################################
ui <- pageWithSidebar(
  
  # Page header
  headerPanel('Star Type Predictor'),
  
  # Input values
  sidebarPanel(
    HTML("<h3>Input parameters</h4>"),
    sliderInput("Temperature..K", label = "Temperature", value = 3000,
                min = min(TrainSet$Temperature..K.),
                max = max(TrainSet$Temperature..K.)
    ),
    sliderInput("Luminosity", label = "Luminosity", value = 0.0020,
                min = min(TrainSet$Luminosity.L.Lo.),
                max = max(TrainSet$Luminosity.L.Lo.)),
    sliderInput("Radius.R.Ro", label = "Radius", value = 0.17,
                min = min(TrainSet$Radius.R.Ro.),
                max = max(TrainSet$Radius.R.Ro.)),
    sliderInput("Absolute.magnitude.mv", label = "Magnitude", value = 15.1,
                min = min(TrainSet$Absolute.magnitude.Mv.),
                max = max(TrainSet$Absolute.magnitude.Mv.)),
    selectInput("Star.color", label = "Star color:", 
                choices = list("Red"="Red","Blue White"="Blue_White","White"="White",
                               "Yellowish White"="yellow_white","Pale Yellowish Orange"="Pale yellow orange","Orange" ="Orange", "Blue"="Blue", "Yellow" ="Yellow", "Orange Red"= "Orange-red"),
                selected = "Blue"),
    selectInput("Spectral.class", label = "Spectral Class:", 
                choices = list("M" = "M","B"="B","A"="A","F" = "F", "O"="O", "K" = "K", "G"="G"),
                selected = "M"),
    
    
    actionButton("submitbutton", "Submit", class = "btn btn-primary")
  ),
  
  mainPanel(
    tags$label(h3('Status/Output')), # Status/Output Text Box
    verbatimTextOutput('contents'),
    tableOutput('tabledata') # Prediction results table
    
  )
)

####################################
# Server                           #
####################################

server<- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    df <- data.frame(
      Name = c("Temperature..K.",
               "Luminosity.L.Lo.",
               "Radius.R.Ro.",
               "Absolute.magnitude.Mv.",
               "Star.color",
               "Spectral.Class"),
      Value = as.character(c(input$Temperature..K.,
                             input$Luminosity.L.Lo.,
                             input$Radius.R.Ro.,
                             input$Absolute.magnitude.Mv.,
                             input$Star.color,
                             input$Spectral.Class)),
      stringsAsFactors = FALSE)
    
    
    Star.type <- data.frame(Name = "Star Type", Value = "star type")
    df <- rbind(df, Star.type)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv("input.csv", header = TRUE)
    Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 3))
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)