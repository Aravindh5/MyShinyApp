
# Load R packages
library(shiny)
library(shinythemes)
library(RMySQL)
library(data.table)
library(RCurl)
library(randomForest)

data(airquality)

# Read data
weather <- read.csv(text = getURL("https://raw.githubusercontent.com/dataprofessor/data/master/weather-weka.csv"),  stringsAsFactors = TRUE)

# Build model
model <- randomForest(play ~ ., data = weather, ntree = 500, mtry = 4, importance = TRUE)

rm(list = ls())
readRenviron(".env")

  killDbConnections <- function () {
    
    all_cons <- dbListConnections(MySQL())
    
    print(all_cons)
    
    for(con in all_cons)
      +  dbDisconnect(con)
    
    print(paste(length(all_cons), " connections killed."))
    
  }

  get_db_connection <- function(){

    db_user <- Sys.getenv("USER_NAME")
    db_password <- Sys.getenv("PASSWORD")
    db_name <- Sys.getenv("DB_NAME")
    db_host <- Sys.getenv("HOST")
    db_port <- integer(Sys.getenv("PORT"))

    # 3. Read data from db
    mydb <-  dbConnect(MySQL(), user = db_user, password = db_password,
                       dbname = db_name, host = db_host, port = db_port)
    
    return(mydb)

  }

  get_user_details <- function(given.email.id){
    
    print(given.email.id)
    # Query Formation
    query <- paste("select * from users where Email='", given.email.id, "'", sep="")
    print(query)
    mydb <- get_db_connection()
    rs <- dbSendQuery(mydb, query)
    
    df <-  fetch(rs, n=-1)
    lapply(dbListConnections(MySQL()), dbDisconnect)
    
    Name <- df$UserName
    Email <- df$Email
    Phone <- df$PhoneNumber
    TotalPurchase <- "$100,000"
    
    name.detail <- c("Name", Name)
    email.detail <- c("Email", Email)
    phone.detail <- c("Phone", Phone)
    total.detail <- c("Total Purchase", TotalPurchase)
    
    required.details <- rbind(name.detail, email.detail, phone.detail, total.detail)
    colnames(required.details) <- c('Particular', 'Detail')
    rownames(required.details) <- c(1, 2, 3, 4)
    print(required.details)
    return(required.details)
  }

  # Define UI
  ui <- fluidPage(theme = shinytheme("cyborg"),
    navbarPage(

      "My Shiny App",
      # Navbar 1, tabPanel
      tabPanel("Navbar 1",
               sidebarPanel(
                 tags$h3("Input:"),
                 textInput("txt1", "Given Name:", ""),
                 textInput("txt2", "Surname:", ""),

               ), # sidebarPanel
               mainPanel(
                            h1("Header 1"),
                            
                            verbatimTextOutput("txtout"),

               ) # mainPanel
               
      ), 
      tabPanel("My-SQL", 
               sidebarPanel(
                 # tags$h3("Input:"),
                 textInput("EmailID", "Email:", ""),
               ),
               mainPanel(
                            h1("User Details"),
                            dataTableOutput("table")
                            
               )
      ),
      tabPanel("Visualization",
               sidebarLayout(
                sidebarPanel(
                  sliderInput(
                       inputId = "bins",
                       label = "Number of bins : ",
                       min = 1,
                       max = 50,
                       value = 30)
                  ),
                   
                mainPanel(
                     
                     h1("Histogram"),
  
                     # Output - Histogram
                     plotOutput(outputId = "distPlot")
                   )
                   
               )),
               
      tabPanel("Machine Learning", 
        
               headerPanel('Play Golf?'),
               
               # Input values
               sidebarPanel(
                 HTML("<h3>Input parameters</h3>"),
                 
                 selectInput("outlook", label = "Outlook:", 
                             choices = list("Sunny" = "sunny", "Overcast" = "overcast", "Rainy" = "rainy"), 
                             selected = "Rainy"),
                 sliderInput("temperature", "Temperature:",
                             min = 64, max = 86,
                             value = 70),
                 sliderInput("humidity", "Humidity:",
                             min = 65, max = 96,
                             value = 90),
                 selectInput("windy", label = "Windy:", 
                             choices = list("Yes" = "TRUE", "No" = "FALSE"), 
                             selected = "TRUE"),
                 
                 actionButton("submitbutton", "Submit", class = "btn btn-primary")
               ),
               
               mainPanel(
                 tags$label(h3('Status/Output')), # Status/Output Text Box
                 verbatimTextOutput('contents'),
                 tableOutput('tabledata') # Prediction results table
                 
               ))
    ) # navbarPage
  ) # fluidPage

  # Define server function  
  server <- function(input, output, session) {
    
    # Input Data
    datasetInput <- reactive({  
      
      # outlook,temperature,humidity,windy,play
      df <- data.frame(
        Name = c("outlook",
                 "temperature",
                 "humidity",
                 "windy"),
        Value = as.character(c(input$outlook,
                               input$temperature,
                               input$humidity,
                               input$windy)),
        stringsAsFactors = FALSE)
      
      play <- "play"
      df <- rbind(df, play)
      input <- transpose(df)
      write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
      
      test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
      
      test$outlook <- factor(test$outlook, levels = c("overcast", "rainy", "sunny"))
      
      
      Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 3))
      print(Output)
      
    })
    
    output$txtout <- renderText({
      paste( input$txt1, input$txt2, sep = " " )
    })
    
    output$table <- renderTable({
      return(get_user_details(input$EmailID))
    })
    
    output$distPlot <- renderPlot({
      
      x <- airquality$Ozone
      x <- na.omit(x)
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      hist(x, breaks = bins, col = "#11AADB", border = "Black",
           xlab = "ozone level",
           main = "Histogram of Ozone level")
    
    output$contents <- renderPrint({
      if (input$submitbutton>0) {
        isolate("Calculation Complete")
      } else {
        return("Server is ready for calculation")
      }
    })
    
    output$tabledata <- renderTable({
      if (input$submitbutton>0) {
        isolate(datasetInput())
      }
    })
    
  }) # server
}

  # Create Shiny object
  shinyApp(ui = ui, server = server)
