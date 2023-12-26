
# Load R packages
library(shiny)
library(shinythemes)
library(RMySQL)

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
  ui <- fluidPage(theme = shinytheme("cerulean"),
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
      tabPanel("Graphs", "This panel is intentionally left blank."),
      tabPanel("Machine Learning", "This panel is intentionally left blank")
  
    ) # navbarPage
  ) # fluidPage

  
  # Define server function  
  server <- function(input, output) {
    
    output$txtout <- renderText({
      paste( input$txt1, input$txt2, sep = " " )
    })
    
    output$table <- renderTable({
      return(get_user_details(input$EmailID))
    })
    
  } # server
  

  # Create Shiny object
  shinyApp(ui = ui, server = server)
