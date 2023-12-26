# 1. Library
library(RMySQL)

# 2. Settings
db_user <- 'root'
db_password <- 'root'
db_name <- 'TestDB'
db_table <- 'users'
db_host <- '127.0.0.1'
db_port <- 3306

# 3. Read data from db
mydb <-  dbConnect(MySQL(), user = db_user, password = db_password,
                   dbname = db_name, host = db_host, port = db_port)
s <- paste("select * from", db_table, "where", "Email='Ktaravindh005@gmail.com'", sep=" ")
rs <- dbSendQuery(mydb, s)
df <-  fetch(rs, n = -1)
df

user_name <- df$UserName
user_name
