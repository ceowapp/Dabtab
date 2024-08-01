library(shiny)
library(shinydashboard)
library(DBI)
library(RMySQL)

# Database connection details
con <- dbConnect(RMySQL::MySQL(), 
                 dbname = "your_db_name",
                 host = "your_db_host",
                 port = your_db_port,
                 user = "your_db_user",
                 password = "your_db_password")

# Define the UI
ui <- dashboardPage(
  dashboardHeader(title = "Admin Dashboard"),
  dashboardSidebar(
    # Add menu items or filters if needed
  ),
  dashboardBody(
    fluidRow(
      box(
        title = "User Data",
        dataTableOutput("userTable")
      )
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Query to retrieve user data from the database
  query <- "SELECT * FROM users"

  # Load user data into a data frame
  user_data <- reactive({
    dbGetQuery(con, query)
  })

  # Render the user data in a data table
  output$userTable <- renderDataTable({
    user_data()
  })
}

shinyApp(ui, server)
