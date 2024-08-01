library(shiny)
library(rpart)
library(rpart.plot)

library(periscope)

library(collapsibleTree)



# Define UI
ui <- fluidPage(
  fluidRow(
    tabsetPanel(
      id="tabs_data",
      type = "tab",
      selected = NULL,
      tabPanel(
        id = "tree",
        # collapsible domains tree section
        column(3, uiOutput("categorySelectComboTree")),
        column(12, collapsibleTreeOutput('tree', height='1500px') %>% withSpinner(color = "green"))
      )
    )
  )
)

# Define server
server <- function(input, output) {
  # Reactive data
  domains <- read.csv("./treechart_test.csv")

  output$categorySelectComboTree <- renderUI({
    selectInput("selectedCategoryTree","Select a category:", sort(as.character(unique(domains$domain))))
  })

  domainsTree <- reactive({
    domains[domains$domain == input$selectedCategoryTree, c("domain", "main_branch", "sub_branch")]
  })

  output$tree <- renderCollapsibleTree({
    collapsibleTree(
      domainsTree(),
      root = input$selectedCategoryTree,
      attribute = "sub_branch",
      hierarchy = c("domain", "main_branch", "sub_branch"),
      fill = "Green",
      zoomable = FALSE
    )
  })
}

# Run app
shinyApp(ui, server)

