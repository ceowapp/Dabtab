library(shiny)
library(shinydashboard)
library('bs4Dash')


samplingUI <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Example Dashboard"),
  dashboardSidebar(
    width = 300,

    sidebarMenu(
      id = "sidebar",
      menuItem("Overview", tabName = "overview", icon = icon("chart-line")),
      uiOutput("sampling_1"),
      uiOutput("sampling_2")
        )),

  dashboardBody(

    tags$head(
      tags$style(
        HTML("
             .btn-lg {
               padding: 30px 20px;
               font-size: 20px;
             }
        ")
      )
    ),

    div(
      class = "btn-group",
      actionButton("go_to_process_page", icon = icon("arrow-left"), "Go to Process Page", class = "btn btn-primary"),
      actionButton("go_to_advanced_page", icon = icon("arrow-right"), "Go to Advanced Page", class = "btn btn-success")
    ),

    tabItems(
             tabItem(tabName="overview",

                     fluidRow(
                       # A static infoBox
                       infoBox("New Orders", 10 * 2, icon = icon("credit-card")),
                       # Dynamic infoBoxes
                       infoBoxOutput("progressBox"),
                       infoBoxOutput("approvalBox")
                     ),

                     # infoBoxes with fill=TRUE
                     fluidRow(
                       infoBox("New Orders", 10 * 2, icon = icon("credit-card")),
                       infoBoxOutput("progressBox2"),
                       infoBoxOutput("approvalBox2")
                     ),

                     fluidRow(
                       # Clicking this will increment the progress amount
                       box(width = 4, actionButton("count", "Increment progress"))
                     ),

                     fluidRow(
                       tabBox(
                         title = "First tabBox",
                         # The id lets us use input$tabset1 on the server to find the current tab
                         id = "tabset1", height = "250px",
                         tabPanel("Tab1", "First tab content"),
                         tabPanel("Tab2", "Tab content 2")
                       ),
                       tabBox(
                         side = "right", height = "250px",
                         selected = "Tab3",
                         tabPanel("Tab1", "Tab content 1"),
                         tabPanel("Tab2", "Tab content 2"),
                         tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
                       )
                     )),
         tabItem(tabName="sampling",
          fluidRow(
            dataTableOutput("sampled_df")),
          fluidRow(
            box(
              textOutput("summary")
            ),
            box(
              background = "maroon", solidHeader = TRUE,
              tags$h2("Dropdown Button"),
              br(),
              dropdown(

                tags$h3("List of Input"),

                pickerInput(inputId = 'xcol2',
                            label = 'X Variable',
                            choices = names(iris),
                            options = list(`style` = "btn-info")),

                pickerInput(inputId = 'ycol2',
                            label = 'Y Variable',
                            choices = names(iris),
                            selected = names(iris)[[2]],
                            options = list(`style` = "btn-warning")),

                sliderInput(inputId = 'clusters2',
                            label = 'Cluster count',
                            value = 3,
                            min = 1, max = 9),

                style = "unite", icon = icon("gear"),
                status = "danger", width = "300px",
                animate = animateOptions(
                  enter = animations$fading_entrances$fadeInLeftBig,
                  exit = animations$fading_exits$fadeOutRightBig
                )),
              shinycssloaders::withSpinner(plotlyOutput("histogram"))
            )
          )
        ),
        tabItem(tabName="calculation",
          fluidRow(
            box(
              dataTableOutput("cal_tabel")
            ),
            box(
              tags$h2("Dropdown Button"),
              br(),
              dropdown(

                tags$h3("List of Input"),

                pickerInput(inputId = 'xcol2',
                            label = 'X Variable',
                            choices = names(iris),
                            options = list(`style` = "btn-info")),

                pickerInput(inputId = 'ycol2',
                            label = 'Y Variable',
                            choices = names(iris),
                            selected = names(iris)[[2]],
                            options = list(`style` = "btn-warning")),

                sliderInput(inputId = 'clusters2',
                            label = 'Cluster count',
                            value = 3,
                            min = 1, max = 9),

                style = "unite", icon = icon("gear"),
                status = "danger", width = "300px",
                animate = animateOptions(
                  enter = animations$fading_entrances$fadeInLeftBig,
                  exit = animations$fading_exits$fadeOutRightBig
                )
              ),
              shinycssloaders::withSpinner(plotlyOutput("mean_plot"))
            )
          )
        )

      )


    )
  )







