rm(list = ls())

library(shinysky)
library(shiny)
library(rintrojs)

my_autocomplete_list <- c("John Doe","Ash","Ajay sharma","Ken Chong","Will Smith","Neo")

ui <- shinyUI(
  fluidPage(

            tabPanel("CAREER PATHFINDER", value = "careerPF",

                     sidebarLayout(

                       sidebarPanel( width = 3,
                                     introjsUI(),

                                     tags$div(
                                       actionButton("help", "Take a Quick Tour"),
                                       style = "height:50px;"
                                     ),
                                     useShinyjs(),

                                     tags$div(
                                       style = "height:50px;",
                                       introBox(
                                         tags$div(
                                           style = "height:50px;",
                                           actionLink("settings", "Settings",
                                                      icon = icon("sliders", class = "fa-2x"))),
                                         data.step = 6,
                                         data.intro = "Settings is where you can set options that affect the graph and career statistics."
                                       ),
                                       radioButtons("selectData",
                                                    label = "How many years of data do you want to include?",
                                                    choices = c("30 Years",
                                                                "15 Years"),
                                                    inline = TRUE,
                                                    width = "100%"
                                       ),
                                       selectizeInput("changeAvatar", "Change Icon:",
                                                      choices = c(
                                                        # "Traveler" = "traveler",  # not compatible with new FA
                                                        "Map Marker" = "map-marker",
                                                        "Rocket" = "rocket",
                                                        # "Paper Plane" = "paper-plane",  # not compatible with new FA
                                                        "Leaf" = "leaf"),
                                                      selected = "rocket"
                                       ),
                                       textInput("userName", "Add your name:", value = ""),

                                       tags$div(
                                         style = "height:50px;",

                                         uiOutput("printInput1"),
                                         uiOutput("printInput2"),
                                         uiOutput("printInput3"),
                                         uiOutput("printInput4"),
                                         uiOutput("printInput5")
                                       )
                                     )
                       ),

            mainPanel(
              # one way of doing it
              textInput.typeahead(id="search",
                                  placeholder="Type your name please",
                                  local=data.frame(name=c(my_autocomplete_list)),
                                  valueKey = "name",
                                  tokens=c(1:length(my_autocomplete_list)),
                                  template = HTML("<p class='repo-language'>{{info}}</p> <p class='repo-name'>{{name}}</p>")
              ),
              br(),br(),
              # using select2Input
              select2Input("select2Input1","",choices=c(my_autocomplete_list),type = c("input", "select"))
            )
  )))
)

server <- function(input, output, session) {


  observeEvent("", {
    showModal(modalDialog(
      includeHTML("intro_text.html"),
      easyClose = TRUE,
      footer = tagList(
        actionButton(inputId = "intro", label = "INTRODUCTION TOUR", icon = icon("info-circle"))
      )
    ))
  })


  observeEvent(input$help, {
    print("introjs button clicked")
    introjs(session, options = list("nextLabel"="Next",
                                    "prevLabel"="Back",
                                    "skipLabel"="Exit"))
  })



}
shinyApp(ui = ui, server = server)
