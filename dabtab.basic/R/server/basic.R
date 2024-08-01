library('plotly')
library('dplyr')
library('tidyr')
library('ggplot2')
library('shiny')
library('shinydashboard')
library('leaflet')
library('shinyWidgets')
library('bs4Dash')
library('shinyjs')
library('shinythemes')
library(sodium)
library(visNetwork)
library(DT)
library(rintrojs)
library('thematic')
library('bslib')
library('ggthemes')
library("shinycssloaders")
library('tidyverse')
library('rlang')
library(collapsibleTree)
library(echarts4r)

source(system.file("theme_modifier.R", package = "dabtab.basic"), chdir = TRUE)
source(system.file("graph_network.R", package = "dabtab.basic"), chdir = TRUE)
source(system.file("dabtab.R", package = "dabtab.basic"), chdir = TRUE)

button_color_css <- "
#datasets, #map_datasets{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;
/* Change the text size to 15 pixels. */
font-size: 15px;
}"

my_class <- "
.my-class {
background-color: #f2f2f2;
border: 1px solid #ddd;
padding: 10px;
}
"

countries_df <- read.csv("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv", strip.white = TRUE)
countries_df <- countries_df[c("name", "alpha.2")]
countries_df$alpha.2 <- tolower(countries_df$alpha.2)

img_urls <- paste0(
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/",
  countries_df$alpha.2, ".svg"
)

input_widget <- multiInput(
  inputId = "country_select",
  label = "Countries :",
  choices = NULL,
  selected = countries_df$name[1],
  choiceNames = lapply(
    seq_along(countries_df$alpha.2),
    function(i) {
      tagList(
        tags$img(src = img_urls[i], width = 20, height = 15),
        countries_df$name[i]
      )
    }
  ),
  choiceValues = countries_df$name
)



## urls for menu
r_url_list <- getOption("radiant.url.list")
r_url_list[["Prepare Data"]] <- "analysis/prepare_data/"
r_url_list[["Data Manipulation"]] <- "analysis/manipulate_data/"
r_url_list[["Visual Inspection"]] <- "analysis/visual_inspection/"
r_url_list[["Process Data"]] <- "analysis/process_data/"
r_url_list[["Analyze Data"]] <- "analysis/analyze_data/"
r_url_list[["Final Visualization"]] <- "analysis/final_visualize/"
r_url_list[["Summary"]] <- "analysis/summary/"
r_url_list[["Porfolio"]] <- "portfolio/cyclic/"
r_url_list[["Porfolio"]] <- "portfolio/sciene_branch/"
r_url_list[["Porfolio"]] <- "portfolio/graph_network/"
r_url_list[["Porfolio"]] <- "portfolio/line_graph/"
r_url_list[["Porfolio"]] <- "portfolio/point_interactive/"
r_url_list[["Porfolio"]] <- "porfolio/3d_visual/"
r_url_list[["Session Management"]] <- "session/download_state/"
r_url_list[["Session Management"]] <- "session/upload_state/"
r_url_list[["Session Management"]] <- "session/new_session/"
r_url_list[["Session Management"]] <- "session/refresh_session/"
r_url_list[["Session Management"]] <- "session/stop_session/"
r_url_list[["Session Management"]] <- "session/session_config/"
r_url_list[["Session Management"]] <- "session/more_infor/"


options(radiant.url.list = r_url_list)
rm(r_url_list)



## design menu
options(
  radiant.basic_server_ui =
advancedUI <- shiny::navbarPage("Dynamic Application",
                         position = c("static-top"),
                       tags$style(
                         type="text/css",
                         ".shiny-output-error { visibility: hidden; }",
                         ".shiny-output-error:before { visibility: hidden; }"
                       ),
                       tags$style(type="text/css", "body {overflow: auto !important;}"),
                       tags$style(type="text/css",".shiny-output-error { visibility: hidden; }",".shiny-output-error:before { visibility: hidden; }"),
                       tags$style(type="text/css","#search { top: 40% !important;left: 50% !important;margin-top: -100px !important;margin-left: -600px
                   !important; color: blue;font-size: 20px;font-style: italic;}"),
                               tags$style(HTML("
                        .input {
                        width: 200px;
                        }
                        ")),

                      tags$style(HTML("
                        .tt-hint {
                        width: 50%;
                        }
                        ")),

                         tags$head(
                           tags$style(
                             HTML(
                               "


                        .navbar-nav:last-child {
                        margin-right: 50px;
                        margin-left: auto;

                        }


                        .custom-column {
                          height: 1500px !important;
                        }


                        #tree {
                          height: 1500px !important;
                        }

                        svg {
                          height: 1500px !important;
                        }


                        g {
                          height: 1500px !important;
                        }


                      .logout-btn {
                        background-color: red;
                        color: white;
                        border: none;
                        font-size: 16px;
                        padding: 10px;
                        text-align: center;
                        cursor: pointer;
                        border-radius: 4px;
                      }



                        .edited-cell {
                          background-color: lightyellow;
                        }

                        label {
                        font-weight: bold;
                        }

                        span {
                        font-weight: normal !important;
                        }

                        .logout-btn:hover {
                          background-color: #e60000;
                        }


                        .navbar-nav:last-of-type .nav-item:nth-child(2) {
                        margin-right: 0px;
                        margin-left: auto;

                        }

                        " )
                           )),

                         useShinyjs(),
                           tabPanel(
                           "Data Analysis", icon = icon("home"),
                           id="tab_a",
                            fluidPage(
                              fluidRow(
                                column(width=3,
                                      conditionalPanel("input.tabs_data == 'Preparing Data'", uiOutput("ui_prepare")),
                                      conditionalPanel("input.tabs_data == 'Data Manipulation'", uiOutput("ui_manipulate"))),
                                column(width=9,
                                   tabsetPanel(
                                        id="tabs_data",
                                        type = "tab",
                                        selected = NULL,

                                        tabPanel("Preparing Data",
                                                 id = "tab1",
                                                 titlePanel("Preparing Data"),
                                                 shinyjs::hidden(fluidRow(
                                                   id = "myrow1",
                                                   style = "background-color: lightblue; font-weight: bold; padding-left: 10px; white-space: normal;",
                                                   column(
                                                     width = 9,
                                                     h4("Dataset Description"),
                                                     textOutput("descr_1")
                                                   )
                                                 ))
                                                 ,

                                                  fluidRow(
                                                   column(
                                                     4,
                                                     introBox(

                                                       textInput(
                                                         "search_input_1",
                                                         "Search:",
                                                         value = "",
                                                         placeholder = "Enter search key"
                                                       ),
                                                       data.step = 2,
                                                       data.intro = "This is the button",
                                                       data.hint = "Here is clue"
                                                     )
                                                   ),
                                                   column(
                                                     4,
                                                     sliderInput(
                                                       inputId = "Data Range",
                                                       label = "Select Data Range:",
                                                       min = 1,
                                                       max = 3500,
                                                       value = c(1, 250)
                                                     )
                                                   ),
                                                   column(4,
                                                          radioGroupButtons(
                                                            inputId = "viz_description",
                                                            label = "Description Showcase",
                                                            choices = c("Show Top", "Show Bottom", "Hide"),
                                                            status = "primary",
                                                            selected="Show Top"
                                                          )

                                                   ))
                                                 ,
                                                 br(),
                                                 introBox(

                                                   dataTableOutput("table"),
                                                   data.step = 1,
                                                   data.intro = "This is the table"
                                                 ),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 shinyjs::hidden(fluidRow(
                                                   id = "myrow2",
                                                   style = "background-color: lightblue; font-weight: bold; padding-left: 10px; white-space: normal;",
                                                   column(
                                                     width = 9,
                                                     h4("Dataset Description"),
                                                     textOutput("descr_2")
                                                   )
                                                 ))),



                                        tabPanel("Data Manipulation", fluid = TRUE,
                                                 id = 'tab20',
                                                 titlePanel("Data Manipulation for Processing"),
                                                 fluidRow(
                                                   column(
                                                     4,
                                                     radioGroupButtons(
                                                       inputId = "viz_data",
                                                       label = "Dataset Showcase",
                                                       choices = c("Show All", "Show Isolate", "Show Multiples"),
                                                       status = "primary",
                                                       selected="Show All"
                                                     ),
                                                     div(id = "dropdown_menu_7",
                                                         style = "display: none;",
                                                         uiOutput("multi_select"))

                                                   ),

                                                   column(
                                                     4,
                                                     checkboxGroupButtons(
                                                       inputId = "plot_sel",
                                                       label = "Choose a graph :",
                                                       choices = c(`<i class='fa fa-bar-chart'></i>` = "scatter", `<i class='fa fa-line-chart'></i>` = "density",
                                                                   `<i class='fa fa-pie-chart'></i>` = "whisker"),
                                                       justified = TRUE
                                                     )
                                                   )

                                                   ,

                                                   column(
                                                     4,

                                                     uiOutput("slider")
                                                   )

                                                 ),
                                                 fluidRow(


                                                   uiOutput("my_output")


                                                 ),


                                                 fluidRow(

                                                   shinycssloaders::withSpinner(uiOutput("my_output_1"))

                                                 )),


                                        tabPanel("Visual Inspection", fluid = TRUE,
                                                 id = 'tab2',
                                                 titlePanel("Data Visualization for Evaluation"),
                                                 fluidRow(
                                                   column(
                                                     4,
                                                     checkboxGroupButtons(
                                                       inputId = "Id057",
                                                       label = "Choose a graph :",
                                                       choices = c(`<i class='fas fa-circle'></i>` = "scatter", `<i class='fas fa-chart-area'></i>` = "density",
                                                                   `<i class='fas fa-box-plot'></i>` = "box_and_whisker"),
                                                       justified = TRUE
                                                     )

                                                   )

                                                   ,

                                                   column(4,
                                                          downloadButton("downloadPlot5", "Download Plot"))),

                                                 br(),
                                                 textOutput("my_text")

                                        )
                                        ,
                                        tabPanel( "Processing Data",

                                                fluid = TRUE,
                                                 id = "tab3",
                                                 titlePanel("Processing Data"),
                                                 actionBttn(inputId = "go_to_process",
                                                            label = "Go to Process Page",
                                                            style = "jelly",
                                                            color = "primary"),
                                                 background = "#e1f5fe",
                                                 border = "2px solid black"
                                        ),


                                        tabPanel("Analyzing Data", fluid = TRUE,
                                                 id = "tab4",
                                                 titlePanel("Analyzing Data"),
                                                 fluidRow(
                                                   column(4,
                                                          selectInput(inputId = "Search Key 2",
                                                                      label = "Search",
                                                                      choices = c(5,10,15),
                                                                      selected = "50 Free")),
                                                   column(4,
                                                          sliderInput(inputId = "Data Range 3",
                                                                      label = "Top Times Range:",
                                                                      min = 1, max = 3500,
                                                                      value = c(1,250))),
                                                   hr(),
                                                   helpText("Tip: Highlight points to populate table"),
                                                   br(),
                                                   textOutput("my_text_2")
                                                 )),


                                        tabPanel("Final Visualization", fluid = TRUE,
                                                 id = "tab5",
                                                 tags$style(button_color_css),
                                                 titlePanel("Visualizing Data for Sharing"),
                                                 fluidRow(
                                                   column(4,
                                                          selectInput(inputId = "Search Key 4",
                                                                      label = "Search",
                                                                      choices = c(5,10,15),
                                                                      selected = "50 Free")),
                                                   column(4,
                                                          sliderInput(inputId = "Data Range 5",
                                                                      label = "Top Times Range:",
                                                                      min = 1, max = 3500,
                                                                      value = c(1,250))),
                                                   hr(),
                                                   helpText("Tip: Highlight points to populate table"),
                                                   br(),
                                                   textOutput("my_text_3")

                                                 )),



                                        tabPanel("Summary", fluid = TRUE,
                                                 id = "tab15",
                                                 tags$style(button_color_css),
                                                 titlePanel("Processing Data"),
                                                 fluidRow(
                                                   column(
                                                     4,
                                                     selectInput(
                                                       inputId = "Search Key",
                                                       label = "Search",
                                                       choices = c(5, 10, 15),
                                                       selected = "50 Free"
                                                     )
                                                   ),
                                                   column(
                                                     4,
                                                     sliderInput(
                                                       inputId = "data_range_2",
                                                       label = "Top Times Range:",
                                                       min = 1,
                                                       max = 3500,
                                                       value = c(1, 250)
                                                     )
                                                   ),
                                                   verbatimTextOutput(outputId = "tb"),

                                                 )
                                        )



                                      ))

                             ))),

                         shiny::navbarMenu("Advanced Analtytics",
                                    icon = icon("chart-bar"),
                                    shiny::tabPanel("Explore more",fluid=TRUE,
                                             id="tab7",
                                             tags$style(button_color_css),
                                             titlePanel("Explore more")
                                    ),
                                    shiny::tabPanel("Ask me",fluid=TRUE,
                                             id="tab8",
                                             tags$style(button_color_css),
                                             titlePanel("Ask me")
                                    )

                         ),
                         shiny::navbarMenu(
                           "Portfolio",
                           icon = icon("bars"),
                           shiny::tabPanel(
                             "Cyclic",
                             id = "tab9",
                              fluidPage(
                               useShinyjs(),
                               sidebarLayout(
                                  sidebarPanel(
                                   shinyjs::hidden(
                                     selectInput(
                                       "datasets",
                                       "Select a dataset",
                                       choices = c(
                                         "FEBRUARY 2022" = "modified_data.csv",
                                         "AUGUST 2022" = "modified_data_2.csv",
                                         "DECEMBER 2022" = "modified_data_3.csv"
                                       )
                                     )
                                   ),
                                   radioButtons(
                                     "plots",
                                     "Select a plot",
                                     choices = c(
                                       "BARCHART" = "plot 1",
                                       "LINECHART" = "plot 2",
                                       "MAP" = "plot 3"
                                     )
                                   ),
                                   shinyjs::hidden(
                                     downloadButton("downloadPlot2", "Download plot")
                                   ),
                                   shinyjs::hidden(
                                     selectInput(
                                       "map_datasets",
                                       "Select a dataset for the map",
                                       choices = c(
                                         "FEBRUARY 2022" = "station_data_1.csv",
                                         "AUGUST 2022" = "station_data_2.csv",
                                         "DECEMBER 2022" = "station_data_3.csv"
                                       )
                                     )
                                   ),
                                   themeModifier(id = "theme")
                                 ),
                                  mainPanel(
                                   tabPanel(
                                     "Visualization",
                                     id = "tab11",
                                     selected = TRUE,
                                     conditionalPanel(
                                       condition = "input.plots == 'plot 1'",
                                       shinycssloaders::withSpinner(plotlyOutput("plot1"))
                                     ),
                                     conditionalPanel(
                                       condition = "input.plots == 'plot 2'",
                                       shinycssloaders::withSpinner(plotlyOutput("plot2"))
                                     ),
                                     conditionalPanel(
                                       condition = "input.plots == 'plot 3'",
                                       leafletOutput("my_map")
                                     ),
                                     em(
                                       "Positive and negative percentages indicate an increase and decrease from the baseline period (median value between January 3 and February 6, 2020) respectively.",
                                       hidden = TRUE
                                     )
                                   )
                                 )
                               )
                             )
                           ),
                           shiny::tabPanel(
                             id = "tree",
                             "Science Branches",
                             fluidPage(
                               # collapsible domains tree section
                               column(3, uiOutput("categorySelectComboTree")),
                               column(12, collapsibleTreeOutput('tree', height='1500px') %>% withSpinner(color = "green"), class = "custom-column")
                             )
                           ),
                           shiny::tabPanel(
                             id = "trend",
                             "World Trend",

                             shiny::fluidPage(
                               titlePanel("Animated Line Chart"),
                               sidebarLayout(
                                 sidebarPanel(
                                   input_widget

                                 ),
                                 shiny::mainPanel(
                                   echarts4rOutput("trend")
                                 )
                               )
                             )
                           ),

                           shiny::tabPanel(
                             id = "network",
                             "Dependency Network",

                             material_card(
                               fluidRow(
                                 column(
                                   width = 3,
                                   DT::DTOutput("package_table"),
                                   style = "font-size: 8pt"
                                 ),
                                 column(
                                   width = 6,
                                   fluidRow(
                                     column(
                                       width = 6,
                                       # sliderInput(
                                       #   ns("order"), label = "Order of ego network",
                                       #   min = 1, max = 10, value = 2,
                                       #   width = "100%"
                                       # ),
                                       style = "font-size: 0.9em"
                                     ),
                                     column(
                                       width = 6,
                                       switchInput("graph_physics_enabled", label = "Layout", value = FALSE, size = "mini"),
                                       style = "padding-top: 10px; text-align: right"
                                     ),
                                     column(
                                       width = 12,
                                       div(
                                         class = "graph-loading-bar",
                                         style = "margin-top: 180px",
                                         id = "loading-bar",
                                         div(id = "loading-text", class = "graph-loading-text", "Select a package"),
                                         div(
                                           id = "loading-bar-background", class = "graph-loading-bar-background",
                                           div(id = "progress-bar", class = "graph-progress-bar")
                                         )
                                       ),
                                       div(
                                         visNetworkOutput("graph", width = "100%", height = "450px"),
                                         style = "margin-top: -180px"
                                       )
                                     )
                                   ),
                                   style = "border-left: solid silver 1px; border-right: solid silver 1px"
                                 ),
                                 column(
                                   width = 3,
                                   fluidRow(
                                     column(
                                       width = 12,
                                       uiOutput("package_info") %>%
                                         withSpinner(type = 6, size = 0.5, proxy.height = "200px")
                                     )

                                   )
                                 )
                               )
                             )
                           )),
              
                            #Report Tab     
                            tabPanel(
                                "Reports",
                                icon = icon("file-text"), # Change the icon here
                                uiOutput("ui_report")
                                ,
,
                            
                         # 3D Visualization tab
                         shiny::navbarMenu(
                           "3D Visualization",
                           icon = icon("bars"),
                           shiny::tabPanel(
                             "3D Plot",
                             plotlyOutput("myplot")
                           )),

                         # 3D Visualization tab
                         shiny::navbarMenu(
                           "Interactive",
                           shiny::tabPanel(
                             "Plot Coordinate",
                             br(),
                             actionButton("remove1", "remove", class = "btn-primary"),
                             br(),
                             shiny::fluidRow(
                               column(
                                 width = 3,
                                 plotOutput("plot_coor", click = "plot_click", width = "100%", height = "700px")
                               ),
                               column(
                                 width = 9,
                                 DTOutput("mytable_1")
                               )
                             ))),

                         shiny::absolutePanel(
                           top = -5, right = 0,
                           width = "auto", height = 50,
                           style = "padding: 10px;",
                           uiOutput("logoutbtn"),
                           cursor = "move"

                         ),
                          # Menu tab
                         navbarMenu(
                           "Dataset Management",
                           icon = icon("bars"),
                           tabPanel("Upload Data", id = "tab18", titlePanel(uiOutput("username_tab"))),
                           tabPanel("View Storage", id = "tab13", icon = icon("user-tag"), titlePanel(uiOutput("permission_tab"))),
                           tabPanel("Share Data", id = "tab14", icon = icon("cog")),
                           tabPanel("More Info", id = "tab16", icon = icon("info-circle"))
                         ),
                         # Menu tab
                         navbarMenu(
                           "Session Management",
                           icon = icon("bars"),
                           conditionalPanel("Save State", id = "tab18", titlePanel(uiOutput("username_tab"))),
                           tabPanel("Load State", id = "tab13", icon = icon("user-tag"), titlePanel(uiOutput("permission_tab"))),
                           tabPanel("share State", id = "tab14", icon = icon("cog")),
                           tabPanel("View State", id = "tab14", icon = icon("cog")),
                           tabPanel("More Info", id = "tab16", icon = icon("info-circle"))
                         ),
                          # Infor tab
                          navbarMenu(
                           "Information",
                           icon = icon("bars"),
                           conditionalPanel("Save State", id = "tab18", titlePanel(uiOutput("username_tab"))),
                           tabPanel("Help", id = "tab13", icon = icon("user-tag"), titlePanel(uiOutput("permission_tab"))),
                           tabPanel("About", id = "tab14", icon = icon("cog")),
                           tabPanel("Con", id = "tab14", icon = icon("cog")),
                           tabPanel("Support", id = "tab16", icon = icon("info-circle"))
                         ),
                          # Dabtab Information tab
                          navbarMenu(
                           "Settings",
                           icon = icon("bars"),
                           tabPanel("Help", id = "tab13", icon = icon("user-tag"), titlePanel(uiOutput("permission_tab"))),
                           tabPanel("About", id = "tab14", icon = icon("cog")),
                           tabPanel("Report Issues", id = "tab16", icon = icon("info-circle"))
                         ),
                        # View Settings tab
                          navbarMenu(
                           "View Settings",
                           icon = icon("bars"),
                           tabPanel("Theme", id = "tab13", icon = icon("user-tag"), titlePanel(uiOutput("permission_tab")))#allow users to configure themes,
                           tabPanel("Switch", id = "tab14", icon = icon("cog")),
                           tabPanel("Support", id = "tab16", icon = icon("info-circle"))
                         ),
                        # Project Settings tab
                         navbarMenu(
                           "Project Settings",
                           icon = icon("bars"),
                           tabPanel("Save Project", id = "tab18", titlePanel(uiOutput("username_tab")))#allow users to configure shortcuts,
                           tabPanel("Load Project", id = "tab13", icon = icon("user-tag"), titlePanel(uiOutput("permission_tab")))#allow users to configure themes,
                           tabPanel("More Information", id = "tab14", icon = icon("cog")),
                         ),
                          # Option Settings tab
                          navbarMenu(
                           "Options",
                           icon = icon("bars"),
                           tabPanel("Check Updates", id = "tab18", titlePanel(uiOutput("username_tab")))#allow users to configure shortcuts,
                           tabPanel("Update", id = "tab13", icon = icon("user-tag"), titlePanel(uiOutput("permission_tab")))#allow users to configure themes,
                           conditionalPanel("Keyboard Shortcuts", id = "tab18", titlePanel(uiOutput("username_tab")))#allow users to configure shortcuts,
                           tabPanel("More Information", id = "tab14", icon = icon("cog")),
                         )


)





















