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
library(DT)
library('thematic')
library('bslib')
library('ggthemes')
library("shinycssloaders")
library('tidyverse')
library('rlang')
library(collapsibleTree)
library(echarts4r)

button_color_css <- "
#datasets, #map_datasets{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;font-size: 15px;

/* Change the text size to 15 pixels. */
}"

my_class <- "
.my-class {
background-color: #f2f2f2;
border: 1px solid #ddd;
padding: 10px;
}
"
default <- bslib::bs_theme(version = 4)

basicUI<-shiny::navbarPage("Dynamic Application",
                    position = c("static-top"),
                    shiny::tabPanel("Dashboard",
                             tags$style(button_color_css),
                             id = "dashboard-tab-1",
                             useShinyjs(),
                             sidebarLayout(
                               sidebarPanel(
                                 shinyjs::hidden(selectInput("datasets", "Select a dataset",
                                                             choices = c("FEBRUARY 2022" = "modified_data.csv",
                                                                         "AUGUST 2022" = "modified_data_2.csv",
                                                                         "DECEMBER 2022" = "modified_data_3.csv"))),
                                 radioButtons("plots", "Select a plot",
                                              choices = c("BARCHART" = "plot 1",
                                                          "LINECHART" = "plot 2",
                                                          "MAP"= "plot 3")),
                                 shinyjs::hidden(downloadButton("downloadPlot", "Download plot")),
                                 shinyjs::hidden(
                                   selectInput("map_datasets", "Select a dataset for the map",
                                               choices = c("FEBRUARY 2022" = "station_data_1.csv",
                                                           "AUGUST 2022" = "station_data_2.csv",
                                                           "DECEMBER 2022" = "station_data_3.csv"))),
                                 selectInput(inputId = "theme", label = "Select Theme", choices = c("Default", "Classic", "Dark")),


                               )
                               ,
                               mainPanel(
                                 tabsetPanel(type = "tab",
                                             tabPanel("Visualization",
                                                      conditionalPanel(condition = "input.plots == 'plot 1'",
                                                                       shinycssloaders::withSpinner(
                                                                         plotlyOutput("plot1"))),
                                                      conditionalPanel(condition = "input.plots == 'plot 2'",
                                                                       shinycssloaders::withSpinner(
                                                                         plotlyOutput("plot2"))),
                                                      conditionalPanel(condition = "input.plots == 'plot 3'",
                                                                       leafletOutput("my_map")),
                                                      em("Positive and negative percentages indicate an increase and decrease from the baseline period (median value between January 3 and February 6, 2020) respectively.",
                                                         hidden = TRUE)
                                             ),
                                             tabPanel("Help",
                                                      em("Some text for Tab 2"),
                                                      div(
                                                        # Image with animation
                                                        img(id = "my_img", src = "DS.jpg", width="100%", height="auto",style = "filter: blur(0px); animation: blur_effect 3s forwards;"),
                                                        br(),
                                                        # Animation style
                                                        tags$style(
                                                          "
                                                                 @keyframes blur_effect {
                                                                   from { filter: blur(0px); }
                                                                   to { filter: blur(10px); }
                                                                 }
                                                                 "
                                                        ),
                                                        # Video
                                                        tags$video(
                                                          id = "my_video",
                                                          width = "100%",
                                                          height = "auto",
                                                          controls = TRUE,
                                                          src="data_analytics.mp4",
                                                          type = "video/mp4",
                                                          style = "display:none;"
                                                        )
                                                      ),
                                                      tags$script(
                                                        "$(document).ready(function(){
                                                                 $('#my_video').on('ended', function(){
                                                                   $('#my_video').hide();
                                                                   $('#my_img').show().css('filter', 'blur(0px)').css('animation', 'blur_effect 3s forwards');
                                                                 });
                                                                 $('#my_img').on('animationend webkitAnimationEnd oAnimationEnd', function(){
                                                                   $(this).hide();
                                                                   $('#my_video').show()[0].play();
                                                                 });
                                                               });"
                                                      )
                                             ),

                                             tabPanel("Data table",dataTableOutput(outputId = "my_tabel")
                                             )
                                 ))))
                    ,
                    shiny::tabPanel("Messages",
                               icon = icon("chart-bar"),
                               tabPanel("Explore more",fluid=TRUE,
                                        tags$style(button_color_css),
                                        titlePanel("Explore more")
                               ),
                               tabPanel("Ask me",fluid=TRUE,
                                        tags$style(button_color_css),
                                        titlePanel("Ask me")
                               )

                    ),
                    shiny::tabPanel("Menu",
                               icon = icon("bars"),
                               tabPanel("Username", titlePanel(uiOutput("username_tab"))),
                               tabPanel("Permission", icon = icon("user-tag"),titlePanel(uiOutput("permission_tab"))),
                               tabPanel("Settings", icon = icon("cog")),
                               tabPanel("Support", icon = icon("hands-helping")),
                               tabPanel("More Info", icon = icon("info-circle"))

                    ),

                    fluidRow(

                      div(class = "my-class",
                          fluidRow(
                            column(12, h1("Section 7")),
                            column(12, h1("Section 8")),
                            column(12, h1("Section 9")),
                            column(12, h1("Section 10")),
                            column(12, h1("Section 11")),
                            column(12, h1("Section 12")),
                            column(12, h1("Section 13")),
                            column(12, h1("Section 14")),
                            column(12, h1("Section 15")),
                            column(12, h1("Section 16")),
                            column(12, h1("Section 17")),
                            column(12, h1("Section 18")),
                            column(12, h1("Section 19")),
                            column(12, h1("Section 20")),
                            column(12, h1("Section 21")),
                            column(12, h1("Section 22")),
                            column(12, h1("Section 23")),
                          )
                      )),

                    shiny::absolutePanel(
                      top = -5, right = 0,
                      width = "auto", height = 50,
                      style = "padding: 10px",
                      uiOutput("logoutbtn"),
                      cursor = "move"

                    )

                    )

