library(shiny)
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


# NOT RUN {
if (interactive()) {

  shinyApp(
    ui = fluidPage(
      tags$h1("Click the button"),
      actionButton(
        inputId = "success",
        label = "Launch a success sweet alert"
      ),
      actionButton(
        inputId = "error",
        label = "Launch an error sweet alert"
      )
    ),
    server = function(input, output, session) {
      observeEvent(input$success, {
        sendSweetAlert(
          session = session,
          title = "Success !!",
          text = "All in order",
          type = "success"
        )
      })
      observeEvent(input$error, {
        sendSweetAlert(
          session = session,
          title = "Error !!",
          text = "It's broken...",
          type = "error"
        )
      })
    }
  )

}
