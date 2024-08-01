# Hello, world!
#
# This is an example function named 'hello' 
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# Load the MATLAB Engine for R

#######################################
##WEB SCRAPING  - DATA FROM WEBSITE
####################################

library(Matlab)

# Start the MATLAB engine
matlab <- matlab.init()

# Define a Shiny app
shinyApp(
  ui = fluidPage(
    actionButton("runMatlab", "Run MATLAB Script")
  ),
  server = function(input, output) {
    observeEvent(input$runMatlab, {
      # Run a MATLAB script
      result <- matlab$eval("1 + 2")
      # Print the result
      print(result)
    })
  }
)
