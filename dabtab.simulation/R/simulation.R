# Load the MATLAB Engine for R

#######################################
##FOR SIMULATION - FLUID, ROBOTICS,.... MATLAB AS ENGINE
####################################



Dynamical systems modeling

cellular automata

lattice Boltzmann method

Barnes-Hut algorithm, a tree algorithm which is popular a popular approach to solve the N-Body problem.

the Discrete Event approach

Agent Based Models (ABM)


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
