ui <- fluidPage(
  # Other UI elements
  tags$head(
    tags$script(
      HTML(
        "var socket = new WebSocket('ws://your-server-url');",
        "socket.onmessage = function(event) {",
        "  Shiny.setInputValue('socketData', event.data);",
        "};"
      )
    )
  )
)


server <- function(input, output, session) {
  # Handle received data from WebSocket
  observeEvent(input$socketData, {
    message <- input$socketData
    # Process the received message
  })
}
