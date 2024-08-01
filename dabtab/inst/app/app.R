server<- source("server.R", local = TRUE)$value
# Define the overall UI
ui <- source("ui.R", local = TRUE)$value


# Run the application
runApp(list(ui = ui, server = server),host="192.168.xx.xx",port=5013, launch.browser = TRUE)
