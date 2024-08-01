
themeModifier <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(6,
           sliderInput(ns("title_size"), "Title font size", min = 12, max = 30, value = 18),
           selectInput(ns("title_color"), "Title color",
                       choices = c("black", "white", "red", "green", "blue"),
                       selected = "Black"),
           sliderInput(ns("axis_size"), "Axis font size", min = 8, max = 24, value = 14),
           selectInput(ns("axis_color"), "Axis color",
                       choices = c("Black", "White", "Red", "Green", "Blue"),
                       selected = "White"),
           selectInput(ns("fill_color"), "Background color",
                       choices = c("Black", "White", "Red", "Green", "Blue"),
                       selected = "Black"),
           selectInput(ns("fill_color_1"), "Panel background color",
                       choices = c("Black", "White", "Red", "Green", "Blue"),
                       selected = "Black"),
           selectInput(ns("font_family"), "Font family",
                       choices = c("Arial", "Helvetica", "Times New Roman"),
                       selected = "Arial")
    ))

}

my_theme_element<-function(title_size, title_color,fill_color,
                           axis_size, axis_color, font_family,
                           label = "Input text: ") {
  print(paste("title_size =", title_size, "title_color =", title_color, "axis_size =", axis_size, "axis_color =", axis_color, "fill_color =", fill_color, "font_family =", font_family))

  my_theme <- theme(plot.background = element_rect(fill = fill_color),
                    plot.title = element_text(size = title_size, color = title_color,
                                              family = font_family, face = "bold"),
                    axis.title = element_text(size = axis_size, color = axis_color,
                                              family = font_family),
                    axis.text = element_text(size = axis_size, color = axis_color,
                                             family = font_family))
  return(my_theme)
}


myThemeServer <- function(id) {
  moduleServer(
    id,

    function(input, output, session) {
      ns <- session$ns

      # Assign the function to a variable outside of the reactive expression

      my_custom_theme <- reactiveValues(result = NULL)

      observeEvent(c(input$title_size, input$title_color, input$axis_size,
                     input$fill_color, input$axis_color, input$font_family),
                   {
                     my_custom_theme$result <- my_theme_element(
                       title_size = input$title_size,
                       title_color = input$title_color,
                       axis_size = input$axis_size,
                       fill_color = input$fill_color,
                       axis_color = input$axis_color,
                       font_family = input$font_family
                     )
                   }
      )

      return(my_custom_theme)
    }
  )
}

