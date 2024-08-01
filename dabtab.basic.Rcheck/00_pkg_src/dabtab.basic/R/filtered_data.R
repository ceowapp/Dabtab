library(shiny)
library(dplyr)
library(shinysky)
library(stringr)
library(shiny)
library(dplyr)
library(shinysky)

my_autocomplete_list <- c("lowest value", "highest value", "top x% value", "bottom x% value",
                          "values that contain", "duplicate value", "unique value", "above value x",
                          "below value x", "between x and y", "contain value x", "equal to",
                          "above average", "below average", "missing value", "find type")


ui <- shinyUI(
  fluidPage(
    tags$style(
      type="text/css",
      ".shiny-output-error { visibility: hidden; }",
      ".shiny-output-error:before { visibility: hidden; }"
    ),
    tags$style(
      type="text/css",
      "#search { top: 50% !important;left: 50% !important;margin-top: -100px !important;margin-left: -250px
                 !important; color: blue;font-size: 20px;font-style: italic;}"
    ),
    sidebarPanel(
      selectInput("filter_type", "Select filter type",
                  choices = c("Whole dataset", names(iris)),
                  selected = "Whole dataset"),

      #This should only for suggestion
      tabPanel("Typeahead",
               h4("Typeahead Text Input ")
               ,div(class="row-fluid ", div(class="well container-fluid",
                                            div(class="container span3",

                                                helpText("Type expression to filter dataset. "),
                                                                                  textInput.typeahead(
                                                                                    id="search"
                                                                                    ,placeholder = "Enter search term"
                                                                                    ,local = data.frame(name = my_autocomplete_list,
                                                                                                       info = ifelse(grepl("x|y|between", my_autocomplete_list),
                                                                                                                     "You need to replace x & y with numeric value.", ""),
                                                                                                       stringsAsFactors = FALSE)

                                                                                    ,valueKey = "name"
                                                                                    ,tokens=c(1:length(my_autocomplete_list))
                                                                                    ,template = HTML("<p class='repo-name'>{{name}}</p><p class='repo-description'>{{info}}</p>")
                                                                                  )

               ),
               div(class="container span9"
                   ,shinyalert("shinyalert3")
               ))
               )),

      actionButton("paste", "Paste suggestion"),

      textAreaInput("expr", "Enter filter condition", placeholder = "Please enter the expression based on the above suggestion"),


      actionButton("filter_button", "Filter")
    ),

    mainPanel(
      sliderInput("rows", "Rows to display", min=1, max=nrow(iris), value=c(1, nrow(iris))),
      tableOutput("table")
    )
  )
)
server <- function(input, output, session) {



  observeEvent(input$paste, {
    updateTextAreaInput(session, "expr", value = input$search)
  })


  observeEvent(input$filter_button,{

    # Create a reactive dataframe
    df <- reactive({
      if (input$filter_type == "Whole dataset") {
        iris[input$rows[1]:input$rows[2], , drop = FALSE]
      } else {
        iris[input$rows[1]:input$rows[2], input$filter_type, drop = FALSE]
      }
    })

    # Filter the dataframe based on the input expression
    filtered_df <- reactive({
      search_term <- input$expr
      if (grepl("above value", search_term)) {
        value <- as.numeric(regmatches(search_term, regexpr("\\d+", search_term))[1])
        if (!is.na(value)) {
          df() %>%
            filter_if(is.numeric, ~ . > value)

        }

      } else if (grepl("below value", search_term)) {
        if (grepl("below value", search_term)) {
          value <- as.numeric(regmatches(search_term, regexpr("\\d+", search_term))[1])
          if (!is.na(value)) {
            df() %>%
              filter_if(is.numeric, ~ . < value)

          }


      }
        } else if (grepl("between", search_term)) {
        x <- as.numeric(gsub("between ", "", gsub(" and .*", "", search_term)))
        y <- as.numeric(gsub(".* and ", "", search_term))


        df() %>%
          filter_if(is.numeric, ~ .>= x & . <= y)
      } else if (grepl("equal to", search_term)) {
        value <- as.numeric(gsub("equal to ", "", search_term))
        df() %>%
          filter_if(is.numeric, ~ . == value, all_vars(.))





      } else if (grepl("contain value", search_term)) {
        value <- as.character(gsub("contain value ", "", search_term))
        df() %>% filter_all(~ grepl(value, .))
      } else if (grepl("duplicate value", search_term)) {
        df() %>% filter(duplicated(.))
      } else if (grepl("unique value", search_term)) {
        df() %>% filter(!duplicated(.))
      } else if (grepl("lowest value", search_term)) {
        df() %>% slice_min(.,n = 1)
      } else if (grepl("highest value", search_term)) {
        df() %>% slice_max(., n = 1)
      } else if (grepl("above average", search_term)) {
        avg <- df() %>% summarise(across(everything(), mean))
        df() %>% filter_all(~ . > avg)
      } else if (grepl("below average", search_term)) {
        avg <- df() %>% summarise(across(everything(), mean))
        df() %>% filter_all(~ . < avg)
      } else if (grepl("top", search_term)) {
        value <- as.numeric(gsub("top ", "", gsub("% value", "", search_term)))
        n <- ceiling(value / 100 * nrow(df()))
        df() %>% arrange_all() %>% slice_tail(n)
      } else if (grepl("bottom", search_term)) {
        value <- as.numeric(gsub("bottom ", "", gsub("% value", "", search_term)))
        n <- ceiling(value / 100 * nrow(df()))
        df() %>% arrange_all() %>% slice_head(n)
        }else if (grepl("missing value", search_term)) {
          df() %>% filter_all(~ !is.na(.) & . != "")
        }else if (grepl("find", search_term)) {
          value <- as.character(gsub("find", "", search_term))
          if (value == "char") {
            df()[, sapply(df(), is.character)]
          } else if (value == "num") {
            df()[, sapply(df(), is.numeric)]
          } else if (value == "log") {
            df()[, sapply(df(), is.logical)]
          } else {
            # handle invalid input here
            # e.g., print an error message or return the original dataframe
            print("Invalid input!")
            df()
          }
        } else {
          df()
        }
    })

    # Render the filtered dataframe as a table
    output$table <- renderTable({
      filtered_df()
    })
  })
}




shinyApp(ui = ui, server = server)
