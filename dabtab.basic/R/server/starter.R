openPageUI<-fluidPage(
  tags$head(
    tags$style(HTML("
        html, body {
          height: 100%;
        }
        .column {
          height: 100%;
          background-color: #F2F3F4 ;
        }
        #left-panel {
          border-right: none;
          background-color: green;
        }
        #middle-panel {
          width: 66%;
          background-color: grey;
        }
        #right-panel {
          border-left: none;
          background-color: yellow;
        }
    "))
  ),

  div(
    class = "dabtab-css",
    fluidRow(
      column(
        width = 3,
        class = "column",
        id = "left-panel",
        tabPanel("Account Settings", id = "tab18", titlePanel(uiOutput("username_tab"))),
        tabPanel("Algorithm", id = "tab13", icon = icon("user-tag"), titlePanel(uiOutput("permission_tab"))),
        tabPanel("Visualization", id = "tab14", icon = icon("cog")),
        tabPanel("Analysis", id = "tab16", icon = icon("info-circle"))
      ),
      column(
        width = 6,
        class = "column",
        id = "middle-panel",
        actionBttn(
          inputId = "start_proj",
          label = "Create Project",
          style = "jelly",
          color = "royal"
        )
      ),
      column(
        width = 3,
        class = "column",
        id = "right-panel",
        tabPanel("Overview", id = "tab18", titlePanel(uiOutput("username_tab"))),
        tabPanel("How-to Videos", id = "tab13", icon = icon("user-tag"), titlePanel(uiOutput("permission_tab"))),
        tabPanel("Interface", id = "tab14", icon = icon("cog")),
        tabPanel("From scratch", id = "tab16", icon = icon("info-circle"))
      )
    )
  ),

  tags$script(HTML("
    $(document).ready(function() {
      $('body,html').css('height','100%');
      $('.column').css('height', ($(window).height())+'px');
    });
  "))
)
