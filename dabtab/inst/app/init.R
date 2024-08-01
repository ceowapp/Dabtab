fluidPage_ui <- if (interactive()) {
  fluidPage(theme = bslib::bs_theme(version = 4),
                introjsUI(),
                useShinyjs(),
                useSweetAlert(),
                tags$style(
                  type="text/css",
                  ".shiny-output-error { visibility: hidden; }",
                  ".shiny-output-error:before { visibility: hidden; }"
                ),
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

                  edited-cell {
                    background-color: lightyellow;
                  },

                  .logout-btn:hover {
                    background-color: #e60000;
                  }
                  .navbar-nav:last-of-type .nav-item:nth-child(2) {
                  margin-right: 0px;
                  margin-left: auto;

                  }

                .tag {
                  display: inline-block;
                  border: 1px solid #ccc;
                  padding: 5px 10px;
                  border-radius: 5px;
                  margin: 5px;
                  background-color: #f1f1f1;
                }
                .tag .close {
                  cursor: pointer;
                  font-size: 14px;
                  font-weight: bold;
                  margin-left: 5px;
                  color: #777;
                }

          ")
      ))
  )

}

