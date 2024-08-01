################################################################################
## function helps for dabtab.basic formulas
################################################################################

# Global variable for storing help texts
list_formula_help <- list()

# Function to generate UI help and populate global list
generate_ui_help <- function(name, output, list) {
  list[[name]] <- output
  return(list)
}

# Function for dabSum
output$dabSum <- renderUI({
  withMathJax(helpText('Dynamic output 1:  $$\\alpha^2$$'))
})
list_formula_help <- generate_ui_help("dabSum", output$dabSum, list_formula_help)

# Function for dabMinus
output$dabMinus <- renderUI({
  withMathJax(
    helpText('and output 2 $$3^2+4^2=5^2$$'),
    helpText('and output 3 $$\\sin^2(\\theta)+\\cos^2(\\theta)=1$$')
  )
})
list_formula_help <- generate_ui_help("dabMinus", output$dabMinus, list_formula_help)

# Function for dabAdd
output$dabAdd <- renderUI({
  withMathJax(
    helpText('The busy Cauchy distribution
             $$\\frac{1}{\\pi\\gamma\\,\\left[1 +
             \\left(\\frac{x-x_0}{\\gamma}\\right)^2\\right]}\\!$$')
  )
})
list_formula_help <- generate_ui_help("dabAdd", output$dabAdd, list_formula_help)

# Function for dabRemove
output$dabRemove <- renderUI({
  invalidateLater(5000, session)
  x <- round(rcauchy(1), 3)
  withMathJax(sprintf("If \\(X\\) is a Cauchy random variable, then
                      $$P(X \\leq %.03f ) = %.03f$$", x, pcauchy(x)))
})
list_formula_help <- generate_ui_help("dabRemove", output$dabRemove, list_formula_help)

# Function for dabMultiply
output$dabMultiply <- renderUI({
  if (!input$ex5_visible) return()
  withMathJax(
    helpText('You do not see me initially: $$e^{i \\pi} + 1 = 0$$')
  )
})
list_formula_help <- generate_ui_help("dabMultiply", output$dabMultiply, list_formula_help)
