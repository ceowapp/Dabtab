################################################################################
## function for dabtab.basic formulas
################################################################################

#' Convert characters to factors
#' @details Convert columns of type character to factors based on a set of rules. By default columns will be converted for small datasets (<= 100 rows) with more rows than unique values. For larger datasets, columns are converted only when the number of unique values is <= 100 and there are 30 or more rows in the data for every unique value
#' @param dataset Data frame
#' @param safx Ratio of number of rows to number of unique values
#' @param nuniq Cutoff for number of unique values
#' @param n Cutoff for small dataset
#' @examples
#' tibble(a = c("a", "b"), b = c("a", "a"), c = 1:2) %>% to_fct()
#' @export

dabAdd <- function(a, b) {
  return(a + b)
}

#' Convert characters to factors
#' @details Convert columns of type character to factors based on a set of rules. By default columns will be converted for small datasets (<= 100 rows) with more rows than unique values. For larger datasets, columns are converted only when the number of unique values is <= 100 and there are 30 or more rows in the data for every unique value
#' @param dataset Data frame
#' @param safx Ratio of number of rows to number of unique values
#' @param nuniq Cutoff for number of unique values
#' @param n Cutoff for small dataset
#' @examples
#' tibble(a = c("a", "b"), b = c("a", "a"), c = 1:2) %>% to_fct()
#' @export

dabMinus <- function(a, b) {
  return(a - b)
}

#' Convert characters to factors
#' @details Convert columns of type character to factors based on a set of rules. By default columns will be converted for small datasets (<= 100 rows) with more rows than unique values. For larger datasets, columns are converted only when the number of unique values is <= 100 and there are 30 or more rows in the data for every unique value
#' @param dataset Data frame
#' @param safx Ratio of number of rows to number of unique values
#' @param nuniq Cutoff for number of unique values
#' @param n Cutoff for small dataset
#' @examples
#' tibble(a = c("a", "b"), b = c("a", "a"), c = 1:2) %>% to_fct()
#' @export
dabMultiply <- function(a, b) {
  return(a * b)
}

#' Convert characters to factors
#' @details Convert columns of type character to factors based on a set of rules. By default columns will be converted for small datasets (<= 100 rows) with more rows than unique values. For larger datasets, columns are converted only when the number of unique values is <= 100 and there are 30 or more rows in the data for every unique value
#' @param dataset Data frame
#' @param safx Ratio of number of rows to number of unique values
#' @param nuniq Cutoff for number of unique values
#' @param n Cutoff for small dataset
#' @examples
#' tibble(a = c("a", "b"), b = c("a", "a"), c = 1:2) %>% to_fct()
#' @export
dabDevide <- function(a, b) {
  if (b != 0) {
    return(a / b)
  } else {
    print("Error: Division by zero is not allowed.")
    return(NA)
  }
}

#' Convert characters to factors
#' @details Convert columns of type character to factors based on a set of rules. By default columns will be converted for small datasets (<= 100 rows) with more rows than unique values. For larger datasets, columns are converted only when the number of unique values is <= 100 and there are 30 or more rows in the data for every unique value
#' @param dataset Data frame
#' @param safx Ratio of number of rows to number of unique values
#' @param nuniq Cutoff for number of unique values
#' @param n Cutoff for small dataset
#' @examples
#' tibble(a = c("a", "b"), b = c("a", "a"), c = 1:2) %>% to_fct()
#' @export
bindRender <- function(expr, input_y) {
  output_reactive <- reactive({
  shiny::exprToFunction(expr, env, quoted)
  }) %>% bindCache(expr)
  %>% bindEvent(input_y) 

  output_rendered <- renderText({
    output_reactive()
  })
  return(output_rendered)
}


#' List all exported functions in the "stats" package
#' @details Convert columns of type character to factors based on a set of rules. By default columns will be converted for small datasets (<= 100 rows) with more rows than unique values. For larger datasets, columns are converted only when the number of unique values is <= 100 and there are 30 or more rows in the data for every unique value
#' @param dataset Data frame
#' @param safx Ratio of number of rows to number of unique values
#' @param nuniq Cutoff for number of unique values
#' @param n Cutoff for small dataset
#' @examples
#' tibble(a = c("a", "b"), b = c("a", "a"), c = 1:2) %>% to_fct()
#' @export
 list_funcs <- function(package) {ls(package)}


# dabModulus

#' List all exported functions in the "stats" package
#' @details Convert columns of type character to factors based on a set of rules. By default columns will be converted for small datasets (<= 100 rows) with more rows than unique values. For larger datasets, columns are converted only when the number of unique values is <= 100 and there are 30 or more rows in the data for every unique value
#' @param dataset Data frame
#' @param safx Ratio of number of rows to number of unique values
#' @param nuniq Cutoff for number of unique values
#' @param n Cutoff for small dataset
#' @examples
#' tibble(a = c("a", "b"), b = c("a", "a"), c = 1:2) %>% to_fct()
#' @export
dabModulus <- function(a, b) {
  a %% b
}


#' List all exported functions in the "stats" package
#' @details Convert columns of type character to factors based on a set of rules. By default columns will be converted for small datasets (<= 100 rows) with more rows than unique values. For larger datasets, columns are converted only when the number of unique values is <= 100 and there are 30 or more rows in the data for every unique value
#' @param dataset Data frame
#' @param safx Ratio of number of rows to number of unique values
#' @param nuniq Cutoff for number of unique values
#' @param n Cutoff for small dataset
#' @examples
#' tibble(a = c("a", "b"), b = c("a", "a"), c = 1:2) %>% to_fct()
#' @export
# POWER
dabPOWER <- function(a, b) {
  a ^ b
}


#' List all exported functions in the "stats" package
#' @details Convert columns of type character to factors based on a set of rules. By default columns will be converted for small datasets (<= 100 rows) with more rows than unique values. For larger datasets, columns are converted only when the number of unique values is <= 100 and there are 30 or more rows in the data for every unique value
#' @param dataset Data frame
#' @param safx Ratio of number of rows to number of unique values
#' @param nuniq Cutoff for number of unique values
#' @param n Cutoff for small dataset
#' @examples
#' tibble(a = c("a", "b"), b = c("a", "a"), c = 1:2) %>% to_fct()
#' @export

# CEILING
dabCEILING <- function(num, devider) {
  ceiling(num / devider) * devider
}


#' List all exported functions in the "stats" package
#' @details Convert columns of type character to factors based on a set of rules. By default columns will be converted for small datasets (<= 100 rows) with more rows than unique values. For larger datasets, columns are converted only when the number of unique values is <= 100 and there are 30 or more rows in the data for every unique value
#' @param dataset Data frame
#' @param safx Ratio of number of rows to number of unique values
#' @param nuniq Cutoff for number of unique values
#' @param n Cutoff for small dataset
#' @examples
#' tibble(a = c("a", "b"), b = c("a", "a"), c = 1:2) %>% to_fct()
#' @export
# floor
dabFloor <- function(num) {
  floor(num)
}


#' List all exported functions in the "stats" package
#' @details Convert columns of type character to factors based on a set of rules. By default columns will be converted for small datasets (<= 100 rows) with more rows than unique values. For larger datasets, columns are converted only when the number of unique values is <= 100 and there are 30 or more rows in the data for every unique value
#' @param dataset Data frame
#' @param safx Ratio of number of rows to number of unique values
#' @param nuniq Cutoff for number of unique values
#' @param n Cutoff for small dataset
#' @examples
#' tibble(a = c("a", "b"), b = c("a", "a"), c = 1:2) %>% to_fct()
#' @export
# CONCATENATE
dabCONCATENATE <- function(...) {
  paste0(...)
}


#' List all exported functions in the "stats" package
#' @details Convert columns of type character to factors based on a set of rules. By default columns will be converted for small datasets (<= 100 rows) with more rows than unique values. For larger datasets, columns are converted only when the number of unique values is <= 100 and there are 30 or more rows in the data for every unique value
#' @param dataset Data frame
#' @param safx Ratio of number of rows to number of unique values
#' @param nuniq Cutoff for number of unique values
#' @param n Cutoff for small dataset
#' @examples
#' tibble(a = c("a", "b"), b = c("a", "a"), c = 1:2) %>% to_fct()
#' @export
# LEN
dabLEN <- function(text) {
  nchar(text)
}


#' List all exported functions in the "stats" package
#' @details Convert columns of type character to factors based on a set of rules. By default columns will be converted for small datasets (<= 100 rows) with more rows than unique values. For larger datasets, columns are converted only when the number of unique values is <= 100 and there are 30 or more rows in the data for every unique value
#' @param dataset Data frame
#' @param safx Ratio of number of rows to number of unique values
#' @param nuniq Cutoff for number of unique values
#' @param n Cutoff for small dataset
#' @examples
#' tibble(a = c("a", "b"), b = c("a", "a"), c = 1:2) %>% to_fct()
#' @export
# REPLACE
dabREPLACE <- function(text, from, to) {
  gsub(from, to, text)
}

#' List all exported functions in the "stats" package
#' @details Convert columns of type character to factors based on a set of rules. By default columns will be converted for small datasets (<= 100 rows) with more rows than unique values. For larger datasets, columns are converted only when the number of unique values is <= 100 and there are 30 or more rows in the data for every unique value
#' @param dataset Data frame
#' @param safx Ratio of number of rows to number of unique values
#' @param nuniq Cutoff for number of unique values
#' @param n Cutoff for small dataset
#' @examples
#' tibble(a = c("a", "b"), b = c("a", "a"), c = 1:2) %>% to_fct()
#' @export
# SUBSTITUTE
dabSUBSTITUTE <- function(text, from, to) {
  gsub(from, to, text)
}

#' List all exported functions in the "stats" package
#' @details Convert columns of type character to factors based on a set of rules. By default columns will be converted for small datasets (<= 100 rows) with more rows than unique values. For larger datasets, columns are converted only when the number of unique values is <= 100 and there are 30 or more rows in the data for every unique value
#' @param dataset Data frame
#' @param safx Ratio of number of rows to number of unique values
#' @param nuniq Cutoff for number of unique values
#' @param n Cutoff for small dataset
#' @examples
#' tibble(a = c("a", "b"), b = c("a", "a"), c = 1:2) %>% to_fct()
#' @export
# LEFT
dabLEFT <- function(text, n) {
  substr(text, 1, n)
}

#' List all exported functions in the "stats" package
#' @details Convert columns of type character to factors based on a set of rules. By default columns will be converted for small datasets (<= 100 rows) with more rows than unique values. For larger datasets, columns are converted only when the number of unique values is <= 100 and there are 30 or more rows in the data for every unique value
#' @param dataset Data frame
#' @param safx Ratio of number of rows to number of unique values
#' @param nuniq Cutoff for number of unique values
#' @param n Cutoff for small dataset
#' @examples
#' tibble(a = c("a", "b"), b = c("a", "a"), c = 1:2) %>% to_fct()
#' @export
# RIGHT
dabRIGHT <- function(text, n) {
  substr(text, nchar(text) - n + 1, nchar(text))
}


#' List all exported functions in the "stats" package
#' @details Convert columns of type character to factors based on a set of rules. By default columns will be converted for small datasets (<= 100 rows) with more rows than unique values. For larger datasets, columns are converted only when the number of unique values is <= 100 and there are 30 or more rows in the data for every unique value
#' @param dataset Data frame
#' @param safx Ratio of number of rows to number of unique values
#' @param nuniq Cutoff for number of unique values
#' @param n Cutoff for small dataset
#' @examples
#' tibble(a = c("a", "b"), b = c("a", "a"), c = 1:2) %>% to_fct()
#' @export
# MID
dabMID <- function(text, start, n) {
  substr(text, start, start + n - 1)
}


#' List all exported functions in the "stats" package
#' @details Convert columns of type character to factors based on a set of rules. By default columns will be converted for small datasets (<= 100 rows) with more rows than unique values. For larger datasets, columns are converted only when the number of unique values is <= 100 and there are 30 or more rows in the data for every unique value
#' @param dataset Data frame
#' @param safx Ratio of number of rows to number of unique values
#' @param nuniq Cutoff for number of unique values
#' @param n Cutoff for small dataset
#' @examples
#' tibble(a = c("a", "b"), b = c("a", "a"), c = 1:2) %>% to_fct()
#' @export
# UPPER
dabUPPER <- function(text) {
  toupper(text)
}

#' List all exported functions in the "stats" package
#' @details Convert columns of type character to factors based on a set of rules. By default columns will be converted for small datasets (<= 100 rows) with more rows than unique values. For larger datasets, columns are converted only when the number of unique values is <= 100 and there are 30 or more rows in the data for every unique value
#' @param dataset Data frame
#' @param safx Ratio of number of rows to number of unique values
#' @param nuniq Cutoff for number of unique values
#' @param n Cutoff for small dataset
#' @examples
#' tibble(a = c("a", "b"), b = c("a", "a"), c = 1:2) %>% to_fct()
#' @export
# LOWER
dabLOWER <- function(text) {
  tolower(text)
}

#' List all exported functions in the "stats" package
#' @details Convert columns of type character to factors based on a set of rules. By default columns will be converted for small datasets (<= 100 rows) with more rows than unique values. For larger datasets, columns are converted only when the number of unique values is <= 100 and there are 30 or more rows in the data for every unique value
#' @param dataset Data frame
#' @param safx Ratio of number of rows to number of unique values
#' @param nuniq Cutoff for number of unique values
#' @param n Cutoff for small dataset
#' @examples
#' tibble(a = c("a", "b"), b = c("a", "a"), c = 1:2) %>% to_fct()
#' @export
# PROPER
dabPROPER <- function(text) {
  tools::toTitleCase(text)
}


#' List all exported functions in the "stats" package
#' @details Convert columns of type character to factors based on a set of rules. By default columns will be converted for small datasets (<= 100 rows) with more rows than unique values. For larger datasets, columns are converted only when the number of unique values is <= 100 and there are 30 or more rows in the data for every unique value
#' @param dataset Data frame
#' @param safx Ratio of number of rows to number of unique values
#' @param nuniq Cutoff for number of unique values
#' @param n Cutoff for small dataset
#' @examples
#' tibble(a = c("a", "b"), b = c("a", "a"), c = 1:2) %>% to_fct()
#' @export
# NOW
dabNOW <- function() {
  Sys.time()
}


#' List all exported functions in the "stats" package
#' @details Convert columns of type character to factors based on a set of rules. By default columns will be converted for small datasets (<= 100 rows) with more rows than unique values. For larger datasets, columns are converted only when the number of unique values is <= 100 and there are 30 or more rows in the data for every unique value
#' @param dataset Data frame
#' @param safx Ratio of number of rows to number of unique values
#' @param nuniq Cutoff for number of unique values
#' @param n Cutoff for small dataset
#' @examples
#' tibble(a = c("a", "b"), b = c("a", "a"), c = 1:2) %>% to_fct()
#' @export
# TODAY
dabTODAY <- function() {
  Sys.Date()
}


#' List all exported functions in the "stats" package
#' @details Convert columns of type character to factors based on a set of rules. By default columns will be converted for small datasets (<= 100 rows) with more rows than unique values. For larger datasets, columns are converted only when the number of unique values is <= 100 and there are 30 or more rows in the data for every unique value
#' @param dataset Data frame
#' @param safx Ratio of number of rows to number of unique values
#' @param nuniq Cutoff for number of unique values
#' @param n Cutoff for small dataset
#' @examples
#' tibble(a = c("a", "b"), b = c("a", "a"), c = 1:2) %>% to_fct()
#' @export
# TIME
dabTIME <- function() {
  strftime(Sys.time(), format = "%T")
}


#' List all exported functions in the "stats" package
#' @details Convert columns of type character to factors based on a set of rules. By default columns will be converted for small datasets (<= 100 rows) with more rows than unique values. For larger datasets, columns are converted only when the number of unique values is <= 100 and there are 30 or more rows in the data for every unique value
#' @param dataset Data frame
#' @param safx Ratio of number of rows to number of unique values
#' @param nuniq Cutoff for number of unique values
#' @param n Cutoff for small dataset
#' @examples
#' tibble(a = c("a", "b"), b = c("a", "a"), c = 1:2) %>% to_fct()
#' @export
# HOUR
dabHOUR <- function(time) {
  format(as.POSIXlt(time), format = "%H")
}


#' List all exported functions in the "stats" package
#' @details Convert columns of type character to factors based on a set of rules. By default columns will be converted for small datasets (<= 100 rows) with more rows than unique values. For larger datasets, columns are converted only when the number of unique values is <= 100 and there are 30 or more rows in the data for every unique value
#' @param dataset Data frame
#' @param safx Ratio of number of rows to number of unique values
#' @param nuniq Cutoff for number of unique values
#' @param n Cutoff for small dataset
#' @examples
#' tibble(a = c("a", "b"), b = c("a", "a"), c = 1:2) %>% to_fct()
#' @export
# MINUTE
dabMINUTE <- function(time) {
  format(as.POSIXlt(time), format = "%M")
}


#' List all exported functions in the "stats" package
#' @details Convert columns of type character to factors based on a set of rules. By default columns will be converted for small datasets (<= 100 rows) with more rows than unique values. For larger datasets, columns are converted only when the number of unique values is <= 100 and there are 30 or more rows in the data for every unique value
#' @param dataset Data frame
#' @param safx Ratio of number of rows to number of unique values
#' @param nuniq Cutoff for number of unique values
#' @param n Cutoff for small dataset
#' @examples
#' tibble(a = c("a", "b"), b = c("a", "a"), c = 1:2) %>% to_fct()
#' @export
# SECOND
dabSECOND <- function(time) {
  format(as.POSIXlt(time), format = "%S")
}


#' List all exported functions in the "stats" package
#' @details Convert columns of type character to factors based on a set of rules. By default columns will be converted for small datasets (<= 100 rows) with more rows than unique values. For larger datasets, columns are converted only when the number of unique values is <= 100 and there are 30 or more rows in the data for every unique value
#' @param dataset Data frame
#' @param safx Ratio of number of rows to number of unique values
#' @param nuniq Cutoff for number of unique values
#' @param n Cutoff for small dataset
#' @examples
#' tibble(a = c("a", "b"), b = c("a", "a"), c = 1:2) %>% to_fct()
#' @export
# DATEDIF
dabDATEDIF <- function(start_date, end_date, unit) {
  lubridate::interval(start_date, end_date) %/% unit
}


#' List all exported functions in the "stats" package
#' @details Convert columns of type character to factors based on a set of rules. By default columns will be converted for small datasets (<= 100 rows) with more rows than unique values. For larger datasets, columns are converted only when the number of unique values is <= 100 and there are 30 or more rows in the data for every unique value
#' @param dataset Data frame
#' @param safx Ratio of number of rows to number of unique values
#' @param nuniq Cutoff for number of unique values
#' @param n Cutoff for small dataset
#' @examples
#' tibble(a = c("a", "b"), b = c("a", "a"), c = 1:2) %>% to_fct()
#' @export
# VLOOKUP
dabVLOOKUP <- function(lookup_value, table, col_index, ...) {
  dplyr::filter(table, !!col_index == lookup_value)
}


#' List all exported functions in the "stats" package
#' @details Convert columns of type character to factors based on a set of rules. By default columns will be converted for small datasets (<= 100 rows) with more rows than unique values. For larger datasets, columns are converted only when the number of unique values is <= 100 and there are 30 or more rows in the data for every unique value
#' @param dataset Data frame
#' @param safx Ratio of number of rows to number of unique values
#' @param nuniq Cutoff for number of unique values
#' @param n Cutoff for small dataset
#' @examples
#' tibble(a = c("a", "b"), b = c("a", "a"), c = 1:2) %>% to_fct()
#' @export
# HLOOKUP
dabHLOOKUP <- function(lookup_value, table, row_index, ...) {
  dplyr::filter(table, !!row_index == lookup_value)
}


#' List all exported functions in the "stats" package
#' @details Convert columns of type character to factors based on a set of rules. By default columns will be converted for small datasets (<= 100 rows) with more rows than unique values. For larger datasets, columns are converted only when the number of unique values is <= 100 and there are 30 or more rows in the data for every unique value
#' @param dataset Data frame
#' @param safx Ratio of number of rows to number of unique values
#' @param nuniq Cutoff for number of unique values
#' @param n Cutoff for small dataset
#' @examples
#' tibble(a = c("a", "b"), b = c("a", "a"), c = 1:2) %>% to_fct()
#' @export
# IF Formula
dabIF <- function(condition, true_value, false_value) {
  ifelse(condition, true_value, false_value)
}


#' List all exported functions in the "stats" package
#' @details Convert columns of type character to factors based on a set of rules. By default columns will be converted for small datasets (<= 100 rows) with more rows than unique values. For larger datasets, columns are converted only when the number of unique values is <= 100 and there are 30 or more rows in the data for every unique value
#' @param dataset Data frame
#' @param safx Ratio of number of rows to number of unique values
#' @param nuniq Cutoff for number of unique values
#' @param n Cutoff for small dataset
#' @examples
#' tibble(a = c("a", "b"), b = c("a", "a"), c = 1:2) %>% to_fct()
#' @export
# INDEX-MATCH
dabINDEXMATCH <- function(index_vector, match_vector) {
  index_vector[match(match_vector, index_vector)]
}


#' List all exported functions in the "stats" package
#' @details Convert columns of type character to factors based on a set of rules. By default columns will be converted for small datasets (<= 100 rows) with more rows than unique values. For larger datasets, columns are converted only when the number of unique values is <= 100 and there are 30 or more rows in the data for every unique value
#' @param dataset Data frame
#' @param safx Ratio of number of rows to number of unique values
#' @param nuniq Cutoff for number of unique values
#' @param n Cutoff for small dataset
#' @examples
#' tibble(a = c("a", "b"), b = c("a", "a"), c = 1:2) %>% to_fct()
#' @export
# COUNTIF
dabCOUNTIF <- function(data, condition) {
  sum(condition)
}

# Goal Seek - Unfortunately, there's no direct one-line solution in R for Excel's Goal Seek feature.

# What-If Analysis - Similar to Goal Seek, such analysis in R might involve more complex procedures.


#' List all exported functions in the "stats" package
#' @details Convert columns of type character to factors based on a set of rules. By default columns will be converted for small datasets (<= 100 rows) with more rows than unique values. For larger datasets, columns are converted only when the number of unique values is <= 100 and there are 30 or more rows in the data for every unique value
#' @param dataset Data frame
#' @param safx Ratio of number of rows to number of unique values
#' @param nuniq Cutoff for number of unique values
#' @param n Cutoff for small dataset
#' @examples
#' tibble(a = c("a", "b"), b = c("a", "a"), c = 1:2) %>% to_fct()
#' @export
# If-Else
dabIfElse <- function(condition, true_value, false_value) {
  ifelse(condition, true_value, false_value)
}


#' List all exported functions in the "stats" package
#' @details Convert columns of type character to factors based on a set of rules. By default columns will be converted for small datasets (<= 100 rows) with more rows than unique values. For larger datasets, columns are converted only when the number of unique values is <= 100 and there are 30 or more rows in the data for every unique value
#' @param dataset Data frame
#' @param safx Ratio of number of rows to number of unique values
#' @param nuniq Cutoff for number of unique values
#' @param n Cutoff for small dataset
#' @examples
#' tibble(a = c("a", "b"), b = c("a", "a"), c = 1:2) %>% to_fct()
#' @export
# If-Error
dabIfError <- function(value, error_value) {
  ifelse(is.error(value), error_value, value)
}

# Index and Match - See dabINDEXMATCH function defined above.

# Offset Function - There's no direct equivalent of Excel's OFFSET function in R.

# Unfortunately, R might not have exact functions for all Excel functions. The provided R functions try to replicate their functionality.


>>A function can

have one or multiple arguments

the argument can be string or number

