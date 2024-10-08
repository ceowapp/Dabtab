% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dabtab.R
\name{is.empty}
\alias{is.empty}
\title{Is a variable empty}
\usage{
is.empty(x, empty = "\\\\s*")
}
\arguments{
\item{x}{Character value to evaluate}

\item{empty}{Indicate what 'empty' means. Default is empty string (i.e., "")}
}
\value{
TRUE if empty, else FALSE
}
\description{
Is a variable empty
}
\details{
Is a variable empty
}
\examples{
is.empty("")
is.empty(NULL)
is.empty(NA)
is.empty(c())
is.empty("none", empty = "none")
is.empty("")
is.empty("   ")
is.empty(" something  ")
is.empty(c("", "something"))
is.empty(c(NA, 1:100))
is.empty(mtcars)
}
