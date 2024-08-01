
dabtab <- function(state, ...) dabtab.basic::launch(package = "dabtab", run = "browser", state, ...)

#' Launch dabtab in an Rstudio window
#'
#' @param state Path to state file to load
#' @param ... additional arguments to pass to shiny::runApp (e.g, port = 8080)
#'
#' @details See \url{https://dabtab-rstats.github.io/docs/} for documentation and tutorials
#'
#' @importFrom dabtab.basic launch
#'
#' @examples
#' \dontrun{
#' dabtab_window()
#' }
#' @export
dabtab_window <- function(state, ...) dabtab.basic::launch(package = "dabatab", run = "window", state, ...)

#' Launch dabtab in the Rstudio viewer
#'
#' @details See \url{https://dabtab-rstats.github.io/docs/} for documentation and tutorials
#'
#' @param state Path to state file to load
#' @param ... additional arguments to pass to shiny::runApp (e.g, port = 8080)
#'
#' @importFrom dabtab.basic launch
#'
#' @examples
#' \dontrun{
#' dabtab_viewer()
#' }
#' @export
dabtab_viewer <- function(state, ...) dabtab.basic::launch(package = "dabtab", run = "viewer", state, ...)

#' Start dabtab but do not open a browser
#'
#' @param state Path to statefile to load
#' @param ... additional arguments to pass to shiny::runApp (e.g, port = 8080)
#'
#' @examples
#' \dontrun{
#' dabtab_url()
#' }
#' @export
dabtab_url <- function(state, ...) dabtab.basic::launch(package = "dabtab", run = FALSE, state, ...)

#' Create a launcher and updater for Windows (.bat)
#'
#' @details On Windows a file named 'dabtab.bat' and one named 'update_dabtab.bat' will be put on the desktop. Double-click the file to launch the specified dabtab app or update dabtab to the latest version
#'
#' @param app App to run when the desktop icon is double-clicked ("dabtab", "dabtab.basic", etc.). Default is "dabtab"
#' @param port Port to use for shiny::runApp (e.g, port = 4444)
#' @param pdir Project directory to use. Default is Sys.getenv("HOME")
#'
#' @examples
#' \dontrun{
#' dabtab::win_launcher()
#' }
#'
#' @export
win_launcher <- function(app = c("dabtab", "dabtab.basic"), port = 4444, pdir = Sys.getenv("HOME")) {
  if (!interactive()) stop("This function can only be used in an interactive R session")

  if (Sys.info()["sysname"] != "Windows") {
    return(message("This function is for Windows only. For Mac use the mac_launcher() function"))
  }

  answ <- readline("Do you want to create shortcuts for DabTab on your Desktop? (y/n) ")
  if (substr(answ, 1, 1) %in% c("y", "Y")) {
    local_dir <- Sys.getenv("R_LIBS_USER")
    if (!file.exists(local_dir)) dir.create(local_dir, recursive = TRUE)

    pt <- file.path(Sys.getenv("HOME"), "Desktop")
    if (!file.exists(pt)) {
      pt <- file.path(Sys.getenv("USERPROFILE"), "Desktop", fsep = "\\")
    }

    if (!file.exists(pt)) {
      pt <- Sys.getenv("HOME")
      message(paste0("The launcher function was unable to find your Desktop. The launcher and update files/icons will be put in the directory: ", pt))
    }

    pt <- normalizePath(pt, winslash = "/")

    if (rmarkdown::pandoc_available()) {
      rsp <- Sys.getenv("RSTUDIO_PANDOC")
      rsp <- paste0("Sys.setenv(RSTUDIO_PANDOC='", rsp, "');")
    } else {
      rsp <- ""
    }

    fn1 <- file.path(pt, "dabtab.bat")
    launch_string <- paste0("\"", Sys.which("R"), "\" -e \"setwd('", normalizePath(pdir, winslash = "/"), "'); options(dabtab.launch_dir = normalizePath(getwd())); if (!require(dabtab)) { install.packages('dabtab', repos = 'https://dabtab-rstats.github.io/minicran/', type = 'binary') }; ", rsp, "library(dabtab); shiny::runApp(system.file('app', package='", app[1], "'), port = ", port, ", launch.browser = TRUE)\"")
    cat(launch_string, file = fn1, sep = "\n")
    Sys.chmod(fn1, mode = "0755")

    fn2 <- file.path(pt, "update_dabtab.bat")
    launch_string <- paste0("\"", Sys.which("R"), "\" -e \"unlink('~/.dabtab.sessions/*.rds', force = TRUE); install.packages('dabtab', repos = 'https://dabtab-rstats.github.io/minicran/', type = 'binary'); suppressWarnings(update.packages(lib.loc = .libPaths()[1], repos = 'https://dabtab-rstats.github.io/minicran', ask = FALSE, type = 'binary'))\"\npause(1000)")
    cat(launch_string, file = fn2, sep = "\n")
    Sys.chmod(fn2, mode = "0755")

    if (file.exists(fn1) && file.exists(fn2)) {
      message("Done! Look for a file named dabtab.bat on your desktop. Double-click it to start DabTab in your default browser. There is also a file called update_dabtab.bat you can double click to update the version of DabTab on your computer.\n")
    } else {
      message("Something went wrong. No shortcuts were created.")
    }
  } else {
    message("No shortcuts were created.\n")
  }
}

#' Create a launcher and updater for Mac (.command)
#'
#' @details On Mac a file named 'dabtab.command' and one named 'update_dabtab.command' will be put on the desktop. Double-click the file to launch the specified dabtab app or update dabtab to the latest version
#'
#' @param app App to run when the desktop icon is double-clicked ("dabtab", "dabtab.basic", etc.). Default is "dabtab"
#' @param port Port to use for shiny::runApp (e.g, port = 4444)
#' @param pdir Project directory to use. Default is Sys.getenv("HOME")
#'
#' @examples
#' \dontrun{
#' dabtab::mac_launcher()
#' }
#'
#' @export
mac_launcher <- function(app = c("dabtab","dabtab.basic"), port = 4444, pdir = Sys.getenv("HOME")) {
  if (!interactive()) stop("This function can only be used in an interactive R session")

  if (Sys.info()["sysname"] != "Darwin") {
    return(message("This function is for Mac only. For windows use the win_launcher() function"))
  }

  answ <- readline("Do you want to create shortcuts for DabTab on your Desktop? (y/n) ")
  if (substr(answ, 1, 1) %in% c("y", "Y")) {
    local_dir <- Sys.getenv("R_LIBS_USER")
    if (!file.exists(local_dir)) dir.create(local_dir, recursive = TRUE)

    if (rmarkdown::pandoc_available()) {
      rsp <- Sys.getenv("RSTUDIO_PANDOC")
      rsp <- paste0("Sys.setenv(RSTUDIO_PANDOC='", rsp, "');")
    } else {
      rsp <- ""
    }

    fn1 <- paste0("/Users/", Sys.getenv("USER"), "/Desktop/dabtab.command")
    launch_string <- paste0("#!/usr/bin/env Rscript\nsetwd('", pdir, "')\noptions(dabtab.launch_dir = normalizePath(getwd()))\nif (!require(dabtab)) {\n  install.packages('dabtab', repos = 'https://dabtab-rstats.github.io/minicran/', type = 'binary')\n}\n", rsp, "\nlibrary(dabtab)\nshiny::runApp(system.file('app', package='", app[1], "'), port = ", port, ", launch.browser = TRUE)\n")
    cat(launch_string, file = fn1, sep = "\n")
    Sys.chmod(fn1, mode = "0755")

    fn2 <- paste0("/Users/", Sys.getenv("USER"), "/Desktop/update_dabtab.command")
    launch_string <- paste0("#!/usr/bin/env Rscript\nunlink('~/.dabtab.sessions/*.rds', force = TRUE)\ninstall.packages('dabtab', repos = 'https://dabtab-rstats.github.io/minicran/', type = 'binary')\nsuppressWarnings(update.packages(lib.loc = .libPaths()[1], repos = 'https://dabtab-rstats.github.io/minicran', ask = FALSE, type = 'binary'))\nSys.sleep(1000)")
    cat(launch_string, file = fn2, sep = "\n")
    Sys.chmod(fn2, mode = "0755")

    if (file.exists(fn1) && file.exists(fn2)) {
      message("Done! Look for a file named dabtab.command  on your desktop. Double-click it to start DabTab in your default browser. There is also a file called update_dabtab.command you can double click to update the version of DabTab on your computer.\n")
    } else {
      message("Something went wrong. No shortcuts were created.")
    }
  } else {
    message("No shortcuts were created.\n")
  }
}

#' Create a launcher and updater for Linux (.sh)
#'
#' @details On Linux a file named 'dabtab.sh' and one named 'update_dabtab.sh' will be put on the desktop. Double-click the file to launch the specified dabtab app or update dabtab to the latest version
#'
#' @param app App to run when the desktop icon is double-clicked ("dabtab", "dabtab.basic", etc.). Default is "dabtab"
#' @param port Port to use for shiny::runApp (e.g, port = 4444)
#' @param pdir Project directory to use. Default is Sys.getenv("HOME")
#'
#' @examples
#' \dontrun{
#' dabtab::lin_launcher("dabtab")
#' }
#'
#' @export
lin_launcher <- function(app = c("dabtab", "dabtab.basic"), port = 4444, pdir = Sys.getenv("HOME")) {
  if (!interactive()) stop("This function can only be used in an interactive R session")

  if (Sys.info()["sysname"] != "Linux") {
    return(message("This function is for Linux only. For windows use the win_launcher() function and for mac use the mac_launcher() function"))
  }

  answ <- readline("Do you want to create shortcuts for DabTab on your Desktop? (y/n) ")
  if (substr(answ, 1, 1) %in% c("y", "Y")) {
    local_dir <- Sys.getenv("R_LIBS_USER")
    if (!file.exists(local_dir)) dir.create(local_dir, recursive = TRUE)

    if (rmarkdown::pandoc_available()) {
      rsp <- Sys.getenv("RSTUDIO_PANDOC")
      rsp <- paste0("Sys.setenv(RSTUDIO_PANDOC='", rsp, "');")
    } else {
      rsp <- ""
    }

    fn1 <- paste0("/Users/", Sys.getenv("USER"), "/Desktop/dabtab.sh")
    launch_string <- paste0("#!/usr/bin/env Rscript\nsetwd('", pdir, "')\noptions(dabtab.launch_dir = normalizePath(getwd()))\nif (!require(dabtab)) {\n  install.packages('dabtab', repos = 'https://packagemanager.rstudio.com/all/__linux__/focal/latest')\n}\n", rsp, "\nlibrary(dabtab)\nshiny::runApp(system.file('app', package='", app[1], "'), port = ", port, ", launch.browser = TRUE)\n")
    cat(launch_string, file = fn1, sep = "\n")
    Sys.chmod(fn1, mode = "0755")

    fn2 <- paste0(Sys.getenv("HOME"), "/Desktop/update_dabtab.sh")
    launch_string <- paste0("#!/usr/bin/env Rscript\nunlink('~/.dabtab.sessions/*.rds', force = TRUE)\ninstall.packages('dabtab', repos = 'https://dabtab-rstats.github.io/minicran/')\nsuppressWarnings(update.packages(lib.loc = .libPaths()[1], repos = 'https://dabtab-rstats.github.io/minicran', ask = FALSE))\nSys.sleep(1000)")
    cat(launch_string, file = fn2, sep = "\n")
    Sys.chmod(fn2, mode = "0755")

    if (file.exists(fn1) && file.exists(fn2)) {
      message("Done! Look for a file named dabtab.sh on your desktop. Double-click it to start DabTab in your default browser. There is also a file called update_dabtab.sh you can double click to update the version of DabTab on your computer.\n\nIf the .sh files are opened in a text editor when you double-click them go to File Manager > Edit > Preferences > Behavior and click 'Run executable text files when they are opened'.")
    } else {
      message("Something went wrong. No shortcuts were created.")
    }
  } else {
    message("No shortcuts were created.\n")
  }
}

#' Create a launcher on the desktop for Windows (.bat), Mac (.command), or Linux (.sh)
#'
#' @details On Windows/Mac/Linux a file named dabtab.bat/dabtab.command/dabtab.sh will be put on the desktop. Double-click the file to launch the specified dabtab app
#'
#' @seealso \code{\link{win_launcher}} to create a shortcut on Windows
#' @seealso \code{\link{mac_launcher}} to create a shortcut on Mac
#' @seealso \code{\link{lin_launcher}} to create a shortcut on Linux
#'
#' @param app App to run when the desktop icon is double-clicked ("analytics", "marketing", "quant", or "base"). Default is "analytics"
#' @param port Port to use for shiny::runApp (e.g, port = 4444)
#' @param pdir Project directory to use. Default is Sys.getenv("HOME")
#'
#' @examples
#' \dontrun{
#' dabtab::launcher("dabtab.model")
#' }
#'
#' @export
launcher <- function(app = c("dabtab", "dabtab.basic"), port = 4444, pdir = Sys.getenv("HOME")) {
  os <- Sys.info()["sysname"]
  if (os == "Darwin") {
    mac_launcher(app[1], port = port, pdir = pdir)
  } else if (os == "Windows") {
    win_launcher(app[1], port = port, pdir = pdir)
  } else if (os == "Linux") {
    lin_launcher(app[1], port = port, pdir = pdir)
  } else {
    return(message("This function is not available for your platform."))
  }
}
