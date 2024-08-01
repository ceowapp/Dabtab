options(HTTPUserAgent = sprintf("R/%s R (%s)", getRversion(), paste(getRversion(), R.version$platform, R.version$arch, R.version$os)))
repos = c(
  MINICRAN = "https://github.com/aigeniusxadmin/dabtdab-minicran/",
  CRAN = "https://cloud.r-project.org"
)

os <- Sys.info()["sysname"]
repos <- repos[c(1, 2)]
options(repos = repos)

## install script for DABTAB
build <- function(type = ifelse(os == "Linux", "source", "binary")) {
  install.packages("remotes", dependencies = FALSE, type = type)
  remotes::install_github("aigeniusxadmin/dabtdab.update", upgrade = "never")

  ## get list of packages to update
  op <- old.packages(lib.loc = .libPaths()[1])

  ## keep track of any package install issues (windoze)
  err_packages <- c()

  ## keep track of loaded name space on Windoze
  lns <- loadedNamespaces()

  if (length(op) > 0) {
    op <- op[,"Package"]

    os <- Sys.info()["sysname"]
    if (os == "Windows") {
      if ("yaml" %in% op && "yaml" %in% lns) {
        op <- op[-which(op == "yaml")]
        err_packages <- "yaml"
      }
    }
  }

  ## installs and unpacks one at time
  ## seems more robust than update.packages
  if (length(op) > 0) {
    cat("\n#################################################\n")
    cat("Updating previously installed packages")
    cat("\n#################################################\n\n")

    for (p in op) {
      if (p %in% lns) {
        err <- try(unloadNamespace(p), silent = TRUE)
        if (inherits(err, "try-error")) {
          cat("** There might be an issue updating package", p, "**\n")
          cat(err, "\n")
          err_packages <- c(err_packages, p)
        }
        if (p == "yaml") loadNamespace("yaml")
      }

      ## sometimes trying to install dependencies (e.g., Matrix) causes problems
      ## all deps should already be available in op by using the minicran package
      ## and all packages will be installed 1-by-1
      update.packages(
        lib.loc = .libPaths()[1], ask = FALSE, oldPkgs = p, dependencies = FALSE,
        type = type
      )
    }
  }

  ## additional packages ... not required but useful
  np <- new.packages(lib.loc = .libPaths()[1], ask = FALSE)

  if (length(np) > 0) {
    cat("\n#################################################\n")
    cat("Installing new packages")
    cat("\n#################################################\n\n")

    for (p in np) {
      if (p %in% lns) {
        err <- try(unloadNamespace(p), silent = TRUE)
        if (inherits(err, "try-error")) {
          cat("** There might be an issue installing package", p, "**\n")
          cat(err, "\n")
          err_packages <- c(err_packages, p)
        }
        if (p == "yaml") loadNamespace("yaml")
      }
      ## sometimes trying to install dependencies (e.g., Matrix) causes problems
      ## all deps should already be available in op by using the minicran package
      ## and all packages will be installed 1-by-1
      install.packages(p, dependencies = FALSE, type = type)
    }
  }


  # see https://github.com/wch/webshot/issues/25#event-740360519
  if (is.null(webshot:::find_phantom())) {
    webshot::install_phantomjs()
  }

  if (length(err_packages) == 0 | identical(err_packages, "yaml")) {
    message('\nTesting if dabtab can be loaded ...')
    success <- "\nDabtab update successfully completed\n"
    failure <- "\nDabtab update attempt was unsuccessful."
    os <- Sys.info()["sysname"]
    if (os == "Windows") {
      if (rstudioapi::versionInfo()$version >= "1.1.383") {
        ret <- suppressPackageStartupMessages(require("dabtab"))
        if (ret) {
          message(success)
        } else {
          message(failure)
        }
      } else {
        ## require(...) reliably causes Rstudio crash (1.1.359)
        cmd <- "source('https://github.com/aigeniusxadmin/dabtdab-minicran/blob/master/win_check.R')"
        ret <- .rs.restartR(cmd)
      }
    } else {
      ret <- suppressPackageStartupMessages(require("dabtdab"))
      if (ret) {
        message(success)
      } else {
        message(failure)
      }
    }
  }

  return(unique(err_packages))
}

updater <- function() {

  ul <- try(
    sapply(
      c("dabtab", "dabtab.basic"),
      unloadNamespace),
    silent = TRUE
  )

  rv <- R.Version()
  rv <- paste(rv$major, rv$minor, sep = ".")

  if (rv < "3.6") {
    message("dabtab requires R-3.6.0 or later. Please install the latest\nversion of R from https://cloud.r-project.org/")
  } else {

    os <- Sys.info()["sysname"]
    if (os == "Windows") {
      build()
    } else if (os == "Darwin") {
      resp <- system("sw_vers -productVersion", intern = TRUE)
      if (resp < "10.9") {
        message("Your version of macOS is no longer supported by R. You will need to upgrade the OS before proceeding\n\n")
      } else {
        build()
      }
    } else {
      build()
    }
  }
}

err <- updater()
err <- err[!err %in% installed.packages()]

## https://stackoverflow.com/questions/50422627/different-results-from-deparse-in-r-3-4-4-and-r-3-5
dctrl <- if (getRversion() > "3.4.4") c("keepNA", "niceNames") else "keepNA"

if (length(err) > 0) {
  cat("\n\n###############################################################\n\n")
  cat("  Some packages were not successfully installed or updated\n\n")
  cat("  Restart R(studio) and use the command(s) below to install these packages:\n\n")
  rerr <- grepl("^dabtab", err)
  if (sum(rerr) > 0) {
    err_dabtab <- paste0(deparse(err[rerr], control = dctrl, width.cutoff = 500L), collapse = "")
    err <- paste0(deparse(err[!rerr], control = dctrl, width.cutoff = 500L), collapse = "")
    if (length(err) > 0) {
      cat(paste0("  install.packages(", err, ")\n"))
    }
    cat(paste0("  install.packages(", err_dabtab, ")\n"))
  } else {
    err <- paste0(deparse(err, control = dctrl, width.cutoff = 500L), collapse = "")
    cat(paste0("  install.packages(", err, ")\n"))
  }
  cat("\n###############################################################\n\n")
}

rm(updater, build, err)
