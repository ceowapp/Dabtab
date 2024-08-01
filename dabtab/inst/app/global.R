## sourcing from dabtab.data 
#options(dabtab.development = FALSE)
options(dabtab.path.data = system.file(package = "dabtab.basic"))
source(file.path(getOption("dabtab.path.basic"), "app/global.R"), encoding = getOption("dabtab.encoding", default = "UTF-8"), local = TRUE)

if (getOption("dabtab.development", default = FALSE)) {
  ifelse(grepl("dabtab", getwd()) && file.exists("../../inst"), "..", system.file(package = "dabtab")) %>%
    options(dabtab.path = .)
}

options(dabtab.path.basic = system.file(package = "dabtab.basic"))
options(dabtab.path.advanced = system.file(package = "dabtab.advanced"))
options(dabtab.path.database = system.file(package = "dabtab.database"))
options(dabtab.path.server = system.file(package = "dabtab.server"))
options(dabtab.path.python = system.file(package = "dabtab.python"))
options(dabtab.path.ai = system.file(package = "dabtab.ai"))
options(dabtab.path.ml = system.file(package = "dabtab.ml"))
options(dabtab.path.simulation = system.file(package = "dabtab.simulation"))
options(dabtab.path.automation = system.file(package = "dabtab.automation"))
options(dabtab.path.finance = system.file(package = "dabtab.finance"))
options(dabtab.path.healthcare = system.file(package = "dabtab.healthcare"))
options(dabtab.path.security = system.file(package = "dabtab.security"))
options(dabtab.path.learn = system.file(package = "dabtab.learn"))
options(dabtab.path.connect = system.file(package = "dabtab.connect"))

# sourcing from dabtab base, note that path is set in base/global.R
# source(file.path(getOption("dabtab.path.data"), "app/global.R"), encoding = getOption("dabtab.encoding", default = "UTF-8"), local = TRUE)

## setting path for figures in help files

## setting path for www resources
addResourcePath("www_basic", file.path(getOption("dabtab.path.basic"), "app/www/"))
addResourcePath("www_advanced", file.path(getOption("dabtab.path.advanced"), "app/www/"))
addResourcePath("www_database", file.path(getOption("dabtab.path.database"), "app/www/"))
addResourcePath("www_server", file.path(getOption("dabtab.path.server"), "app/www/"))
addResourcePath("www_python", file.path(getOption("dabtab.path.python"), "app/www/"))
addResourcePath("www_ai", file.path(getOption("dabtab.path.ai"), "app/www/"))
addResourcePath("www_ml", file.path(getOption("dabtab.path.ml"), "app/www/"))
addResourcePath("www_simulation", file.path(getOption("dabtab.path.simulation"), "app/www/"))
addResourcePath("www_automation", file.path(getOption("dabtab.path.automation"), "app/www/"))
addResourcePath("www_finance", file.path(getOption("dabtab.path.finance"), "app/www/"))
addResourcePath("www_healthcare", file.path(getOption("dabtab.path.healthcare"), "app/www/"))
addResourcePath("www_security", file.path(getOption("dabtab.path.security"), "app/www/"))
addResourcePath("www_learn", file.path(getOption("dabtab.path.learn"), "app/www/"))
addResourcePath("www_connect", file.path(getOption("dabtab.path.connect"), "app/www/"))


## setting path for figures in help files
addResourcePath("figures_basic", file.path(getOption("radiant.path.basic"), "app/tools/help/figures/"))
addResourcePath("figures_advanced", file.path(getOption("radiant.ath.advanced"), "app/tools/help/figures/"))
addResourcePath("figures_database", file.path(getOption("radiant.path.database"), "app/tools/help/figures/"))
addResourcePath("figures_server", file.path(getOption("radiant.path.server"), "app/tools/help/figures/"))


## loading url information
source(file.path(getOption("dabtab.path.advanced"), "app/init.R"), encoding = getOption("dabtab.encoding"), local = TRUE)
source(file.path(getOption("dabtab.path.database"), "app/init.R"), encoding = getOption("dabtab.encoding"), local = TRUE)
source(file.path(getOption("dabtab.path.server"), "app/init.R"), encoding = getOption("dabtab.encoding"), local = TRUE)
source(file.path(getOption("dabtab.path.python"), "app/init.R"), encoding = getOption("dabtab.encoding"), local = TRUE)
source(file.path(getOption("dabtab.path.ai"), "app/init.R"), encoding = getOption("dabtab.encoding"), local = TRUE)
source(file.path(getOption("dabtab.path.ml"), "app/init.R"), encoding = getOption("dabtab.encoding"), local = TRUE)
source(file.path(getOption("dabtab.path.simulation"), "app/init.R"), encoding = getOption("dabtab.encoding"), local = TRUE)
source(file.path(getOption("dabtab.path.automation"), "app/init.R"), encoding = getOption("dabtab.encoding"), local = TRUE)
source(file.path(getOption("dabtab.path.finance"), "app/init.R"), encoding = getOption("dabtab.encoding"), local = TRUE)
source(file.path(getOption("dabtab.path.healthcare"), "app/init.R"), encoding = getOption("dabtab.encoding"), local = TRUE)
source(file.path(getOption("dabtab.path.security"), "app/init.R"), encoding = getOption("dabtab.encoding"), local = TRUE)
source(file.path(getOption("dabtab.path.learn"), "app/init.R"), encoding = getOption("dabtab.encoding"), local = TRUE)
source(file.path(getOption("dabtab.path.connect"), "app/init.R"), encoding = getOption("dabtab.encoding"), local = TRUE)

options(dabtab.url.patterns = make_url_patterns())

if (!"package:dabtab" %in% search() &&
  isTRUE(getOption("dabtab.development")) &&
  getOption("dabtab.path") == "..") {
  options(dabtab.from.package = FALSE)
} else {
  options(dabtab.from.package = TRUE)
}

## to use an alternative set of .rda files with data.frames as the default adapt and
## un-comment the line below
## note that "data/" here points to inst/app/data in the dabtab directory but can
## be any (full) path on a server
# options(dabtab.init.data = list.files(path = "data/", full.names = TRUE))