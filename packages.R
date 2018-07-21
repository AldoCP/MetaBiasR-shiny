# Inspired by: https://github.com/btubbs/shiny-example-1/raw/master/packages.R

the_packages = c("scales", "metafor", "ggplot2", "plotly", "cubature") #"MetaBiasR"

###########################################################

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p, dependencies = TRUE)
  }
  else {
    cat(paste("Skipping already installed package:", p, "\n"))
  }
}
invisible(sapply(the_packages, install_if_missing))

