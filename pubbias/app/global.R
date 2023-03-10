library(PublicationBias)
source("www/_shared/setup.R")

example_file <- "www/data/anderson_prepped.csv"
tooltip <- tooltip_fun(yaml::read_yaml("docs/tooltips.yaml"))
