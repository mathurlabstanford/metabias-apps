library(phacking)

source("www/_shared/setup.R")

example_file <- "www/data/money_priming_meta.csv"
tooltip <- tooltip_fun(yaml::read_yaml("docs/tooltips.yaml"))
# shinyOptions(cache = cachem::cache_disk(file.path(dirname(tempdir()), "phacking-cache")))
