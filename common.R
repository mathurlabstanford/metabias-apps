library(shiny)
library(shinythemes)
library(shinyFeedback)
library(shinycssloaders)
library(shinyBS)
library(rclipboard)
library(glue)
library(tidyverse)
library(markdown)


# theming options
shiny_theme <- "flatly"
ggplot2::theme_set(theme_classic(base_family = "Lato"))
options(spinner.color = "#3498db")
options(spinner.type = 1)


# tooltip generator
tooltip_fun <- function(tooltips) function(tt) bsTooltip(tt, tooltips[[tt]]$text, placement = "top")

# ------------------------------------------------------------------------------
# helper functions for formatting
# ------------------------------------------------------------------------------

.str <- function(s) {
  paste(strwrap(glue(s, .envir = parent.frame())), collapse = " ")
}

ci_text <- function(estimate, ci_lower, ci_upper, sig = 2) {
  .str("{signif(estimate, sig)} (95% CI [{signif(ci_lower, sig)},
        {signif(ci_upper, sig)}])")
}

estimate_text <- function(model_label, model_result, sig = 2) {
  if (is.null(model_result)) ci <- ""
  else ci <- ci_text(model_result$estimate, model_result$ci_lower,
                     model_result$ci_upper, sig = sig)
  p(strong(glue("{str_to_sentence(model_label)} estimate:")), br(), ci)
}

sval_print <- function(sval) if (is.numeric(sval)) signif(sval, 2) else sval

danger <- function(inputId, show, text) {
  feedbackDanger(inputId, show, text, color = "#e74c3c", icon = NULL)
}

warn <- function(inputId, show, text) {
  feedbackWarning(inputId, show, text, color = "#f39c12", icon = NULL)
}
