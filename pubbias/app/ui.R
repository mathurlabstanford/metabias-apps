library(shiny)
library(shinythemes)
library(shinyFeedback)
library(shinycssloaders)
library(shinyBS)
library(rclipboard)
library(glue)
options(spinner.color = "#3498db")
options(spinner.type = 1)

tooltips <- yaml::read_yaml("docs/tooltips.yaml")
tooltip <- function(tt) bsTooltip(tt, tooltips[[tt]]$text, placement = "top")

fluidPage(
  useShinyFeedback(),
  rclipboardSetup(),
  
  theme = shinytheme("flatly"),
  includeCSS("www/styles.css"),
  tags$head(tags$base(target = "_blank")),
  
  title = "Publication bias",
  
  titlePanel("Sensitivity analysis for publication bias in meta-analyses"),
  
  fluidRow(
    column(
      width = 8,
      div(class = "bs-callout bs-callout-info",
          div(class = "docs", includeMarkdown("docs/header.md")))
    ),
  ),
  
  div(class = "bs-callout bs-callout-input",
      fluidRow(
        column(
          width = 2,
          fileInput("meta_data", "Upload meta-analysis data (csv)",
                    accept = ".csv", placeholder = "")
        ),
        column(width = 2, uiOutput("y_cols")), tooltip("y_cols"),
        column(width = 2, uiOutput("v_cols")), tooltip("v_cols"),
        column(width = 2, uiOutput("directions")), tooltip("directions"),
        column(width = 2, uiOutput("model_type")), tooltip("model_type"),
        column(width = 2, uiOutput("cluster_cols")), tooltip("cluster_cols"),
      ),
      fluidRow(column(width = 11, offset = 2, textInput("error", "")))),
  
  fluidRow(
    column(
      width = 4,
      div(
        class = "bs-callout bs-callout-output",
        div(class = "docs", includeMarkdown("docs/corrected.md")),
        uiOutput("eta_slider"), tooltip("eta_slider"),
        withSpinner(tagList(
          uiOutput("uncorrected"),
          uiOutput("corrected"),
          uiOutput("worst"),
          uiOutput("corrected_summary"),
          uiOutput("clip_corrected"),
        ))
      )
    ),
    
    column(
      width = 4,
      div(
        class = "bs-callout bs-callout-output",
        div(class = "docs", includeMarkdown("docs/svalue.md")),
        uiOutput("q_slider"), tooltip("q_slider"),
        withSpinner(tagList(
          uiOutput("sval_est"),
          uiOutput("sval_ci"),
          uiOutput("sval_summary"),
          uiOutput("clip_sval")
        ))
      )
    ),
    
    column(
      width = 4,
      div(
        class = "bs-callout bs-callout-output",
        div(class = "docs", includeMarkdown("docs/funnel.md")),
        withSpinner(tagList(
          plotOutput("funnel", inline = TRUE), #height = "auto"),
          uiOutput("download_funnel_button")
        ))
      )
    )
  )
)
