fluidPage(
  useShinyjs(),
  useShinyFeedback(),
  rclipboardSetup(),
  
  theme = shinytheme(shiny_theme),
  includeCSS("www/_shared/styles.css"),
  tags$head(tags$base(target = "_blank")),
  
  title = "p-hacking",
  
  titlePanel("Sensitivity analysis for phacking in meta-analyses"),
  
  fluidRow(
    column(
      width = 8,
      div(class = "bs-callout bs-callout-info",
          div(class = "docs", includeMarkdown("docs/header.md"))
      )
    ),
  ),
  
  div(class = "bs-callout bs-callout-input",
      fluidRow(
        column(
          width = 3,
          fileInput("meta_file", "Upload meta-analysis data (csv)",
                    accept = ".csv", placeholder = ""),
        ),
        column(width = 2, uiOutput("y_cols")), tooltip("y_cols"),
        column(width = 2, uiOutput("v_cols")), tooltip("v_cols"),
        column(width = 2, uiOutput("directions")), tooltip("directions"),
      ),
      fluidRow(
        column(width = 3, actionButton("add_example", "Show example dataset",
                                       class = "btn btn-file")),
        column(width = 3, textInput("default", "")),
        column(width = 7, textInput("error", ""))
      )
  ),
  
  fluidRow(
    column(
      width = 4,
      div(
        class = "bs-callout bs-callout-output",
        div(class = "docs", includeMarkdown("docs/corrected.md")),
        withSpinner(tagList(
          uiOutput("uncorrected"),
          uiOutput("worst"),
          uiOutput("corrected_mu"),
          uiOutput("corrected_tau"),
          uiOutput("corrected_summary"),
          uiOutput("clip_corrected"),
        ))
      )
    ),
    
    column(
      width = 4,
      div(
        class = "bs-callout bs-callout-output",
        div(class = "docs", includeMarkdown("docs/qqplot.md")),
        withSpinner(tagList(
          plotOutput("qqplot", inline = TRUE),
          uiOutput("download_qqplot_button")
        ))
      )
    ),

    column(
      width = 4,
      div(
        class = "bs-callout bs-callout-output",
        div(class = "docs", includeMarkdown("docs/zdensity.md")),
        withSpinner(tagList(
          plotOutput("zdensity", inline = TRUE),
          uiOutput("download_zdensity_button")
        ))
      )
    )
  )
)
