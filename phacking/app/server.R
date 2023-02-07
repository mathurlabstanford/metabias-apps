shinyServer(function(input, output) {
  
  # ----------------------------------------------------------------------------
  # overall input elements
  # ----------------------------------------------------------------------------
  
  output$y_cols <- renderUI({
    req(meta_data_raw())
    selectInput("y_col", "Column of point estimates",
                choices = c("Select a column" = "", names(meta_data_raw())))
  })
  
  output$v_cols <- renderUI({
    req(meta_data_raw())
    selectInput("v_col", "Column of estimated variances",
                choices = c("Select a column" = "", names(meta_data_raw())))
  })
  
  output$directions <- renderUI({
    req(meta_data_raw())
    selectInput("direction", "Direction",
                choices = c("favor positive", "favor negative"))
  })
  
  # ----------------------------------------------------------------------------
  # reactive values based on overall inputs
  # ----------------------------------------------------------------------------
  
  meta_data_raw <- reactive({
    req(input$meta_file)
    read_csv(input$meta_file$datapath, show_col_types = FALSE)
  })
  
  meta_data <- reactive({
    req(meta_data_raw(), input$y_col, input$v_col)
    meta_data_raw() |>
      filter(!is.na(.data[[input$y_col]]), !is.na(.data[[input$v_col]]))
  })
  
  positive <- reactive({
    req(input$direction)
    str_detect(input$direction, "positive")
  })
  
  y_vals <- reactive({
    req(meta_data(), input$y_col)
    meta_data()[[input$y_col]]
  })
  
  v_vals <- reactive({
    req(meta_data(), input$v_col)
    meta_data()[[input$v_col]]
  })
  
  # ----------------------------------------------------------------------------
  # input validation
  # ----------------------------------------------------------------------------
  
  valid_y <- reactive({
    req(y_vals())
    y_valid <- is.numeric(y_vals())
    danger("y_col", !y_valid, "values must be numeric")
    req(y_valid)
  })
  
  valid_v <- reactive({
    req(v_vals())
    v_valid <- is.numeric(v_vals()) & all(v_vals() > 0)
    danger("v_col", !v_valid, "values must be numeric & positive")
    req(v_valid)
  })
  
  valid_affirm <- reactive({
    req(y_vals(), v_vals(), input$direction)
    
    if (positive()) yi = y_vals() else yi = -y_vals()
    # TODO: could this not duplicate affirm calculation?
    pvals <- 2 * (1 - pnorm(abs(yi) / sqrt(v_vals())))
    alpha <- formals(phacking::phacking_meta)$alpha_select
    affirm <- (pvals < alpha) & (yi > 0)
    no_aff <- sum(affirm) == 0
    no_nonaff <- sum(!affirm) == 0
    no_either <- no_aff | no_nonaff
    no_dir <- if (no_aff) "affirmative" else if (no_nonaff) "nonaffirmative"
    error <- .str("There are zero {no_dir} studies – double check your columns
                  and direction.")
    danger("error", no_either, error)
    req(!no_either)
  })
  
  # ----------------------------------------------------------------------------
  # phacking_meta
  # ----------------------------------------------------------------------------
  
  uncorrected_model <- reactive({
    req(valid_y(), valid_v(), valid_affirm())
    robu_formula <- as.formula(glue("{input$y_col} ~ 1"))
    meta_model <- robumeta::robu(robu_formula,
                                 studynum = 1:nrow(meta_data()),
                                 data = meta_data(),
                                 var.eff.size = v_vals(),
                                 small = TRUE)
    meta_result <- metabias::robu_ci(meta_model)
    
    opposite_dir <- meta_result$estimate < 0 & positive() |
      meta_result$estimate > 0 & !positive()
    warn("error", opposite_dir,
         "Warning: favored direction is opposite of the pooled estimate.")
    meta_result
  })

  output$uncorrected <- renderUI({
    req(uncorrected_model())
    estimate_text("uncorrected", uncorrected_model())
  })
  
  worst_model <- reactive({
    req(valid_y(), valid_v(), valid_affirm(), corrected_model())
    robu_formula <- as.formula(glue("{input$y_col} ~ 1"))
    dnaff <- corrected_model()$data |> filter(!affirm)
    worst_model <- robumeta::robu(robu_formula,
                                  studynum = 1:nrow(dnaff),
                                  data = dnaff,
                                  var.eff.size = dnaff[[input$v_col]],
                                  small = TRUE)
    metabias::robu_ci(worst_model)
  })
  
  output$worst <- renderUI({
    req(worst_model())
    estimate_text("worst case", worst_model())
  })
  
  corrected_model <- reactive({
    req(valid_y(), valid_v(), valid_affirm())
    meta <- phacking_meta(yi = meta_data()[[input$y_col]],
                          vi = meta_data()[[input$v_col]],
                          favor_positive = positive(),
                          parallelize = FALSE)
    meta$stats <- meta$stats |> rename(estimate = mode)
    meta
  })
  
  mu <- reactive({
    corrected_model()$stats |> filter(param == "mu")
  })
  
  tau <- reactive({
    corrected_model()$stats |> filter(param == "tau")
  })
  
  output$corrected_mu <- renderUI({
    req(corrected_model())
    estimate_text("corrected mean (μ)", mu())
  })
  
  output$corrected_tau <- renderUI({
    req(corrected_model())
    estimate_text("corrected heterogeneity (τ)", tau())
  })
  
  corrected_summary <- reactive({
    req(corrected_model())
    .str("Accounting for potential <em>p</em>-hacking and publication bias that
          favor affirmative results, the estimated meta-analytic mean (μ) is
          {ci_text(mu()$estimate, mu()$ci_lower, mu()$ci_upper)} and the
          estimated standard deviation of the effects, i.e., heterogeneity (τ),
          is {ci_text(tau()$estimate, tau()$ci_lower, tau()$ci_upper)}.")
  })
  
  output$corrected_summary <- renderUI({
    req(corrected_summary())
    p(em(HTML(corrected_summary())))
  })
  
  output$clip_corrected <- renderUI({
    req(corrected_summary())
    rclipButton(
      inputId = "clipbtn_corrected",
      label = "Copy summary",
      clipText = corrected_summary(), 
      icon = icon("clipboard")
    )
  })
  
  # ----------------------------------------------------------------------------
  # qqplot
  # ----------------------------------------------------------------------------
  
  plot_qqplot <- function() {
    cm <- corrected_model()
    cm$values$tcrit <- qnorm(0.975)
    rtma_qqplot(cm) +
      theme(legend.position = "top",
            legend.title = element_blank())
  }
  
  qq_res <- 300
  qq_width <- 1200
  qq_height <- 1100
  
  output$qqplot <- renderPlot({
    req(corrected_model())
    plot_qqplot()
  }, res = qq_res, height = qq_height, width = qq_width)
  
  output$download_qqplot <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(input$meta_file$name), "_qqplot", ".png")
    },
    content = function(file) {
      ggsave(file, plot = plot_qqplot(), device = "png", dpi = qq_res,
             height = qq_height, width = qq_width, units = "px")
    }
  )
  
  output$download_qqplot_button <- renderUI({
    req(corrected_model())
    downloadButton("download_qqplot")
  })
  
  # ----------------------------------------------------------------------------
  # z_density
  # ----------------------------------------------------------------------------
  
  plot_zdensity <- function() {
    z_density(y_vals(), v_vals(), crit_color = "#dc322f")
  }
  
  zd_res <- 300
  zd_width <- 1200
  zd_height <- 800
  
  output$zdensity <- renderPlot({
    req(corrected_model())
    plot_zdensity()
  }, res = zd_res, height = zd_height, width = zd_width)
  
  output$download_zdensity <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(input$meta_file$name), "_zdensity", ".png")
    },
    content = function(file) {
      ggsave(file, plot = plot_zdensity(), device = "png", dpi = zd_res,
             height = zd_height, width = zd_width, units = "px")
    }
  )
  
  output$download_zdensity_button <- renderUI({
    req(corrected_model())
    downloadButton("download_zdensity")
  })
  
})
