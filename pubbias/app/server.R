library(shiny)
library(shinyFeedback)
library(glue)
library(tidyverse)
library(markdown)
library(PublicationBias)

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

shinyServer(function(input, output) {
  
  # ----------------------------------------------------------------------------
  # overall input elements
  # ----------------------------------------------------------------------------
  
  output$y_cols <- renderUI({
    req(input$meta_data)
    selectInput("y_col", "Column of point estimates",
                choices = c("Select a column" = "", names(meta_data_raw())))
  })
  
  output$v_cols <- renderUI({
    req(input$meta_data)
    selectInput("v_col", "Column of estimated variances",
                choices = c("Select a column" = "", names(meta_data_raw())))
  })
  
  output$directions <- renderUI({
    req(input$meta_data, input$v_col)
    selectInput("direction", "Direction", #selected = dir,
                choices = c("favor positive", "favor negative"))
  })
  
  output$model_type <- renderUI({
    req(input$meta_data, input$y_col, input$v_col, input$direction)
    selectInput("model_type", "Model type",
                choices = c("fixed-effects" = "fixed",
                            "robust random-effects" = "robust"))
  })
  
  output$cluster_cols <- renderUI({
    req(input$meta_data, input$y_col, input$v_col, input$direction,
        input$model_type)
    if (input$model_type == "robust") {
      selectInput("cluster_col", "Column of cluster labels",
                  choices = c("", "[none]", names(meta_data_raw())))
    }
  })
  
  # ----------------------------------------------------------------------------
  # reactive values based on overall inputs
  # ----------------------------------------------------------------------------
  
  meta_data_raw <- reactive({
    meta_file <- input$meta_data
    # ext <- tools::file_ext(meta_file$datapath)
    # req(meta_file)
    # validate(need(ext == "csv", "Please upload a csv file"))
    # danger("meta_data", ext == "csv", "please upload a csv file")
    read_csv(meta_file$datapath, show_col_types = FALSE)
    
    # read_csv("../escalc_example.csv", show_col_types = FALSE) |>
    #   mutate("unique_id" = 1:n(), .before = 1)
    
    # example_df <- metafor::escalc(measure = "RR", ai = tpos, bi = tneg,
    #                               ci = cpos, di = cneg, data = metafor::dat.bcg)
    # example_df$yi <- -example_df$yi
    # example_df
  })
  
  meta_data <- reactive({
    req(input$meta_data, input$y_col, input$v_col)
    meta_data_raw() |>
      filter(!is.na(.data[[input$y_col]]), !is.na(.data[[input$v_col]]))
  })
  
  positive <- reactive({
    req(input$direction)
    str_detect(input$direction, "positive")
  })
  
  cluster_col <- reactive({
    req(input$model_type)
    cc <- input$cluster_col
    cluster_none <- !is.null(cc) && str_detect(cc, "none")
    fixed <- str_detect(input$model_type, "fixed")
    if (!fixed && (is.null(cc) || cc == "")) return(FALSE) # for robust model, wait for cluster selection
    if (fixed || cluster_none) 1:nrow(meta_data()) else meta_data()[[cc]]
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
    alpha <- formals(PublicationBias::pubbias_meta)$alpha_select
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
  # pubbias_meta
  # ----------------------------------------------------------------------------
  
  output$eta_slider <- renderUI({
    req(uncorrected_model())
    sliderInput("eta", "Selection ratio (η)", value = 2, min = 1, max = 20, step = 1)
  })
  
  # TODO: run pubbias_meta with selection_ratio 1 instead?
  uncorrected_model <- reactive({
    req(valid_y(), valid_v(), input$model_type, valid_affirm(), cluster_col())
    if (input$model_type == "fixed") {
      meta_model <- metafor::rma(yi = y_vals(), vi = v_vals(), method = "FE")
      meta_result <- list(estimate = meta_model$beta,
                          ci_lower = meta_model$ci.lb,
                          ci_upper = meta_model$ci.ub)
    } else if (input$model_type == "robust") {
      robu_formula <- as.formula(glue("{input$y_col} ~ 1"))
      meta_model <- robumeta::robu(robu_formula,
                                   studynum = cluster_col(),
                                   data = meta_data(),
                                   var.eff.size = v_vals(),
                                   small = TRUE)
      meta_result <- metabias::robu_ci(meta_model)
    }
    opposite_dir <- meta_result$estimate < 0 & positive() |
      meta_result$estimate > 0 & !positive()
    warn("error", opposite_dir,
         "Warning: favored direction is opposite of the pooled estimate.")
    meta_result
  })
  
  meta_model <- reactive({
    req(input$eta, valid_y(), valid_v(), valid_affirm(),
        input$model_type, cluster_col())
    pubbias_meta(yi = meta_data()[[input$y_col]],
                 vi = meta_data()[[input$v_col]],
                 selection_ratio = input$eta,
                 cluster = cluster_col(),
                 model_type = input$model_type,
                 favor_positive = positive(),
                 return_worst_meta = TRUE)
  })
  
  corrected_model <- reactive({
    meta_model()$stats |> filter(model == "pubbias")
  })

  worst_model <- reactive({
    meta_model()$stats |> filter(model == "worst_case")
  })
  
  output$uncorrected <- renderUI({
    req(uncorrected_model())
    estimate_text("uncorrected", uncorrected_model())
  })
  
  output$corrected <- renderUI({
    req(corrected_model())
    estimate_text("corrected", corrected_model())
  })
  
  output$worst <- renderUI({
    req(worst_model())
    estimate_text("worst-case", worst_model())
  })
  
  corrected_summary <- reactive({
    req(corrected_model())
    more_likely <- if (positive()) "positive" else "negative"
    less_likely <- if (positive()) "negative" else "positive"
    cm <- corrected_model()
    wm <- worst_model()
    .str("If affirmative (i.e., significant and {more_likely}) studies were
         {input$eta} times more likely to be published than nonaffirmative
         (i.e., nonsignificant or {less_likely}) studies, the meta-analytic
         point estimate corrected for publication bias would be
         {ci_text(cm$estimate, cm$ci_lower, cm$ci_upper)}.<br>
         If there were worst-case publication bias (i.e., that favors
         affirmative results infinitely more than nonaffirmative results), the
         corrected meta-analytic point estimate would be
         {ci_text(wm$estimate, wm$ci_lower, wm$ci_upper)}.")
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
  # svalue
  # ----------------------------------------------------------------------------
  
  sval_model <- reactive({
    req(input$q, valid_y(), valid_v(), valid_affirm(),
        input$model_type, cluster_col())
    pubbias_svalue(yi = meta_data()[[input$y_col]],
                   vi = meta_data()[[input$v_col]],
                   q = input$q,
                   cluster = cluster_col(),
                   favor_positive = positive(),
                   model_type = input$model_type)
  })
  
  sval <- reactive({
    req(sval_model())
    sval_model()$stats
  })
  
  output$q_slider <- renderUI({
    req(uncorrected_model())
    m0 <- uncorrected_model()$estimate
    q_range <- if (m0 < 0) c(m0, 0) else c(0, m0)
    q_range <- round(q_range, 2)
    sliderInput("q", "q", value = 0, min = q_range[1], max = q_range[2],
                step = 0.01)
  })
  
  output$sval_est <- renderUI({
    req(sval())
    p(strong(
      glue("Publication bias required to shift point estimate to {input$q}:")),
      br(), sval_print(sval()$sval_est)
    )
  })
  
  output$sval_ci <- renderUI({
    req(sval())
    p(strong(glue("Publication bias required to shift CI limit to {input$q}:")),
      br(), sval_print(sval()$sval_ci))
  })
  
  sval_summary <- reactive({
    req(sval())
    more_likely <- if (positive()) "positive" else "negative"
    less_likely <- if (positive()) "negative" else "positive"
    sval_text <- function(var, val) {
      if (str_detect(val, "Not possible")) {
        .str("Under this model of publication bias, there is no amount of
             publication bias that would shift the {var} to 0.")
      } else if (str_detect(var, "bound") & str_detect(val, "--")) {
        .str("Since the uncorrected CI already contains {input$q}, it is not
             relevant to consider publication bias to shift the CI to include
             {input$q}.")
      } else {
        .str("For the {var} corrected for publication bias to shift to
             {input$q}, affirmative (i.e., significant and {more_likely})
             studies would need to be {sval_print(val)} times more likely to be
             published than nonaffirmative (i.e, nonsignificant or
             {less_likely}) studies.")
      }
    }
    paste(sval_text("point estimate", sval()$sval_est),
          sval_text("CI bound", sval()$sval_ci),
          sep = "<br>")
  })
  
  output$sval_summary <- renderUI({
    p(em(HTML(sval_summary())))
  })
  
  output$clip_sval <- renderUI({
    req(sval_summary())
    rclipButton(
      inputId = "clipbtn_sval",
      label = "Copy summary",
      clipText = str_replace(sval_summary(), "<br>", "\n"),
      icon = icon("clipboard")
    )
  })
  
  # ----------------------------------------------------------------------------
  # significance_funnel
  # ----------------------------------------------------------------------------
  
  funnel_plot <- function() {
    significance_funnel(yi = y_vals(), vi = v_vals(),
                        favor_positive = positive(),
                        est_all = uncorrected_model()$estimate,
                        est_worst = worst_model()$estimate) +
      theme_classic(base_family = "Lato") +
      theme(legend.position = "top",
            legend.title = element_blank())
  }
  
  fp_res <- 300
  fp_width <- 1200
  fp_height <- 1100
  
  output$funnel <- renderPlot({
    req(uncorrected_model(), worst_model())
    funnel_plot()
  }, res = fp_res, height = fp_height, width = fp_width)
  
  output$download_funnel <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(input$meta_data$name), "_funnel", ".png")
    },
    content = function(file) {
      ggsave(file, plot = funnel_plot(), device = "png", dpi = fp_res,
             height = fp_height, width = fp_width, units = "px")
    }
  )
  
  output$download_funnel_button <- renderUI({
    req(uncorrected_model(), worst_model())
    downloadButton("download_funnel")
  })
  
})
