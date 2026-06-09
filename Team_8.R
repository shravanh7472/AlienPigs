read_psid_excel <- function(path) {
  if (requireNamespace("readxl", quietly = TRUE)) {
    return(as.data.frame(readxl::read_excel(path), stringsAsFactors = FALSE))
  }

  as.data.frame(openxlsx::read.xlsx(path), stringsAsFactors = FALSE)
}

clean_numeric <- function(x) {
  suppressWarnings(as.integer(x))
}

drop_codes <- function(x, codes) {
  x[x %in% codes] <- NA_integer_
  x
}

recode_child_sex <- function(x) {
  out <- rep(NA_character_, length(x))
  out[x == 1L] <- "male"
  out[x == 2L] <- "female"
  out
}

build_family_panel <- function(cah_path) {
  cah <- read_psid_excel(cah_path)
  cah$CAH3 <- clean_numeric(cah$CAH3)
  cah$CAH4 <- clean_numeric(cah$CAH4)
  cah$CAH5 <- clean_numeric(cah$CAH5)
  cah$CAH9 <- drop_codes(clean_numeric(cah$CAH9), c(98L, 99L))
  cah$CAH12 <- drop_codes(clean_numeric(cah$CAH12), c(8L, 9L))
  cah$CAH15 <- drop_codes(clean_numeric(cah$CAH15), c(9998L, 9999L))

  cah <- cah[!is.na(cah$CAH9), c("CAH3", "CAH4", "CAH5", "CAH9", "CAH12", "CAH15")]
  cah$parent_id <- paste(cah$CAH3, cah$CAH4, sep = "_")

  parents <- cah[!duplicated(cah$parent_id), c("parent_id", "CAH3", "CAH4", "CAH5")]
  names(parents) <- c(
    "parent_id",
    "parent_interview_1968",
    "parent_person_number",
    "parent_sex_code"
  )
  parents$parent_sex <- recode_child_sex(parents$parent_sex_code)

  panel <- parents

  for (k in 1:6) {
    child_detail <- cah[cah$CAH9 == k, c("parent_id", "CAH12", "CAH15")]
    child_detail <- child_detail[!duplicated(child_detail$parent_id), ]
    names(child_detail) <- c(
      "parent_id",
      sprintf("child_%d_sex_code", k),
      sprintf("child_%d_birth_year", k)
    )
    panel <- merge(panel, child_detail, by = "parent_id", all.x = TRUE)
  }

  for (k in 1:6) {
    has_k <- unique(cah$parent_id[cah$CAH9 == k])
    panel[[sprintf("has_child_%d", k)]] <- as.integer(panel$parent_id %in% has_k)
  }

  panel$first_child_birth_year <- panel$child_1_birth_year
  panel$second_child_birth_year <- panel$child_2_birth_year
  panel$third_child_birth_year <- panel$child_3_birth_year
  panel$first_child_female <- as.integer(panel$child_1_sex_code == 2L)
  panel$first_two_girls <- as.integer(
    panel$child_1_sex_code == 2L & panel$child_2_sex_code == 2L
  )
  panel$first_two_boys <- as.integer(
    panel$child_1_sex_code == 1L & panel$child_2_sex_code == 1L
  )
  panel$first_two_same_gender <- as.integer(
    !is.na(panel$child_1_sex_code) &
      !is.na(panel$child_2_sex_code) &
      panel$child_1_sex_code == panel$child_2_sex_code
  )

  panel
}

detect_weight_column <- function(x_names) {
  preferred <- c("psid_individual_weight", "ER35265")
  exact_match <- preferred[preferred %in% x_names]
  if (length(exact_match) > 0L) {
    return(exact_match[[1]])
  }

  fuzzy_match <- x_names[grepl("weight|wgt", x_names, ignore.case = TRUE)]
  if (length(fuzzy_match) > 0L) {
    return(fuzzy_match[[1]])
  }

  NULL
}

prepare_weight_data <- function(weight_path, fallback_csv_path) {
  if (file.exists(weight_path)) {
    raw <- read_psid_excel(weight_path)
    names(raw) <- trimws(names(raw))
    weight_col <- detect_weight_column(names(raw))

    if (!is.null(weight_col) && "ER82009" %in% names(raw)) {
      out <- raw[, c("ER82009", weight_col)]
      names(out) <- c("parent_interview_1968", "psid_individual_weight")
      out$parent_interview_1968 <- clean_numeric(out$parent_interview_1968)
      out$psid_individual_weight <- suppressWarnings(as.numeric(out$psid_individual_weight))
      out <- out[!duplicated(out$parent_interview_1968), ]
      return(list(data = out, merge_type = "family_key"))
    }

    if (!is.null(weight_col) && "ER82002" %in% names(raw)) {
      out <- raw[, c("ER82002", weight_col)]
      names(out) <- c("parent_interview_1968", "psid_individual_weight")
      out$parent_interview_1968 <- clean_numeric(out$parent_interview_1968)
      out$psid_individual_weight <- suppressWarnings(as.numeric(out$psid_individual_weight))
      out <- out[!duplicated(out$parent_interview_1968), ]
      return(list(data = out, merge_type = "family_key"))
    }
  }

  if (file.exists(fallback_csv_path)) {
    fallback_raw <- read.csv(fallback_csv_path, stringsAsFactors = FALSE)
    weight_col <- detect_weight_column(names(fallback_raw))
    required_keys <- c("parent_interview_1968", "parent_person_number")

    if (!is.null(weight_col) && all(required_keys %in% names(fallback_raw))) {
      out <- fallback_raw[, c(required_keys, weight_col)]
      names(out) <- c("parent_interview_1968", "parent_person_number", "psid_individual_weight")
      out$parent_interview_1968 <- clean_numeric(out$parent_interview_1968)
      out$parent_person_number <- clean_numeric(out$parent_person_number)
      out$psid_individual_weight <- suppressWarnings(as.numeric(out$psid_individual_weight))
      out <- out[!duplicated(out[, c("parent_interview_1968", "parent_person_number")]), ]
      return(list(data = out, merge_type = "parent_keys"))
    }
  }

  NULL
}

merge_weights <- function(panel, weight_info) {
  if (is.null(weight_info)) {
    panel$psid_individual_weight <- NA_real_
    return(panel)
  }

  if (identical(weight_info$merge_type, "parent_keys")) {
    return(merge(
      panel,
      weight_info$data,
      by = c("parent_interview_1968", "parent_person_number"),
      all.x = TRUE
    ))
  }

  merge(
    panel,
    weight_info$data,
    by = "parent_interview_1968",
    all.x = TRUE
  )
}

estimate_ate <- function(df, outcome_var, treat_var, weight_var = NULL) {
  use_sample <- !is.na(df[[outcome_var]]) & !is.na(df[[treat_var]])

  if (!is.null(weight_var)) {
    use_sample <- use_sample &
      !is.na(df[[weight_var]]) &
      is.finite(df[[weight_var]]) &
      df[[weight_var]] > 0
  }

  sub <- df[use_sample, , drop = FALSE]

  if (nrow(sub) == 0L) {
    return(data.frame(
      n = 0L,
      n_treated = 0L,
      n_control = 0L,
      sample_total = 0,
      ate = NA_real_,
      std_error = NA_real_,
      p_value = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  n_treated <- sum(sub[[treat_var]] == 1L, na.rm = TRUE)
  n_control <- sum(sub[[treat_var]] == 0L, na.rm = TRUE)

  if (n_treated == 0L || n_control == 0L) {
    return(data.frame(
      n = nrow(sub),
      n_treated = n_treated,
      n_control = n_control,
      sample_total = if (is.null(weight_var)) nrow(sub) else sum(sub[[weight_var]], na.rm = TRUE),
      ate = NA_real_,
      std_error = NA_real_,
      p_value = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  fit <- if (is.null(weight_var)) {
    stats::lm(stats::as.formula(sprintf("%s ~ %s", outcome_var, treat_var)), data = sub)
  } else {
    stats::lm(
      stats::as.formula(sprintf("%s ~ %s", outcome_var, treat_var)),
      data = sub,
      weights = sub[[weight_var]]
    )
  }

  coef_table <- summary(fit)$coefficients
  ate <- unname(coef_table[treat_var, "Estimate"])
  std_error <- unname(coef_table[treat_var, "Std. Error"])
  p_value <- unname(coef_table[treat_var, "Pr(>|t|)"])

  data.frame(
    n = nrow(sub),
    n_treated = n_treated,
    n_control = n_control,
    sample_total = if (is.null(weight_var)) nrow(sub) else sum(sub[[weight_var]], na.rm = TRUE),
    ate = ate,
    std_error = std_error,
    p_value = p_value,
    ci_low = ate - 1.96 * std_error,
    ci_high = ate + 1.96 * std_error,
    stringsAsFactors = FALSE
  )
}

make_bin_labels <- function(years, bin_width, start_year, end_year) {
  bin_start <- floor((years - start_year) / bin_width) * bin_width + start_year
  bin_end <- pmin(bin_start + bin_width - 1L, end_year)
  data.frame(
    bin_start = bin_start,
    bin_end = bin_end,
    bin_label = sprintf("%d-%d", bin_start, bin_end),
    stringsAsFactors = FALSE
  )
}

estimate_binned_ates <- function(df, birth_year_var, outcome_var, treat_var,
                                 bin_width, start_year, end_year, weight_var = NULL) {
  in_window <- !is.na(df[[birth_year_var]]) &
    df[[birth_year_var]] >= start_year &
    df[[birth_year_var]] <= end_year

  sub <- df[in_window, , drop = FALSE]
  if (nrow(sub) == 0L) {
    return(data.frame())
  }

  bins <- make_bin_labels(sub[[birth_year_var]], bin_width, start_year, end_year)
  sub$bin_start <- bins$bin_start
  sub$bin_end <- bins$bin_end
  sub$bin_label <- bins$bin_label

  split_df <- split(sub, sub$bin_start)
  results <- lapply(split_df, function(chunk) {
    est <- estimate_ate(chunk, outcome_var, treat_var, weight_var = weight_var)
    est$bin_start <- chunk$bin_start[[1]]
    est$bin_end <- chunk$bin_end[[1]]
    est$bin_label <- chunk$bin_label[[1]]
    est
  })

  out <- do.call(rbind, results)
  out <- out[order(out$bin_start), c(
    "bin_start",
    "bin_end",
    "bin_label",
    "n",
    "n_treated",
    "n_control",
    "sample_total",
    "ate",
    "std_error",
    "p_value",
    "ci_low",
    "ci_high"
  )]
  row.names(out) <- NULL
  out
}

plot_binned_ates <- function(results_df, overall_ate, overall_ci_low, overall_ci_high,
                             output_path, x_label) {
  plot_data <- results_df[is.finite(results_df$ate), ]
  if (nrow(plot_data) == 0L) {
    return(invisible(NULL))
  }

  png(output_path, width = 1800, height = 900, res = 150)
  layout(matrix(c(1, 2), nrow = 1), widths = c(4.8, 1.4))
  par(mar = c(8.8, 5.5, 1.8, 1.5) + 0.1, mgp = c(3.0, 0.9, 0))

  x_pos <- seq_len(nrow(plot_data))
  ylim <- range(c(plot_data$ci_low, plot_data$ci_high, overall_ate, 0), na.rm = TRUE)

  plot(
    x_pos, plot_data$ate,
    type = "n",
    xaxt = "n",
    ylim = ylim,
    xlab = "",
    ylab = "ATE",
    main = ""
  )

  usr <- par("usr")
  rect(
    xleft = usr[1],
    ybottom = overall_ci_low,
    xright = usr[2],
    ytop = overall_ci_high,
    col = rgb(0.8, 0.8, 0.8, alpha = 0.35),
    border = NA
  )
  segments(x_pos, plot_data$ci_low, x_pos, plot_data$ci_high, col = "#1F4E79", lwd = 1.5)
  segments(x_pos - 0.08, plot_data$ci_low, x_pos + 0.08, plot_data$ci_low, col = "#1F4E79", lwd = 1.5)
  segments(x_pos - 0.08, plot_data$ci_high, x_pos + 0.08, plot_data$ci_high, col = "#1F4E79", lwd = 1.5)
  lines(x_pos, plot_data$ate, lwd = 2, col = "#1F4E79")
  points(x_pos, plot_data$ate, pch = 19, cex = 0.9, col = "#1F4E79")
  abline(h = 0, lty = 3, col = "gray50")
  abline(h = overall_ate, lty = 2, col = "#AA3A2A")
  axis(side = 1, at = x_pos, labels = plot_data$bin_label, las = 2, cex.axis = 0.95)
  mtext(x_label, side = 1, line = 5.6, cex = 1)

  par(mar = c(8.8, 0.5, 1.8, 1.8) + 0.1)
  plot.new()
  legend(
    "center",
    legend = c("Bin ATE", "Bin 95% CI", "Overall ATE", "Overall 95% CI"),
    col = c("#1F4E79", "#1F4E79", "#AA3A2A", "gray70"),
    lty = c(1, 1, 2, 0),
    lwd = c(2, 1.5, 1.5, 0),
    pch = c(19, NA, NA, 22),
    pt.cex = c(0.9, NA, NA, 1.8),
    pt.bg = c(NA, NA, NA, rgb(0.8, 0.8, 0.8, alpha = 0.35)),
    bty = "n",
    seg.len = 2.3,
    y.intersp = 1.3
  )

  layout(1)
  dev.off()
}

run_binned_analysis <- function(df, output_dir, output_prefix, birth_year_var, outcome_var,
                                treat_var, start_year = 1970L, end_year = 2009L,
                                bin_widths = c(5L, 10L, 15L), weight_var = NULL,
                                x_label = "") {
  in_window <- !is.na(df[[birth_year_var]]) &
    df[[birth_year_var]] >= start_year &
    df[[birth_year_var]] <= end_year
  sample_df <- df[in_window, , drop = FALSE]

  overall <- estimate_ate(sample_df, outcome_var, treat_var, weight_var = weight_var)
  binned_results <- list()

  for (bin_width in bin_widths) {
    binned <- estimate_binned_ates(
      df = sample_df,
      birth_year_var = birth_year_var,
      outcome_var = outcome_var,
      treat_var = treat_var,
      bin_width = bin_width,
      start_year = start_year,
      end_year = end_year,
      weight_var = weight_var
    )

    plot_path <- file.path(output_dir, sprintf("%s_%dyr.png", output_prefix, bin_width))
    plot_binned_ates(
      results_df = binned,
      overall_ate = overall$ate[[1]],
      overall_ci_low = overall$ci_low[[1]],
      overall_ci_high = overall$ci_high[[1]],
      output_path = plot_path,
      x_label = x_label
    )

    binned_results[[sprintf("%d-year bins", bin_width)]] <- binned
  }

  list(overall = overall, binned = binned_results)
}

same_gender_indicator <- function(df, parity) {
  sex_cols <- paste0("child_", seq_len(parity), "_sex_code")
  sex_mat <- df[, sex_cols, drop = FALSE]

  apply(sex_mat, 1, function(row_vals) {
    if (any(is.na(row_vals))) {
      return(NA_integer_)
    }
    as.integer(length(unique(row_vals)) == 1L)
  })
}

same_prefix_indicator <- function(df, prefix_length) {
  sex_cols <- paste0("child_", seq_len(prefix_length), "_sex_code")
  sex_mat <- df[, sex_cols, drop = FALSE]

  apply(sex_mat, 1, function(row_vals) {
    if (any(is.na(row_vals))) {
      return(NA_integer_)
    }
    as.integer(length(unique(row_vals)) == 1L)
  })
}

estimate_parity_effect <- function(df, parity, weight_var = NULL) {
  prefix_same <- same_prefix_indicator(df, parity - 1L)
  first_p_same <- same_gender_indicator(df, parity)
  outcome_var <- sprintf("has_child_%d", parity + 1L)

  use_sample <- prefix_same == 1L &
    !is.na(first_p_same) &
    !is.na(df[[outcome_var]])

  if (!is.null(weight_var)) {
    use_sample <- use_sample &
      !is.na(df[[weight_var]]) &
      is.finite(df[[weight_var]]) &
      df[[weight_var]] > 0
  }

  sub <- df[use_sample, , drop = FALSE]
  if (nrow(sub) == 0L) {
    return(data.frame(
      parity = parity,
      n = 0L,
      n_treated = 0L,
      n_control = 0L,
      sample_total = 0,
      ate = NA_real_,
      std_error = NA_real_,
      p_value = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  sub$treat <- first_p_same[use_sample]
  out <- estimate_ate(sub, outcome_var, "treat", weight_var = weight_var)
  out$parity <- parity
  out <- out[, c(
    "parity",
    "n",
    "n_treated",
    "n_control",
    "sample_total",
    "ate",
    "std_error",
    "p_value",
    "ci_low",
    "ci_high"
  )]
  out
}

plot_parity_effects <- function(results_df, output_path) {
  plot_data <- results_df[is.finite(results_df$ate), ]
  if (nrow(plot_data) == 0L) {
    return(invisible(NULL))
  }

  x_labels <- ifelse(plot_data$parity >= 5, "5+", as.character(plot_data$parity))

  png(output_path, width = 1550, height = 860, res = 150)
  par(mar = c(5.8, 5.5, 1.8, 2.5) + 0.1, mgp = c(3.0, 0.9, 0))

  ylim <- range(c(plot_data$ci_low, plot_data$ci_high, 0), na.rm = TRUE)
  plot(
    plot_data$parity, plot_data$ate,
    type = "n",
    xaxt = "n",
    ylim = ylim,
    xlab = "",
    ylab = "ATE on Pr(additional child)",
    main = ""
  )

  segments(plot_data$parity, plot_data$ci_low, plot_data$parity, plot_data$ci_high, col = "#1F4E79", lwd = 1.5)
  segments(plot_data$parity - 0.06, plot_data$ci_low, plot_data$parity + 0.06, plot_data$ci_low, col = "#1F4E79", lwd = 1.5)
  segments(plot_data$parity - 0.06, plot_data$ci_high, plot_data$parity + 0.06, plot_data$ci_high, col = "#1F4E79", lwd = 1.5)
  points(plot_data$parity, plot_data$ate, pch = 19, cex = 0.9, col = "#1F4E79")
  abline(h = 0, lty = 3, col = "gray50")
  axis(side = 1, at = plot_data$parity, labels = x_labels, cex.axis = 0.95)
  mtext("Number of children of same sex", side = 1, line = 3.6, cex = 1)
  dev.off()
}

plot_third_child_comparison <- function(comparison_df, output_path) {
  png(output_path, width = 1600, height = 840, res = 150)
  par(mar = c(5.8, 5.5, 1.8, 5.5) + 0.1, mgp = c(3.0, 0.9, 0))

  x_pos <- seq_len(nrow(comparison_df))
  ylim <- range(c(comparison_df$ci_low, comparison_df$ci_high, 0), na.rm = TRUE)

  plot(
    x_pos, comparison_df$estimate,
    type = "n",
    xaxt = "n",
    xlim = c(0.9, length(x_pos) + 0.18),
    ylim = ylim,
    xlab = "",
    ylab = "ATE",
    main = ""
  )

  segments(x_pos, comparison_df$ci_low, x_pos, comparison_df$ci_high, col = "#1F4E79", lwd = 2)
  segments(x_pos - 0.08, comparison_df$ci_low, x_pos + 0.08, comparison_df$ci_low, col = "#1F4E79", lwd = 2)
  segments(x_pos - 0.08, comparison_df$ci_high, x_pos + 0.08, comparison_df$ci_high, col = "#1F4E79", lwd = 2)
  points(x_pos, comparison_df$estimate, pch = 19, cex = 1, col = "#1F4E79")
  abline(h = 0, lty = 3, col = "gray50")
  axis(side = 1, at = x_pos, labels = comparison_df$label, las = 1, cex.axis = 0.95)
  mtext("Child sex composition", side = 1, line = 2.8, cex = 1)
  dev.off()
}

script_path <- normalizePath(
  sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[[1]])
)
script_dir <- dirname(script_path)

args <- commandArgs(trailingOnly = TRUE)
cah_path <- if (length(args) >= 1L) args[[1]] else file.path(script_dir, "J360456", "J360456.xlsx")
default_weight_path <- if (file.exists(file.path(script_dir, "hetero_4.xlsx"))) {
  file.path(script_dir, "hetero_4.xlsx")
} else {
  file.path(script_dir, "heterogeneity.xlsx")
}
weight_path <- if (length(args) >= 2L) args[[2]] else default_weight_path
output_dir <- if (length(args) >= 3L) args[[3]] else file.path(script_dir, "Figures")
fallback_weight_path <- file.path(script_dir, "weighted_family_panel_preview.csv")

main_second_dir <- file.path(output_dir, "main", "second_child")
main_third_dir <- file.path(output_dir, "main", "third_child")
main_parity_dir <- file.path(output_dir, "main", "parity")

for (dir_path in c(main_second_dir, main_third_dir, main_parity_dir)) {
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
}

analysis_start_year <- 1970L
analysis_end_year <- 2009L
analysis_window_tag <- sprintf("%d_%d", analysis_start_year, analysis_end_year)

panel <- build_family_panel(cah_path)
weight_info <- prepare_weight_data(weight_path, fallback_weight_path)
panel <- merge_weights(panel, weight_info)
weight_var <- if (!is.null(weight_info)) "psid_individual_weight" else NULL

second_child_sample <- panel[
  !is.na(panel$first_child_female) &
    !is.na(panel$has_child_2),
  ,
  drop = FALSE
]

third_child_base_sample <- panel[
  panel$has_child_2 == 1L &
    !is.na(panel$child_1_sex_code) &
    !is.na(panel$child_2_sex_code) &
    !is.na(panel$has_child_3),
  ,
  drop = FALSE
]

third_child_girls_sample <- third_child_base_sample[
  third_child_base_sample$child_1_sex_code == 2L,
  ,
  drop = FALSE
]

third_child_boys_sample <- third_child_base_sample[
  third_child_base_sample$child_1_sex_code == 1L,
  ,
  drop = FALSE
]

third_child_same_gender_sample <- third_child_base_sample

second_child_results <- run_binned_analysis(
  df = second_child_sample,
  output_dir = main_second_dir,
  output_prefix = sprintf("second_child_on_first_girl_%s", analysis_window_tag),
  birth_year_var = "first_child_birth_year",
  outcome_var = "has_child_2",
  treat_var = "first_child_female",
  start_year = analysis_start_year,
  end_year = analysis_end_year,
  weight_var = weight_var,
  x_label = "Birth year of first child"
)

third_child_girls_results <- run_binned_analysis(
  df = third_child_girls_sample,
  output_dir = main_third_dir,
  output_prefix = sprintf("third_child_on_gg_vs_gb_%s", analysis_window_tag),
  birth_year_var = "second_child_birth_year",
  outcome_var = "has_child_3",
  treat_var = "first_two_girls",
  start_year = analysis_start_year,
  end_year = analysis_end_year,
  weight_var = weight_var,
  x_label = "Birth year of second child"
)

third_child_boys_results <- run_binned_analysis(
  df = third_child_boys_sample,
  output_dir = main_third_dir,
  output_prefix = sprintf("third_child_on_bb_vs_bg_%s", analysis_window_tag),
  birth_year_var = "second_child_birth_year",
  outcome_var = "has_child_3",
  treat_var = "first_two_boys",
  start_year = analysis_start_year,
  end_year = analysis_end_year,
  weight_var = weight_var,
  x_label = "Birth year of second child"
)

third_child_same_gender_results <- run_binned_analysis(
  df = third_child_same_gender_sample,
  output_dir = main_third_dir,
  output_prefix = sprintf("third_child_on_first_two_same_gender_%s", analysis_window_tag),
  birth_year_var = "second_child_birth_year",
  outcome_var = "has_child_3",
  treat_var = "first_two_same_gender",
  start_year = analysis_start_year,
  end_year = analysis_end_year,
  weight_var = weight_var,
  x_label = "Birth year of second child"
)

parity_results <- do.call(
  rbind,
  lapply(2:5, function(parity) estimate_parity_effect(panel, parity, weight_var = weight_var))
)

parity_plot_path <- file.path(main_parity_dir, "parity_same_gender_additional_child_ate.png")
plot_parity_effects(parity_results, parity_plot_path)

comparison_df <- data.frame(
  label = c("M/M", "F/F", "Combined Same Sex"),
  estimate = c(
    third_child_boys_results$overall$ate[[1]],
    third_child_girls_results$overall$ate[[1]],
    third_child_same_gender_results$overall$ate[[1]]
  ),
  std_error = c(
    third_child_boys_results$overall$std_error[[1]],
    third_child_girls_results$overall$std_error[[1]],
    third_child_same_gender_results$overall$std_error[[1]]
  ),
  p_value = c(
    third_child_boys_results$overall$p_value[[1]],
    third_child_girls_results$overall$p_value[[1]],
    third_child_same_gender_results$overall$p_value[[1]]
  ),
  stringsAsFactors = FALSE
)
comparison_df$ci_low <- comparison_df$estimate - 1.96 * comparison_df$std_error
comparison_df$ci_high <- comparison_df$estimate + 1.96 * comparison_df$std_error

comparison_plot_path <- file.path(main_third_dir, "third_child_estimate_comparison.png")
plot_third_child_comparison(comparison_df, comparison_plot_path)