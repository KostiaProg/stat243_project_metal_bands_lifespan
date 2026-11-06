library(tidyverse)
library(dplyr)
library(ggplot2)
library(forcats)
library(rlang)

if (!dir.exists("graphs")) dir.create("graphs", recursive = TRUE)

prepare_plot_data <- function(df, years_col, cols, top_n_facets = 12) {
  req_cols <- c(years_col, cols)
  missing_cols <- setdiff(req_cols, colnames(df))
  if (length(missing_cols) > 0) return(NULL)
  
  df_clean <- df %>%
    filter(is.finite(.data[[years_col]]))
  
  if (length(cols) > 0) {
    df_clean <- df_clean %>%
      filter(if_all(all_of(cols), ~ !is.na(.x) & as.character(.x) != ""))
    
    for (col_name in cols) {
      if (n_distinct(df_clean[[col_name]]) > top_n_facets) {
        df_clean[[col_name]] <- fct_lump_n(
          fct_infreq(as.factor(df_clean[[col_name]])), 
          n = top_n_facets, 
          other_level = "Other"
        )
      }
    }
  }
  
  if (nrow(df_clean) == 0) return(NULL)
  
  summary_df <- df_clean %>%
    group_by(across(all_of(cols))) %>%
    summarise(
      mean_val = mean(.data[[years_col]], na.rm = TRUE),
      median_val = median(.data[[years_col]], na.rm = TRUE),
      max_val= max(.data[[years_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      label_text = paste0("Mean: ", round(mean_val, 1), "\nMed: ", round(median_val, 1))
    )
  
  list(df_clean = df_clean, summary_df = summary_df)
}

create_lifespan_chart_graphs <- function(df, years_col = "years_lived", group_cols = c(), top_n_facets = 12) {
  build_chart <- function(cols) {
    prep <- prepare_plot_data(df, years_col, cols, top_n_facets = top_n_facets)
    if (is.null(prep)) return(NULL)
    
    df_clean <- prep$df_clean
    summary_df <- prep$summary_df
    
    df_counts <- df_clean %>%
      count(across(all_of(c(years_col, cols))), name = "n_count")
    
    ch <- ggplot(df_counts, aes(x = .data[[years_col]], y = n_count)) +
      geom_col(fill = "steelblue", color = "white", alpha = 0.8) +
      geom_vline(
        data = summary_df, 
        aes(xintercept = mean_val, color = "Mean"), 
        linetype = "dashed", linewidth = 0.8
      ) +
      geom_vline(
        data = summary_df, 
        aes(xintercept = median_val, color = "Median"), 
        linetype = "dotted", linewidth = 0.8
      ) +
      geom_label(
        data = summary_df,
        aes(x = Inf, y = Inf, label = label_text),
        hjust = 1.1, vjust = 1.1,
        size = 3, fill = "white", alpha = 0.85,
        inherit.aes = FALSE
      ) +
      scale_color_manual(
        name = "Statistics", 
        values = c("Mean" = "firebrick", "Median" = "darkgreen")
      ) +
      labs(
        x = years_col,
        y = "Count",
        title = if (length(cols) == 0) {
          paste("Overall Distribution of", years_col)
        } else {
          paste("Distribution of", years_col, "by Top", top_n_facets, paste(cols, collapse = " & "))
        }
      ) +
      theme_minimal()
    
    if (length(cols) > 0) {
      ch <- ch + facet_wrap(vars(!!!syms(cols)))
    }
    
    col_name <- if (length(cols) == 0) "overall" else paste(cols, collapse = "_")
    ggsave(
      filename = paste0("graphs/years_", col_name, "_colsplot.png"), 
      plot = ch, width = 10, height = 7, dpi = 300, device = "png"
    )
    return(ch)
  }
  
  if (is.list(group_cols)) {
    lapply(group_cols, build_chart)
  } else {
    build_chart(group_cols)
  }
}

create_lifespan_box <- function(df, years_col = "years_lived", group_cols = c(), top_n_facets = 12) {
  build_boxplot <- function(cols) {
    prep <- prepare_plot_data(df, years_col, cols, top_n_facets = top_n_facets)
    if (is.null(prep)) return(NULL)
    
    df_clean <- prep$df_clean
    summary_df <- prep$summary_df
    
    if (length(cols) == 0) {
      ch <- ggplot(df_clean, aes(x = "Overall", y = .data[[years_col]])) +
        geom_boxplot(fill = "steelblue", alpha = 0.6, width = 0.4) +
        geom_text(
          data = summary_df,
          aes(x = "Overall", y = max_val, label = label_text),
          vjust = -0.3, size = 3.5, inherit.aes = FALSE
        ) +
        labs(x = "", y = years_col, title = paste("Overall Boxplot of", years_col))
      
    } else if (length(cols) == 1) {
      x_var <- cols[1]
      ch <- ggplot(df_clean, aes(x = .data[[x_var]], y = .data[[years_col]], fill = .data[[x_var]])) +
        geom_boxplot(alpha = 0.6, show.legend = FALSE) +
        geom_text(
          data = summary_df,
          aes(x = .data[[x_var]], y = max_val, label = label_text),
          vjust = -0.3, size = 3, inherit.aes = FALSE
        ) +
        labs(
          x = x_var, 
          y = years_col, 
          title = paste("Boxplot of", years_col, "by", x_var)
        )
      
    } else {
      x_var <- cols[1]
      facet_vars <- cols[-1]
      ch <- ggplot(df_clean, aes(x = .data[[x_var]], y = .data[[years_col]], fill = .data[[x_var]])) +
        geom_boxplot(alpha = 0.6, show.legend = FALSE) +
        geom_text(
          data = summary_df,
          aes(x = .data[[x_var]], y = max_val, label = label_text),
          vjust = -0.3, size = 2.8, inherit.aes = FALSE
        ) +
        facet_wrap(vars(!!!syms(facet_vars))) +
        labs(
          x = x_var,
          y = years_col,
          title = paste("Boxplot of", years_col, "by", paste(cols, collapse = " & "))
        )
    }
    
    ch <- ch +
      stat_summary(
        fun = mean, 
        geom = "point", 
        shape = 18, 
        size = 3.5, 
        color = "firebrick"
      ) +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
    
    col_name <- if (length(cols) == 0) "overall" else paste(cols, collapse = "_")
    ggsave(
      filename = paste0("graphs/years_", col_name, "_boxplot.png"), 
      plot = ch, width = 10, height = 7, dpi = 300, device = "png"
    )
    return(ch)
  }
  
  if (is.list(group_cols)) {
    lapply(group_cols, build_boxplot)
  } else {
    build_boxplot(group_cols)
  }
}

create_group_mean_median_bars <- function(df, years_col = "years_lived", group_cols = c("Genre", "Country_x"), top_n = 12) {
  if (!dir.exists("graphs")) dir.create("graphs", recursive = TRUE)
  
  for (col in group_cols) {
    if (!col %in% colnames(df)) next
    
    df_clean <- df %>%
      filter(is.finite(.data[[years_col]])) %>%
      filter(!is.na(.data[[col]]) & as.character(.data[[col]]) != "")
    
    top_categories <- df_clean %>%
      count(.data[[col]], sort = TRUE) %>%
      slice_head(n = top_n) %>%
      pull(1)
    
    if (length(top_categories) == 0) next
    
    summary_df <- df_clean %>%
      filter(.data[[col]] %in% top_categories) %>%
      group_by(across(all_of(col))) %>%
      summarise(
        Mean = mean(.data[[years_col]], na.rm = TRUE),
        Median = median(.data[[years_col]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_longer(
        cols = c(Mean, Median),
        names_to = "Statistic",
        values_to = "Value"
      )
    
    summary_df[[col]] <- factor(summary_df[[col]], levels = top_categories)
    
    ch <- ggplot(summary_df, aes(x = .data[[col]], y = Value, fill = Statistic)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "black") +
      geom_text(
        aes(label = round(Value, 1)),
        position = position_dodge(width = 0.8),
        vjust = -0.4,
        size = 3.2
      ) +
      scale_fill_manual(values = c("Mean" = "#2b5c8f", "#e07a5f")) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(
        x = col,
        y = paste("Years (", years_col, ")", sep = ""),
        title = paste("Top", length(top_categories), col, "- Mean vs Median Lifespan"),
        fill = "Statistic"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 35, hjust = 1, face = "bold"),
        legend.position = "top"
      )
    
    ggsave(
      filename = paste0("graphs/years_", col, "_mean_median_bars.png"),
      plot = ch, width = 10, height = 6, dpi = 300, device = "png"
    )
  }
}

lifespan_bands_graphs <- function(
    df,
    years_col = "years_lived",
    graphs_chart_cols = list(c()),
    group_bars_cols = list(c("Genre"), c("Country_band")),
    boxplots_cols_orig = c("total_releases_count", "full_length_ep_count", "single_count"),
    top_n_facets = 12
) {
  create_lifespan_chart_graphs(df, years_col, graphs_chart_cols, top_n_facets = top_n_facets)
  create_group_mean_median_bars(df, years_col, group_bars_cols, top_n = 12)
  
  box_levels <- c("0", "1-3", "4-6", "7-9", "10-12", "13-15", "15+")
  
  df_binned <- df %>%
    mutate(
      across(
        all_of(intersect(boxplots_cols_orig, colnames(df))),
        ~ factor(
          case_when(
            .x >= 1 & .x <= 3 ~ "1-3",
            .x >= 4 & .x <= 6 ~ "4-6",
            .x >= 7 & .x <= 9 ~ "7-9",
            .x >= 10 & .x <= 12 ~ "10-12",
            .x >= 13 & .x <= 15 ~ "13-15",
            .x == 0 ~ "0",
            .x > 15 ~ "15+",
            TRUE ~ NA_character_
          ),
          levels = box_levels
        ),
        .names = "{.col}_boxes"
      )
    )
  
  boxplots_cols <- paste0(intersect(boxplots_cols_orig, colnames(df)), "_boxes")
  
  for (col in boxplots_cols) {
    create_lifespan_box(df_binned, years_col, col, top_n_facets = top_n_facets)
  }
}
