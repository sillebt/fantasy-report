# =============================================================================
# Trade Visualization Functions
# =============================================================================
# GT themes and visualization generators for The Trade Tribunal.
# Creates trade ledger, balance sheet, marquee trades, and pick revelations.
# =============================================================================

# -----------------------------------------------------------------------------
# Required Libraries
# -----------------------------------------------------------------------------

library(gt)
library(gtExtras)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# Load configuration and trade analysis
source("R/config.R")
source("R/trade_analysis.R")

# -----------------------------------------------------------------------------
# Trade Tribunal Theme
# -----------------------------------------------------------------------------

#' Apply Trade Tribunal styling to a gt table
#' @param gt_tbl A gt table object
#' @param title Optional title for the table
#' @param subtitle Optional subtitle for the table
#' @return Styled gt table
gt_theme_tribunal <- function(gt_tbl, title = NULL, subtitle = NULL) {
  gt_tbl <- gt_tbl %>%
    # Header styling
    tab_options(
      table.background.color = TRADE_COLORS$background,
      table.font.size = px(14),
      table.border.top.style = "none",
      table.border.bottom.style = "solid",
      table.border.bottom.width = px(3),
      table.border.bottom.color = TRADE_COLORS$header,
      heading.background.color = TRADE_COLORS$header,
      heading.title.font.size = px(24),
      heading.subtitle.font.size = px(14),
      heading.align = "center",
      heading.border.bottom.style = "none",
      column_labels.background.color = TRADE_COLORS$header,
      column_labels.font.weight = "bold",
      column_labels.border.top.style = "none",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = px(2),
      column_labels.border.bottom.color = "white",
      row_group.background.color = "#e5e7eb",
      row_group.font.weight = "bold",
      data_row.padding = px(8),
      source_notes.font.size = px(11)
    ) %>%
    tab_style(
      style = list(
        cell_text(color = "white", weight = "bold")
      ),
      locations = cells_column_labels()
    ) %>%
    tab_style(
      style = list(
        cell_text(color = "white")
      ),
      locations = cells_title()
    ) %>%
    # Alternating row colors
    opt_row_striping()

  # Add title if provided
 if (!is.null(title)) {
    gt_tbl <- gt_tbl %>%
      tab_header(
        title = title,
        subtitle = subtitle
      )
  }

  gt_tbl
}

# -----------------------------------------------------------------------------
# Trade Ledger (Primary Table)
# -----------------------------------------------------------------------------

#' Format asset details for display
#' @param assets List of asset details
#' @return Character string with formatted assets
format_assets_for_display <- function(assets) {
  if (length(assets) == 0 || is.null(assets[[1]])) {
    return("")
  }

  formatted <- sapply(assets, function(asset) {
    if (is.null(asset)) return("")

    name <- asset$asset_name %||% "Unknown"
    # Use vor_points for display (the actual metric we calculate)
    points <- asset$vor_points %||% 0

    if (isTRUE(asset$is_pick) && !is.na(asset$resolved_name)) {
      # Draft pick with resolved player
      sprintf("%s\n-> %s\n(%s VOR)", name, asset$resolved_name, round(points))
    } else if (isTRUE(asset$is_pick) && isTRUE(asset$is_future)) {
      # Future/unresolved pick with projected value
      sprintf("%s\n(Projected: %s VOR)", name, round(points))
    } else if (isTRUE(asset$is_pick)) {
      # Unresolved pick
      sprintf("%s\n(Future/Unresolved)", name)
    } else {
      # Regular player
      sprintf("%s\n(%s VOR)", name, round(points))
    }
  })

  paste(formatted[formatted != ""], collapse = "\n\n")
}

#' Generate the Trade Ledger table
#' @param scored_trades Data frame from process_all_trades()
#' @param output_path Optional path to save the table
#' @return gt table object
generate_trade_ledger <- function(scored_trades, output_path = NULL) {
  if (nrow(scored_trades) == 0) {
    message("No trades to display")
    return(NULL)
  }

  # Prepare display data
  ledger_data <- scored_trades %>%
    mutate(
      # Format date - timestamp is already POSIXct
      trade_date = format(timestamp, "%m/%d/%y"),
      # Format parties
      parties = paste(side_a_owner, "<->", side_b_owner),
      # Format verdict
      verdict_display = case_when(
        verdict == "A_WINS" ~ paste(winner, "WINS\n+", margin, "pts"),
        verdict == "B_WINS" ~ paste(winner, "WINS\n+", margin, "pts"),
        TRUE ~ paste("PUSH\n", round(margin), "pts diff")
      ),
      # Verdict color class
      verdict_color = case_when(
        verdict == "A_WINS" ~ TRADE_COLORS$winner,
        verdict == "B_WINS" ~ TRADE_COLORS$winner,
        TRUE ~ TRADE_COLORS$push
      )
    ) %>%
    rowwise() %>%
    mutate(
      side_a_display = format_assets_for_display(side_a_assets),
      side_b_display = format_assets_for_display(side_b_assets)
    ) %>%
    ungroup() %>%
    arrange(desc(timestamp)) %>%
    select(
      Season = season,
      Date = trade_date,
      Parties = parties,
      `Side A Received` = side_a_display,
      `Side B Received` = side_b_display,
      Verdict = verdict_display,
      verdict_color,
      winner,
      margin
    )

  # Create the gt table
  tbl <- ledger_data %>%
    select(-verdict_color, -winner, -margin) %>%
    gt() %>%
    gt_theme_tribunal(
      title = md("**THE TRADE TRIBUNAL**"),
      subtitle = md("*2020-2024 | All Verdicts Final*")
    ) %>%
    # Format text columns for line breaks
    fmt_markdown(columns = c(`Side A Received`, `Side B Received`, Verdict)) %>%
    # Column widths
    cols_width(
      Season ~ px(70),
      Date ~ px(80),
      Parties ~ px(120),
      `Side A Received` ~ px(200),
      `Side B Received` ~ px(200),
      Verdict ~ px(120)
    ) %>%
    # Center alignment
    cols_align(align = "center", columns = c(Season, Date, Verdict)) %>%
    cols_align(align = "left", columns = c(Parties, `Side A Received`, `Side B Received`)) %>%
    # Style verdict column based on outcome
    tab_style(
      style = cell_fill(color = TRADE_COLORS$winner),
      locations = cells_body(
        columns = Verdict,
        rows = grepl("WINS", Verdict)
      )
    ) %>%
    tab_style(
      style = cell_fill(color = TRADE_COLORS$push),
      locations = cells_body(
        columns = Verdict,
        rows = grepl("PUSH", Verdict)
      )
    ) %>%
    tab_style(
      style = cell_text(color = "white", weight = "bold"),
      locations = cells_body(columns = Verdict)
    ) %>%
    # Source note
    tab_source_note(
      source_note = md("*Methodology: Points measured over 2 seasons post-trade. Win threshold: 50+ point margin.*")
    )

  # Save if path provided
  if (!is.null(output_path)) {
    gtsave(tbl, output_path)
    message(sprintf("Trade ledger saved to %s", output_path))
  }

  tbl
}

# -----------------------------------------------------------------------------
# Balance Sheet Visualization
# -----------------------------------------------------------------------------

#' Generate the Trade Balance Sheet bar chart
#' @param balance_sheet Data frame from get_trade_balance_sheet()
#' @param output_path Optional path to save the plot
#' @return ggplot object
generate_trade_balance_sheet <- function(balance_sheet, output_path = NULL) {
  if (nrow(balance_sheet) == 0) {
    message("No balance sheet data to display")
    return(NULL)
  }

  # Prepare data for plotting
  plot_data <- balance_sheet %>%
    mutate(
      bar_color = case_when(
        net_points >= TRADE_WIN_THRESHOLD ~ TRADE_COLORS$winner,
        net_points <= -TRADE_WIN_THRESHOLD ~ TRADE_COLORS$loser,
        TRUE ~ TRADE_COLORS$push
      ),
      label_position = if_else(net_points >= 0, net_points + 20, net_points - 20),
      owner = factor(owner, levels = owner)  # Preserve order
    )

  # Create diverging bar chart
  p <- ggplot(plot_data, aes(x = net_points, y = reorder(owner, net_points))) +
    # Reference line at 0
    geom_vline(xintercept = 0, color = TRADE_COLORS$header, linewidth = 1) +
    # Push zone shading
    annotate(
      "rect",
      xmin = -TRADE_WIN_THRESHOLD, xmax = TRADE_WIN_THRESHOLD,
      ymin = -Inf, ymax = Inf,
      fill = TRADE_COLORS$push, alpha = 0.15
    ) +
    # Bars
    geom_col(aes(fill = bar_color), width = 0.7) +
    scale_fill_identity() +
    # Labels
    geom_text(
      aes(label = sprintf("%+.0f pts", net_points),
          hjust = if_else(net_points >= 0, -0.1, 1.1)),
      size = 3.5,
      fontface = "bold"
    ) +
    # Trade count annotations
    geom_text(
      aes(x = if_else(net_points >= 0, -15, 15),
          label = sprintf("W:%d L:%d", trades_won, trades_lost)),
      size = 2.5,
      color = TRADE_COLORS$header,
      alpha = 0.7
    ) +
    # Theming
    labs(
      title = "THE TRADE BALANCE SHEET",
      subtitle = "Net Points Gained/Lost Through Trades (2020-2024)",
      x = "Net Fantasy Points",
      y = NULL,
      caption = "Green = Net Winner | Rose = Net Loser | Gray = Break-even Zone (50 pts)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5,
                                 color = TRADE_COLORS$header),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
      plot.caption = element_text(size = 9, color = "gray50"),
      plot.background = element_rect(fill = TRADE_COLORS$background, color = NA),
      panel.background = element_rect(fill = TRADE_COLORS$background, color = NA),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "gray80", linetype = "dashed"),
      axis.text.y = element_text(face = "bold", size = 11),
      axis.text.x = element_text(size = 10),
      plot.margin = margin(20, 30, 20, 20)
    ) +
    scale_x_continuous(
      limits = c(min(plot_data$net_points) - 100, max(plot_data$net_points) + 100),
      expand = c(0.05, 0)
    )

  # Save if path provided
  if (!is.null(output_path)) {
    ggsave(output_path, p, width = 10, height = 6, dpi = 300, bg = TRADE_COLORS$background)
    message(sprintf("Balance sheet saved to %s", output_path))
  }

  p
}

# -----------------------------------------------------------------------------
# Marquee Trade Cards
# -----------------------------------------------------------------------------

#' Generate a marquee trade callout card with proper table format
#' @param trade Single trade row from scored_trades
#' @param card_type Character. One of "heist", "robbery", "blockbuster"
#' @param output_path Optional path to save the card
#' @return gt table object styled as a card
generate_marquee_card <- function(trade, card_type = "heist", output_path = NULL) {
  if (is.null(trade) || nrow(trade) == 0) {
    return(NULL)
  }

  # Card styling based on type
  card_config <- switch(card_type,
    "heist" = list(
      icon = "HEIST OF THE LEAGUE",
      color = TRADE_COLORS$accent,
      header_bg = TRADE_COLORS$winner
    ),
    "robbery" = list(
      icon = "THE ROBBERY",
      color = TRADE_COLORS$loser,
      header_bg = TRADE_COLORS$loser
    ),
    "blockbuster" = list(
      icon = "THE BLOCKBUSTER",
      color = TRADE_COLORS$header,
      header_bg = TRADE_COLORS$header
    ),
    list(icon = "NOTABLE TRADE", color = TRADE_COLORS$push, header_bg = TRADE_COLORS$push)
  )

  # Format trade date - timestamp is already POSIXct
  trade_date <- format(trade$timestamp, "%B %d, %Y")

  # Extract and format assets for each side as data frames
  format_side_assets <- function(assets) {
    if (length(assets) == 0 || is.null(assets[[1]])) {
      return(data.frame(asset = character(), vor = numeric()))
    }

    asset_list <- lapply(assets, function(a) {
      name <- if (isTRUE(a$is_pick) && !is.na(a$resolved_name)) {
        sprintf("%s → %s", a$asset_name, a$resolved_name)
      } else if (isTRUE(a$is_pick) && isTRUE(a$is_future)) {
        sprintf("%s (Proj.)", a$asset_name)
      } else {
        a$asset_name %||% "Unknown"
      }
      vor <- a$vor_points %||% 0
      data.frame(asset = name, vor = vor, stringsAsFactors = FALSE)
    })

    df <- do.call(rbind, asset_list)
    df <- df[order(-df$vor), ]  # Sort by VOR descending
    df
  }

  side_a_df <- format_side_assets(trade$side_a_assets[[1]])
  side_b_df <- format_side_assets(trade$side_b_assets[[1]])

  # Pad to same number of rows
  max_rows <- max(nrow(side_a_df), nrow(side_b_df))
  if (nrow(side_a_df) < max_rows) {
    side_a_df <- rbind(side_a_df, data.frame(
      asset = rep("", max_rows - nrow(side_a_df)),
      vor = rep(NA_real_, max_rows - nrow(side_a_df))
    ))
  }
  if (nrow(side_b_df) < max_rows) {
    side_b_df <- rbind(side_b_df, data.frame(
      asset = rep("", max_rows - nrow(side_b_df)),
      vor = rep(NA_real_, max_rows - nrow(side_b_df))
    ))
  }

  # Create combined table data
  card_data <- data.frame(
    side_a_asset = side_a_df$asset,
    side_a_vor = side_a_df$vor,
    side_b_asset = side_b_df$asset,
    side_b_vor = side_b_df$vor,
    stringsAsFactors = FALSE
  )

  # Calculate totals
  side_a_total <- trade$side_a_vor
  side_b_total <- trade$side_b_vor

  # Verdict text
  verdict_text <- if (trade$verdict != "PUSH") {
    sprintf("%s WINS BY %s VOR", trade$winner, round(trade$margin))
  } else {
    "TOO CLOSE TO CALL - PUSH"
  }

  # Create gt table
  card <- card_data %>%
    gt() %>%
    tab_header(
      title = md(sprintf("**%s**", card_config$icon)),
      subtitle = trade_date
    ) %>%
    tab_spanner(
      label = sprintf("%s received (TOTAL: %+.0f)", trade$side_a_owner, side_a_total),
      columns = c(side_a_asset, side_a_vor)
    ) %>%
    tab_spanner(
      label = sprintf("%s received (TOTAL: %+.0f)", trade$side_b_owner, side_b_total),
      columns = c(side_b_asset, side_b_vor)
    ) %>%
    cols_label(
      side_a_asset = "Asset",
      side_a_vor = "VOR",
      side_b_asset = "Asset",
      side_b_vor = "VOR"
    ) %>%
    fmt_number(columns = c(side_a_vor, side_b_vor), decimals = 0, use_seps = FALSE) %>%
    sub_missing(columns = c(side_a_vor, side_b_vor), missing_text = "") %>%
    # Color code VOR values
    data_color(
      columns = c(side_a_vor, side_b_vor),
      palette = c(TRADE_COLORS$loser, "white", TRADE_COLORS$winner),
      domain = c(-200, 200),
      na_color = "white"
    ) %>%
    tab_options(
      table.background.color = TRADE_COLORS$background,
      table.border.top.style = "solid",
      table.border.top.width = px(4),
      table.border.top.color = card_config$color,
      table.border.left.style = "solid",
      table.border.left.width = px(4),
      table.border.left.color = card_config$color,
      table.border.right.style = "solid",
      table.border.right.width = px(4),
      table.border.right.color = card_config$color,
      table.border.bottom.style = "solid",
      table.border.bottom.width = px(4),
      table.border.bottom.color = card_config$color,
      heading.background.color = card_config$header_bg,
      heading.title.font.size = px(20),
      heading.subtitle.font.size = px(12),
      heading.align = "center",
      column_labels.font.weight = "bold",
      data_row.padding = px(6)
    ) %>%
    tab_style(
      style = cell_text(color = "white", weight = "bold"),
      locations = cells_title()
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_spanners()
    ) %>%
    tab_footnote(
      footnote = md(sprintf("**VERDICT: %s**", verdict_text))
    ) %>%
    cols_width(
      side_a_asset ~ px(180),
      side_a_vor ~ px(60),
      side_b_asset ~ px(180),
      side_b_vor ~ px(60)
    ) %>%
    cols_align(align = "right", columns = c(side_a_vor, side_b_vor)) %>%
    cols_align(align = "left", columns = c(side_a_asset, side_b_asset))

  # Save if path provided
  if (!is.null(output_path)) {
    gtsave(card, output_path)
    message(sprintf("Marquee card saved to %s", output_path))
  }

  card
}

#' Generate a combined marquee card with multiple trades
#' @param trades Data frame with multiple trade rows
#' @param card_type Character. One of "heist", "blockbuster"
#' @param output_path Optional path to save the combined card
#' @return gt table object
generate_combined_marquee_card <- function(trades, card_type = "heist", output_path = NULL) {
  if (is.null(trades) || nrow(trades) == 0) {
    return(NULL)
  }

  # Card styling based on type
  card_config <- switch(card_type,
    "heist" = list(
      icon = "TOP 3 HEISTS",
      color = TRADE_COLORS$accent,
      header_bg = TRADE_COLORS$winner
    ),
    "blockbuster" = list(
      icon = "TOP 3 BLOCKBUSTERS",
      color = TRADE_COLORS$header,
      header_bg = TRADE_COLORS$header
    ),
    list(icon = "MARQUEE TRADES", color = TRADE_COLORS$push, header_bg = TRADE_COLORS$push)
  )

  # Format each trade into rows
  all_rows <- list()

  for (i in seq_len(nrow(trades))) {
    trade <- trades[i, ]
    trade_date <- format(trade$timestamp, "%b %d, %Y")

    # Extract assets for each side
    format_side_assets <- function(assets) {
      if (length(assets) == 0 || is.null(assets[[1]])) {
        return(data.frame(asset = character(), vor = numeric()))
      }
      asset_list <- lapply(assets, function(a) {
        name <- if (isTRUE(a$is_pick) && !is.na(a$resolved_name)) {
          sprintf("%s → %s", a$asset_name, a$resolved_name)
        } else if (isTRUE(a$is_pick) && isTRUE(a$is_future)) {
          sprintf("%s (Proj.)", a$asset_name)
        } else {
          a$asset_name %||% "Unknown"
        }
        vor <- a$vor_points %||% 0
        data.frame(asset = name, vor = vor, stringsAsFactors = FALSE)
      })
      df <- do.call(rbind, asset_list)
      df[order(-df$vor), ]
    }

    side_a_df <- format_side_assets(trade$side_a_assets[[1]])
    side_b_df <- format_side_assets(trade$side_b_assets[[1]])

    # Pad to same rows
    max_rows <- max(nrow(side_a_df), nrow(side_b_df))
    if (nrow(side_a_df) < max_rows) {
      side_a_df <- rbind(side_a_df, data.frame(asset = rep("", max_rows - nrow(side_a_df)),
                                                vor = rep(NA_real_, max_rows - nrow(side_a_df))))
    }
    if (nrow(side_b_df) < max_rows) {
      side_b_df <- rbind(side_b_df, data.frame(asset = rep("", max_rows - nrow(side_b_df)),
                                                vor = rep(NA_real_, max_rows - nrow(side_b_df))))
    }

    # Verdict
    verdict <- if (trade$verdict != "PUSH") {
      sprintf("%s +%d", trade$winner, round(trade$margin))
    } else {
      "PUSH"
    }

    # Create row group data
    trade_rows <- data.frame(
      trade_num = i,
      trade_label = sprintf("#%d: %s vs %s (%s)", i, trade$side_a_owner, trade$side_b_owner, trade_date),
      side_a_header = sprintf("%s (Total: %+.0f)", trade$side_a_owner, trade$side_a_vor),
      side_b_header = sprintf("%s (Total: %+.0f)", trade$side_b_owner, trade$side_b_vor),
      side_a_asset = side_a_df$asset,
      side_a_vor = side_a_df$vor,
      side_b_asset = side_b_df$asset,
      side_b_vor = side_b_df$vor,
      verdict = verdict,
      stringsAsFactors = FALSE
    )

    all_rows[[i]] <- trade_rows
  }

  combined_data <- do.call(rbind, all_rows)

  # Create gt table
  card <- combined_data %>%
    gt(groupname_col = "trade_label") %>%
    tab_header(
      title = md(sprintf("**%s**", card_config$icon)),
      subtitle = "Biggest Winners by VOR Margin"
    ) %>%
    cols_hide(columns = c(trade_num, side_a_header, side_b_header)) %>%
    cols_label(
      side_a_asset = "Asset",
      side_a_vor = "VOR",
      side_b_asset = "Asset",
      side_b_vor = "VOR",
      verdict = "Winner"
    ) %>%
    fmt_number(columns = c(side_a_vor, side_b_vor), decimals = 0, use_seps = FALSE) %>%
    sub_missing(columns = c(side_a_vor, side_b_vor), missing_text = "") %>%
    data_color(
      columns = c(side_a_vor, side_b_vor),
      palette = c(TRADE_COLORS$loser, "white", TRADE_COLORS$winner),
      domain = c(-200, 200),
      na_color = "white"
    ) %>%
    tab_options(
      table.background.color = TRADE_COLORS$background,
      table.border.top.style = "solid",
      table.border.top.width = px(4),
      table.border.top.color = card_config$color,
      heading.background.color = card_config$header_bg,
      heading.title.font.size = px(22),
      heading.align = "center",
      row_group.background.color = "#e2e8f0",
      row_group.font.weight = "bold",
      column_labels.font.weight = "bold",
      data_row.padding = px(4)
    ) %>%
    tab_style(
      style = cell_text(color = "white", weight = "bold"),
      locations = cells_title()
    ) %>%
    cols_width(
      side_a_asset ~ px(160),
      side_a_vor ~ px(50),
      side_b_asset ~ px(160),
      side_b_vor ~ px(50),
      verdict ~ px(80)
    ) %>%
    cols_align(align = "right", columns = c(side_a_vor, side_b_vor)) %>%
    cols_align(align = "center", columns = verdict)

  if (!is.null(output_path)) {
    gtsave(card, output_path)
    message(sprintf("Combined marquee card saved to %s", output_path))
  }

  card
}

#' Generate all marquee trade cards (combined)
#' @param marquee_trades List from get_marquee_trades()
#' @param output_dir Directory to save cards
#' @return List of gt table objects
generate_all_marquee_cards <- function(marquee_trades, output_dir = NULL) {
  cards <- list()

  # Generate combined heists card (top 3 biggest winners)
  if (!is.null(marquee_trades$heists) && nrow(marquee_trades$heists) > 0) {
    output_path <- if (!is.null(output_dir)) file.path(output_dir, "trade_marquee_heists.png") else NULL
    cards$heists <- generate_combined_marquee_card(marquee_trades$heists, "heist", output_path)
  }

  # Generate combined blockbusters card (top 3 most value exchanged)
  if (!is.null(marquee_trades$blockbusters) && nrow(marquee_trades$blockbusters) > 0) {
    output_path <- if (!is.null(output_dir)) file.path(output_dir, "trade_marquee_blockbusters.png") else NULL
    cards$blockbusters <- generate_combined_marquee_card(marquee_trades$blockbusters, "blockbuster", output_path)
  }

  cards
}

# -----------------------------------------------------------------------------
# Pick Revelations Table
# -----------------------------------------------------------------------------

#' Generate the Pick Revelations table showing draft picks traded and what they became
#' @param scored_trades Data frame from process_all_trades()
#' @param output_path Optional path to save the table
#' @return gt table object
generate_pick_revelations <- function(scored_trades, output_path = NULL) {
  if (nrow(scored_trades) == 0) {
    return(NULL)
  }

  # Extract all picks from trades
  pick_data <- list()

  for (i in seq_len(nrow(scored_trades))) {
    trade <- scored_trades[i, ]
    trade_date <- format(trade$timestamp, "%m/%y")

    # Check side A assets
    for (asset in trade$side_a_assets[[1]]) {
      if (isTRUE(asset$is_pick)) {
        pick_data[[length(pick_data) + 1]] <- data.frame(
          pick = asset$asset_name,
          traded_by = trade$side_b_owner,  # B gave, A received
          traded_to = trade$side_a_owner,
          trade_date = trade_date,
          player_drafted = asset$resolved_name %||% "Unknown/Future",
          production = asset$vor_points %||% 0,
          stringsAsFactors = FALSE
        )
      }
    }

    # Check side B assets
    for (asset in trade$side_b_assets[[1]]) {
      if (isTRUE(asset$is_pick)) {
        pick_data[[length(pick_data) + 1]] <- data.frame(
          pick = asset$asset_name,
          traded_by = trade$side_a_owner,  # A gave, B received
          traded_to = trade$side_b_owner,
          trade_date = trade_date,
          player_drafted = asset$resolved_name %||% "Unknown/Future",
          production = asset$vor_points %||% 0,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(pick_data) == 0) {
    message("No draft picks found in trades")
    return(NULL)
  }

  picks_df <- bind_rows(pick_data) %>%
    distinct() %>%
    arrange(pick) %>%
    rename(
      Pick = pick,
      `Traded By` = traded_by,
      `Traded To` = traded_to,
      `Trade Date` = trade_date,
      `Player Drafted` = player_drafted,
      `Production (VOR)` = production
    )

  # Create gt table
  tbl <- picks_df %>%
    gt() %>%
    gt_theme_tribunal(
      title = md("**DRAFT PICKS: WHAT THEY BECAME**"),
      subtitle = md("*Every Pick Traded and Its Ultimate Value*")
    ) %>%
    # Format production column
    fmt_number(columns = `Production (VOR)`, decimals = 0) %>%
    # Color code production
    data_color(
      columns = `Production (VOR)`,
      palette = c(TRADE_COLORS$loser, "white", TRADE_COLORS$winner),
      domain = c(0, max(picks_df$`Production (VOR)`, na.rm = TRUE))
    ) %>%
    # Column alignment
    cols_align(align = "center") %>%
    cols_align(align = "left", columns = c(Pick, `Player Drafted`)) %>%
    # Column widths
    cols_width(
      Pick ~ px(100),
      `Traded By` ~ px(90),
      `Traded To` ~ px(90),
      `Trade Date` ~ px(80),
      `Player Drafted` ~ px(150),
      `Production (VOR)` ~ px(100)
    )

  # Save if path provided
  if (!is.null(output_path)) {
    gtsave(tbl, output_path)
    message(sprintf("Pick revelations saved to %s", output_path))
  }

  tbl
}

# -----------------------------------------------------------------------------
# Master Generation Function
# -----------------------------------------------------------------------------

#' Generate all Trade Tribunal visualizations
#' @param tribunal_results List from run_trade_tribunal()
#' @param output_dir Directory to save outputs (default: OUTPUT_PATHS$trades)
#' @return List of all generated visualizations
generate_trade_tribunal_visuals <- function(tribunal_results, output_dir = NULL) {
  if (is.null(output_dir)) {
    output_dir <- OUTPUT_PATHS$trades
  }

  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  message("\n=== GENERATING TRADE TRIBUNAL VISUALIZATIONS ===\n")

  visuals <- list()

  # 1. Trade Ledger
  message("Generating Trade Ledger...")
  visuals$ledger <- generate_trade_ledger(
    tribunal_results$scored_trades,
    file.path(output_dir, "trade_tribunal_ledger.png")
  )

  # 2. Balance Sheet
  message("Generating Balance Sheet...")
  visuals$balance_sheet <- generate_trade_balance_sheet(
    tribunal_results$balance_sheet,
    file.path(output_dir, "trade_balance_sheet.png")
  )

  # 3. Marquee Cards
  message("Generating Marquee Trade Cards...")
  visuals$marquee_cards <- generate_all_marquee_cards(
    tribunal_results$marquee_trades,
    output_dir
  )

  # 4. Pick Revelations
  message("Generating Pick Revelations...")
  visuals$pick_revelations <- generate_pick_revelations(
    tribunal_results$scored_trades,
    file.path(output_dir, "trade_pick_revelations.png")
  )

  # Save raw data for debugging
  saveRDS(tribunal_results, file.path(output_dir, "trade_analysis_data.rds"))
  message(sprintf("Raw data saved to %s", file.path(output_dir, "trade_analysis_data.rds")))

  message("\n=== TRADE TRIBUNAL VISUALIZATIONS COMPLETE ===\n")
  message(sprintf("All outputs saved to: %s", output_dir))

  visuals
}
