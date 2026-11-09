writeLines(capture.output(sessionInfo()), paste0("sessionInfo_", Sys.Date(), ".txt"))

library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(multcompView)
writeLines(capture.output(sessionInfo()), paste0("sessionInfo_", Sys.Date(), ".txt"))

# ==============================================================================
#  Boxplots for all six dimensions
# ==============================================================================
# - Compact letters (Wilcoxon signed-rank, BH-adjusted) are added only for
#   dimensions that have a post-hoc (i.e. a significant Friedman test).
# - A dimension without a post-hoc (e.g. D3) is drawn in the same style with
#   no letters; its caption states that the omnibus test was not significant.
# - A caption for every figure (with the Friedman result) is saved to a .txt file.

# --- 1. Files (edit names as needed) ------------------------------------------
scores_file    <- "dimension_scores_HumanAIproject_V2.csv"
crosswalk_file <- "item_crosswalk_HumanAIproject.csv"
friedman_file  <- "friedman_DIMENSIONS_HumanAIproject_V2.csv"
wilcoxon_file  <- "wilcoxon_posthoc_DIMENSIONS_HumanAIproject_V2.csv"
captions_file  <- "figure_captions_HumanAIproject_v2.txt"
incomplete_ids <- c("computerandlife_14", "vitro_28")

# Provisional labels for the new model: edit if you rename any dimension
dim_labels <- c(
  D1 = "D1: Informational Density vs. Stance Modality",
  D2 = "D2: Personal-Experiential Involvement",
  D3 = "D3: Narrative specificity vs possibility/evaluation",
  D4 = "D4: Communicative Attribution vs. Event Occurrence",
  D5 = "D5: Comparative/quantitative emphasis vs necessity",
  D6 = "D6: Collective Technical Stance "
)

# --- 2. Matched dataset --------------------------------------------------------
# Register labels in the scores file and the crosswalk must match each other
dim_scores  <- read_csv(scores_file)
parsed <- read_csv(crosswalk_file)

dim_scores_matched <- dim_scores %>%
  left_join(parsed %>% select(filename, Register, item_id),
            by = c("filename", "Register")) %>%
  filter(!item_id %in% incomplete_ids)

stopifnot(all(count(dim_scores_matched, item_id)$n == 3))
n_items <- n_distinct(dim_scores_matched$item_id)

# Register order on the x-axis: GPT, Humanised, Human (works whatever the exact labels are)
regs <- unique(as.character(dim_scores_matched$Register))
gpt <- regs[grepl("GPT", regs)]
hum <- regs[grepl("Humanised", regs)]
human <- setdiff(regs, c(gpt, hum))
reg_order <- c(gpt, hum, human)
stopifnot(length(reg_order) == 3)
dim_scores_matched$Register <- factor(dim_scores_matched$Register, levels = reg_order)

# --- 3. Test results -----------------------------------------------------------
friedman <- read.csv(friedman_file)
wilcoxon_all <- read.csv(wilcoxon_file)

fmt_p <- function(p) if (p < .001) "< .001" else paste0("= ", sub("^0", "", sprintf("%.3f", p)))
fmt_w <- function(w) sub("^0", "", sprintf("%.3f", w))

# --- 4. Long format ------------------------------------------------------------
dims <- c("D1", "D2", "D3", "D4", "D5", "D6")

dim_long <- dim_scores_matched %>%
  pivot_longer(cols = all_of(dims), names_to = "Dimension", values_to = "Score")

# --- 5. Plots and captions -----------------------------------------------------
captions <- character(0)

for (i in seq_along(dims)) {
  d <- dims[i]
  sub_data <- dim_long %>% filter(Dimension == d)
  fr <- friedman %>% filter(Dimension == d)

  # Letters only if the Friedman test was significant AND a post-hoc exists
  has_posthoc <- (d %in% wilcoxon_all$Dimension) && (fr$p_value_raw < 0.05)

  p <- ggplot(sub_data, aes(x = Register, y = Score, fill = Register)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    geom_point(position = position_jitter(width = 0.2, seed = 1),
               colour = "grey50", alpha = 0.4, size = 1) +
    scale_x_discrete(labels = c("LOCNESS_GPT" = "GPT-4",
                                "LOCNESS_Humanised" = "Humanized",
                                "LOCNESS_Human" = "Student")) +
    theme_minimal(base_size = 13) +
    labs(x = "Register", y = "Dimension Score", title = dim_labels[[d]]) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")

  if (has_posthoc) {
    wt <- wilcoxon_all %>% filter(Dimension == d)

    p_vec <- wt$p.adj
    names(p_vec) <- paste(wt$group1, wt$group2, sep = "-")

    letters_out <- multcompLetters(p_vec)$Letters
    letters_df <- data.frame(Register = names(letters_out),
                             Letter = letters_out,
                             stringsAsFactors = FALSE)

    # Place letters just above each register's highest point (safe for negative scores)
    score_range <- diff(range(sub_data$Score, na.rm = TRUE))
    y_top <- sub_data %>%
      mutate(Register = as.character(Register)) %>%
      group_by(Register) %>%
      summarise(y = max(Score, na.rm = TRUE) + 0.06 * score_range, .groups = "drop")

    letters_df <- inner_join(letters_df, y_top, by = "Register")

    p <- p + geom_text(data = letters_df, aes(x = Register, y = y, label = Letter),
                       inherit.aes = FALSE, size = 5, fontface = "bold")
  }

  ggsave(paste0("boxplot_v2_", d, ".png"), p, width = 8, height = 6, dpi = 300)

  # Caption draft
  stat_txt <- sprintf("Friedman test: \u03c7\u00b2(%d) = %.2f, p_adj %s, W = %s.",
                      fr$df, fr$chi_sq, fmt_p(fr$p_adj_raw), fmt_w(fr$kendalls_w))
  post_txt <- if (has_posthoc) {
    "Letters indicate pairwise differences (Wilcoxon signed-rank tests, Benjamini-Hochberg corrected); registers sharing a letter do not differ significantly."
  } else {
    "No post-hoc comparisons were run because the omnibus test was not significant, so no letters are shown."
  }
  short_label <- sub("^D[0-9]+: ", "", dim_labels[[d]])

  captions <- c(captions,
                sprintf("Figure %d. Dimension %d (%s) scores by register (n = %d matched triplets). %s %s",
                        i, i, short_label, n_items, stat_txt, post_txt),
                "")
}

# Save captions as UTF-8 text
con <- file(captions_file, open = "w", encoding = "UTF-8")
writeLines(captions, con)
close(con)

cat(captions, sep = "\n")
