writeLines(capture.output(sessionInfo()), paste0("sessionInfo_", Sys.Date(), ".txt"))
# install.packages("readr")
# install.packages("dplyr")
# install.packages("stringr")
# install.packages("tidyr")
# install.packages("rstatix")
# install.packages("coin")
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(rstatix)
library(coin)

# 1. Load the raw files fresh
dim_scores <- read_csv("dimension_scores_HumanAIproject_V2.csv")
parsed <- read_csv("item_crosswalk_HumanAIproject.csv")

# 2. Clean text keys to prevent whitespace or case mismatches
dim_scores_clean <- dim_scores %>%
  mutate(
    filename = str_trim(filename),
    Register = str_trim(Register)
  )

parsed_clean <- parsed %>%
  mutate(
    filename = str_trim(filename),
    Register = str_trim(Register)
  )

# 3. Join the data
dim_scores_matched <- dim_scores_clean %>%
  left_join(
    parsed_clean %>% select(filename, Register, item_id),
    by = c("filename", "Register")
  ) %>%
  filter(!is.na(item_id)) # Drop rows that failed to find an item_id match

# 4. Drop incomplete items (no full triplet)
incomplete_ids <- c("computerandlife_14", "vitro_28")
dim_scores_matched <- dim_scores_matched %>%
  filter(!item_id %in% incomplete_ids)

# 5. Verify row counts per item_id (should all be exactly 3)
item_counts <- dim_scores_matched %>% count(item_id)
stopifnot(all(item_counts$n == 3))
print(paste("Successfully validated", nrow(item_counts), "complete items!"))

# 5b. Sort so rows line up item-by-item across registers.
# rstatix::wilcox_test(paired = TRUE) pairs observations by row position within
# each register, not by item_id, so the row order must be identical across registers.
dim_scores_matched <- dim_scores_matched %>% arrange(Register, item_id)
stopifnot(length(unique(split(dim_scores_matched$item_id, dim_scores_matched$Register))) == 1)

# NEW: significance stars using the conventional strict cut-offs (p < .05, < .01, < .001).
# rstatix's p.adj.signif uses p <= cut-off and an extra **** level, so a value of
# exactly .010 would get ** there but * here.
conventional_stars <- function(p) {
  case_when(
    p < .001 ~ "***",
    p < .01  ~ "**",
    p < .05  ~ "*",
    TRUE     ~ "ns"
  )
}

# --- 6. Run Friedman & Wilcoxon Analysis Loop ---
dims <- c("D1", "D2", "D3", "D4", "D5", "D6")
friedman_results <- data.frame()
wilcoxon_results_list <- list()

expected_registers <- c("LOCNESS_GPT", "LOCNESS_Humanised", "LOCNESS_Human")

for (d in dims) {

  wide_d <- dim_scores_matched %>%
    select(item_id, Register, all_of(d)) %>%
    pivot_wider(names_from = Register, values_from = all_of(d))

  # Ensure the matrix contains the exact expected registers
  friedman_mat <- as.matrix(wide_d %>% select(all_of(expected_registers)))

  ft <- friedman.test(friedman_mat)

  n_items <- nrow(friedman_mat)
  k_conditions <- ncol(friedman_mat)
  kendalls_w <- ft$statistic / (n_items * (k_conditions - 1))

  friedman_results <- rbind(friedman_results, data.frame(
    Dimension = d,
    chi_sq = round(ft$statistic, 2),
    df = ft$parameter,
    p_value_raw = ft$p.value,
    kendalls_w = round(kendalls_w, 3)
  ))

  if (ft$p.value < 0.05) {
    long_d <- dim_scores_matched %>%
      select(item_id, Register, all_of(d)) %>%
      rename(Score = all_of(d))

    wt <- long_d %>%
      rstatix::wilcox_test(Score ~ Register, paired = TRUE, p.adjust.method = "BH")

    # Cross-check each pairwise statistic against base R on item-aligned
    # columns (wide_d is aligned by item_id via pivot_wider). Stops if they differ.
    # EDIT: also keep base R's unrounded p value from the same test, because
    # rstatix rounds p and p.adj in its output.
    aligned <- t(mapply(
      function(g1, g2) {
        w <- wilcox.test(wide_d[[g1]], wide_d[[g2]], paired = TRUE)
        c(V = unname(w$statistic), p = w$p.value)
      },
      wt$group1, wt$group2
    ))
    stopifnot(isTRUE(all.equal(as.numeric(wt$statistic), unname(aligned[, "V"]))))
    cat("Dimension", d, ": rstatix statistics match the item-aligned check\n")

    # EDIT: exact p values, BH adjustment across the three contrasts in this
    # dimension, and conventional stars based on the exact adjusted p
    wt$p_exact            <- unname(aligned[, "p"])
    wt$p_adj_exact        <- p.adjust(wt$p_exact, method = "BH")
    wt$signif_conventional <- conventional_stars(wt$p_adj_exact)

    wt$Dimension <- d
    wilcoxon_results_list[[d]] <- wt
  }
}

# Adjust p-values and save outputs
friedman_results$p_adj_raw <- p.adjust(friedman_results$p_value_raw, method = "BH")
print(friedman_results)

write.csv(friedman_results, "friedman_DIMENSIONS_HumanAIproject_V2.csv", row.names = FALSE)
if (length(wilcoxon_results_list) > 0) {
  wilcoxon_results_all <- bind_rows(wilcoxon_results_list)

  # EDIT: print the exact adjusted p values to 6 significant digits for checking
  print(
    wilcoxon_results_all %>%
      transmute(Dimension, group1, group2, statistic,
                p_adj_exact = format(p_adj_exact, digits = 6),
                p.adj.signif, signif_conventional),
    n = Inf
  )

  write.csv(wilcoxon_results_all, "wilcoxon_posthoc_DIMENSIONS_HumanAIproject_V2.csv", row.names = FALSE)
}

dim_scores_matched %>% group_by(Register) %>% summarise(median = median(D3), IQR = IQR(D3))

dim_scores_matched %>% group_by(Register) %>% summarise(median = median(D4), IQR = IQR(D4))

dim_scores_matched %>% group_by(Register) %>% summarise(min = min(D5), max = max(D5), IQR = IQR(D5))


# --- 7. Pairwise effect sizes (r = Z / sqrt(n), n = number of pairs) for dimensions with a post-hoc ---
# wilcox_effsize(paired = TRUE) also pairs by row position within each register,
# so it relies on dim_scores_matched still being sorted as in step 5b.
# Do not re-sort or filter long_d before this point.
effsize_list <- list()

for (d in names(wilcoxon_results_list)) {
  long_d <- dim_scores_matched %>%
    select(item_id, Register, all_of(d)) %>%
    rename(Score = all_of(d))

  eff <- long_d %>%
    rstatix::wilcox_effsize(Score ~ Register, paired = TRUE)

  eff$Dimension <- d
  effsize_list[[d]] <- eff
}

effsize_all <- bind_rows(effsize_list)
print(effsize_all)
write.csv(effsize_all, "wilcoxon_effsizes_DIMENSIONS_HumanAIproject_V2.csv", row.names = FALSE)

# --- 8. Combined Wilcoxon table (manuscript Table 4) ---
# Joins V, exact adjusted p, conventional stars and r (with its magnitude label)
# by dimension and contrast. Stops if any contrast fails to match.
table4_data <- wilcoxon_results_all %>%
  select(Dimension, group1, group2, n1, n2, statistic,
         p_exact, p_adj_exact, signif_conventional) %>%
  left_join(
    effsize_all %>% select(Dimension, group1, group2, effsize, magnitude),
    by = c("Dimension", "group1", "group2")
  ) %>%
  rename(V = statistic, r = effsize)

stopifnot(nrow(table4_data) == nrow(wilcoxon_results_all), !any(is.na(table4_data$r)))

# --- 8b. NEW: direction of the differences ---
# Condition medians/IQRs, and for each contrast the median within-triplet difference
# (group1 - group2, the same order as rstatix and V) plus how many triplets go each way.
# wide data are aligned by item_id via pivot_wider, so the differences are paired.
medians_all <- dim_scores_matched %>%
  pivot_longer(all_of(dims), names_to = "Dimension", values_to = "Score") %>%
  group_by(Dimension, Register) %>%
  summarise(Median = median(Score), IQR = IQR(Score), .groups = "drop")

cat("\n=== Condition medians and IQRs ===\n")
print(medians_all %>% mutate(across(c(Median, IQR), ~ round(.x, 2))), n = Inf)

contrasts <- tibble(
  group1 = c("LOCNESS_GPT",   "LOCNESS_GPT",       "LOCNESS_Human"),
  group2 = c("LOCNESS_Human", "LOCNESS_Humanised", "LOCNESS_Humanised")
)

paired_diffs <- bind_rows(lapply(dims, function(d) {
  wide_d <- dim_scores_matched %>%
    select(item_id, Register, all_of(d)) %>%
    pivot_wider(names_from = Register, values_from = all_of(d))
  bind_rows(lapply(seq_len(nrow(contrasts)), function(i) {
    diff <- wide_d[[contrasts$group1[i]]] - wide_d[[contrasts$group2[i]]]
    tibble(Dimension = d,
           group1 = contrasts$group1[i], group2 = contrasts$group2[i],
           median_diff     = median(diff),
           n_group1_higher = sum(diff > 0),
           n_group2_higher = sum(diff < 0),
           n_tied          = sum(diff == 0))
  }))
}))

cat("\n=== Median paired differences (group1 - group2) and triplets going each way ===\n")
print(paired_diffs %>% mutate(median_diff = round(median_diff, 2)), n = Inf, width = Inf)

# Add the direction columns to the combined Wilcoxon table
table4_data <- table4_data %>%
  left_join(paired_diffs, by = c("Dimension", "group1", "group2"))
stopifnot(!any(is.na(table4_data$median_diff)))

# Consistency check: V above its midpoint should normally go with a positive median
# difference. A mismatch is possible (the two summarise the differences differently),
# so it is flagged, not stopped.
table4_data <- table4_data %>%
  mutate(V_midpoint = n1 * (n1 + 1) / 4,
         direction_agrees = sign(V - V_midpoint) == sign(median_diff))
if (any(!table4_data$direction_agrees)) {
  cat("\nNOTE: V and the median difference point in different directions for:\n")
  print(table4_data %>% filter(!direction_agrees) %>%
          select(Dimension, group1, group2, V, V_midpoint, median_diff))
}

print(table4_data, n = Inf, width = Inf)
write.csv(table4_data, "wilcoxon_combined_table4_HumanAIproject_V2.csv", row.names = FALSE)
write.csv(medians_all, "condition_medians_HumanAIproject_V2.csv", row.names = FALSE)

# --- 9. Build Table 4 (one row per dimension) ---
dim_labels <- c(
  D1 = "D1 Informational density vs stance modality",
  D2 = "D2 Personal–experiential involvement",
  D3 = "D3 Narrative specificity vs possibility/evaluation",
  D4 = "D4 Communicative attribution vs event occurrence",
  D5 = "D5 Comparative/quantitative emphasis vs necessity",
  D6 = "D6 Collective technical stance"
)

# .xx without leading zero, plus stars (or " ns")
fmt_r <- function(r, s) paste0(sub("^0", "", sprintf("%.2f", r)), ifelse(s == "ns", " ns", s))
# Medians to two decimals with a typographic minus sign
fmt_med <- function(x) sub("^-", "−", sprintf("%.2f", x))

contrast_cols <- table4_data %>%
  mutate(
    contrast = case_when(
      group1 == "LOCNESS_GPT"   & group2 == "LOCNESS_Human"     ~ "GPT-4 vs student, r",
      group1 == "LOCNESS_GPT"   & group2 == "LOCNESS_Humanised" ~ "GPT-4 vs humanized, r",
      group1 == "LOCNESS_Human" & group2 == "LOCNESS_Humanised" ~ "Humanized vs student, r"
    ),
    value = fmt_r(r, signif_conventional)
  ) %>%
  select(Dimension, contrast, value) %>%
  pivot_wider(names_from = contrast, values_from = value)

median_cols <- medians_all %>%
  mutate(col = recode(Register,
                      LOCNESS_GPT       = "Mdn GPT-4",
                      LOCNESS_Humanised = "Mdn humanized",
                      LOCNESS_Human     = "Mdn student"),
         value = fmt_med(Median)) %>%
  select(Dimension, col, value) %>%
  pivot_wider(names_from = col, values_from = value)

table4 <- friedman_results %>%
  mutate(
    # Non-significant Friedman tests show the exact p instead of "ns"
    chi = ifelse(p_value_raw < .05,
                 sprintf("%.2f", chi_sq),
                 paste0(sprintf("%.2f", chi_sq), " (p = ", sub("^0", "", sprintf("%.3f", p_value_raw)), ")")),
    W   = sub("^0", "", sprintf("%.2f", kendalls_w))
  ) %>%
  select(Dimension, chi, W) %>%
  left_join(median_cols, by = "Dimension") %>%
  left_join(contrast_cols, by = "Dimension") %>%
  mutate(
    across(ends_with(", r"), ~ replace_na(.x, "–")),  # en dash where no post-hoc (D3)
    Dimension = unname(dim_labels[Dimension])
  ) %>%
  select(Dimension, chi, W, `Mdn GPT-4`, `Mdn humanized`, `Mdn student`,
         `GPT-4 vs student, r`, `GPT-4 vs humanized, r`, `Humanized vs student, r`)
names(table4)[names(table4) == "chi"] <- "Friedman χ²(2)"

print(as_tibble(table4), width = Inf)
write_excel_csv(table4, "Table4_HumanAIproject_V2.csv")  # opens in Excel with χ², minus signs and dashes intact

# Closing sessionInfo moved here so packages loaded in steps 7-8 (e.g., coin) are captured
writeLines(capture.output(sessionInfo()), paste0("sessionInfo_end_", Sys.Date(), ".txt"))
