writeLines(capture.output(sessionInfo()), paste0("sessionInfo_", Sys.Date(), ".txt"))

library(readr)
library(dplyr)
library(stringr)
library(tidyr)

# Finds the most representative matched triplet for each dimension with a
# significant Friedman test (D1, D2, D4, D5, D6), using a median-difference rule:
#
#   For each triplet, take the three within-item differences
#     GPT - Student, GPT - Humanised, Student - Humanised
#   and compare them with the median of each difference across all 144 triplets.
#   Each difference is scaled by its SD across triplets, so the three count equally.
#   The triplet with the smallest distance from the medians is the most typical
#   example of the condition contrast on that dimension.
#
# It also lists triplets that are representative on several dimensions at once.

# --- 1. Load and match, exactly as in the Friedman-Wilcoxon script ---
dim_scores <- read_csv("dimension_scores_HumanAIproject_V2.csv")
parsed     <- read_csv("item_crosswalk_HumanAIproject.csv")

dim_scores <- dim_scores %>%
  mutate(filename = str_trim(filename), Register = str_trim(Register))
parsed <- parsed %>%
  mutate(filename = str_trim(filename), Register = str_trim(Register))

incomplete_ids <- c("computerandlife_14", "vitro_28")

matched <- dim_scores %>%
  left_join(parsed %>% select(filename, Register, item_id),
            by = c("filename", "Register")) %>%
  filter(!is.na(item_id), !item_id %in% incomplete_ids)

item_counts <- matched %>% count(item_id)
cat("Matched", nrow(item_counts), "complete triplets\n")
stopifnot(nrow(item_counts) == 144, all(item_counts$n == 3))

# File names of each triplet, so the texts can be found
triplet_files <- matched %>%
  select(item_id, Register, filename) %>%
  pivot_wider(names_from = Register, values_from = filename, names_prefix = "file_")

# --- 2. Distance from the median differences, per dimension ---
dims <- c("D1", "D2", "D4", "D5", "D6")
top_n_show <- 5   # candidates listed per dimension

results_list <- list()

for (d in dims) {
  wide <- matched %>%
    select(item_id, Register, all_of(d)) %>%
    pivot_wider(names_from = Register, values_from = all_of(d)) %>%
    mutate(
      diff_GPT_Student       = LOCNESS_GPT   - LOCNESS_Human,
      diff_GPT_Humanised     = LOCNESS_GPT   - LOCNESS_Humanised,
      diff_Student_Humanised = LOCNESS_Human - LOCNESS_Humanised
    )

  med <- sapply(wide[, c("diff_GPT_Student", "diff_GPT_Humanised", "diff_Student_Humanised")], median)
  sds <- sapply(wide[, c("diff_GPT_Student", "diff_GPT_Humanised", "diff_Student_Humanised")], sd)

  wide <- wide %>%
    mutate(
      distance = sqrt(
        ((diff_GPT_Student       - med["diff_GPT_Student"])       / sds["diff_GPT_Student"])^2 +
        ((diff_GPT_Humanised     - med["diff_GPT_Humanised"])     / sds["diff_GPT_Humanised"])^2 +
        ((diff_Student_Humanised - med["diff_Student_Humanised"]) / sds["diff_Student_Humanised"])^2
      ),
      rank = rank(distance, ties.method = "first"),
      Dimension = d
    )

  cat("\n=====", d, "=====\n")
  cat("Median differences: GPT-Student =", round(med["diff_GPT_Student"], 2),
      "| GPT-Humanised =", round(med["diff_GPT_Humanised"], 2),
      "| Student-Humanised =", round(med["diff_Student_Humanised"], 2), "\n")
  print(
    wide %>%
      arrange(rank) %>%
      slice_head(n = top_n_show) %>%
      select(rank, item_id, LOCNESS_GPT, LOCNESS_Humanised, LOCNESS_Human,
             diff_GPT_Student, diff_GPT_Humanised, diff_Student_Humanised, distance) %>%
      mutate(across(where(is.numeric) & !rank, ~ round(.x, 2))),
    width = Inf
  )

  results_list[[d]] <- wide %>%
    select(Dimension, item_id, rank, distance, LOCNESS_GPT, LOCNESS_Humanised, LOCNESS_Human,
           diff_GPT_Student, diff_GPT_Humanised, diff_Student_Humanised)
}

results_all <- bind_rows(results_list) %>%
  left_join(triplet_files, by = "item_id") %>%
  arrange(Dimension, rank)

write.csv(results_all, "representative_triplets_by_dimension_HumanAIproject_V2.csv", row.names = FALSE)

# --- 3. Triplets representative on several dimensions ---
# "Representative" = within the closest 10% of triplets on that dimension (rank <= 14 of 144)
top_cut <- ceiling(0.10 * 144)

multi_dim <- results_all %>%
  group_by(item_id) %>%
  summarise(
    n_dims_top10   = sum(rank <= top_cut),
    dims_top10     = paste(Dimension[rank <= top_cut], collapse = ", "),
    mean_rank      = mean(rank),
    .groups = "drop"
  ) %>%
  filter(n_dims_top10 >= 2) %>%
  arrange(desc(n_dims_top10), mean_rank) %>%
  left_join(triplet_files, by = "item_id")

cat("\n===== Triplets in the closest 10% on two or more dimensions =====\n")
if (nrow(multi_dim) == 0) {
  cat("None: use a separate triplet for each dimension.\n")
} else {
  print(multi_dim, n = Inf, width = Inf)
}

write.csv(multi_dim, "representative_triplets_multidimension_HumanAIproject_V2.csv", row.names = FALSE)

writeLines(capture.output(sessionInfo()), paste0("sessionInfo_end_", Sys.Date(), ".txt"))
