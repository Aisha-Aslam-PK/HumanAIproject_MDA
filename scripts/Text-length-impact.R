writeLines(capture.output(sessionInfo()), paste0("sessionInfo_", Sys.Date(), ".txt"))

library(readr)
library(dplyr)
library(stringr)
library(tidyr)

# Does the GPT-4 -> humanized change in text length explain the change in dimension scores?
#
# For each of the 144 matched triplets:
#   length change = log(words_humanized / words_GPT)   (negative = humanized text is shorter)
#   score change  = humanized score - GPT-4 score, on each dimension (D1-D6)
# Spearman correlations between the two, BH-adjusted across the six dimensions.
#
# Reading the results: a NEGATIVE rho means the more a text was shortened, the more its
# score rose on that dimension (as in Europe3 on D5). A rho near zero means length change
# does not account for the score change on that dimension.
# The log ratio is used rather than the raw word difference because a 50-word cut matters
# more in a 300-word text than in a 600-word text.

# --- 0. Settings ---------------------------------------------------------------
# Word counts come from the MFTE statistics folder of each corpus. Give the full path to
# one counts table per corpus (any MFTE table with a filename column and a Words column),
# labelled with the Register names used in the dimension-scores file.
wc_files <- c(
  LOCNESS_GPT       = 'GPTcorpus_counts_raw.csv',
  LOCNESS_Humanised = 'Humanisedcorpus_counts_raw.csv',
  LOCNESS_Human     = 'HumanLOCNESS_counts_raw.csv'
)
wc_col  <- NULL   # NULL = detect automatically; otherwise give the column name, e.g. "Words"
run_date <- Sys.Date()

# --- 1. Load and match, as in the other scripts ---------------------------------
dim_scores <- read_csv("dimension_scores_HumanAIproject_V2.csv")
parsed     <- read_csv("item_crosswalk_HumanAIproject.csv")
clean_keys <- function(d) {
  names(d)[tolower(names(d)) == "filename"] <- "filename"
  d %>% mutate(filename = str_trim(filename), Register = str_trim(Register))
}
dim_scores <- clean_keys(dim_scores)
parsed     <- clean_keys(parsed)

# File names can differ in folder or extension between files (e.g. "Europe3.txt" vs
# "Europe3"), so word counts are joined on the bare name without path or extension
bare_name <- function(x) str_remove(basename(str_trim(x)), "\\.[A-Za-z0-9]+$")

# Read the three counts tables, keep file name + word count, and stack them
read_wc <- function(path, register) {
  d <- read_delim(path, delim = ifelse(grepl("\\.tsv$|\\.txt$", path), "\t", ","),
                  show_col_types = FALSE)
  fn_col <- names(d)[tolower(names(d)) == "filename"][1]
  if (is.na(fn_col)) stop("No filename column in ", path)
  col <- wc_col
  if (is.null(col)) {
    candidates <- c("Words", "words", "Word_count", "word_count", "WordCount", "nwords", "Tokens", "tokens")
    col <- intersect(candidates, names(d))[1]
    if (is.na(col)) stop("No word-count column in ", path, ". Columns are: ",
                         paste(names(d), collapse = ", "))
  }
  cat(register, ": ", nrow(d), " texts, word counts from column '", col, "'\n", sep = "")
  tibble(key = bare_name(d[[fn_col]]), Register = register, Words = d[[col]])
}

wc_data <- bind_rows(lapply(names(wc_files), function(r) read_wc(wc_files[[r]], r))) %>%
  distinct()
stopifnot(!anyDuplicated(wc_data[, c("key", "Register")]))

# The Register names must match those in the dimension-scores file
if (!all(names(wc_files) %in% unique(dim_scores$Register))) {
  stop("Register labels in wc_files do not match the dimension-scores file, which has: ",
       paste(unique(dim_scores$Register), collapse = ", "))
}

incomplete_ids <- c("computerandlife_14", "vitro_28")
dims <- paste0("D", 1:6)

matched <- dim_scores %>%
  select(filename, Register, all_of(dims)) %>%
  left_join(parsed %>% select(filename, Register, item_id), by = c("filename", "Register")) %>%
  mutate(key = bare_name(filename)) %>%
  left_join(wc_data, by = c("key", "Register")) %>%
  select(-key) %>%
  filter(!is.na(item_id), !item_id %in% incomplete_ids)

item_counts <- matched %>% count(item_id)
cat("Matched", nrow(item_counts), "complete triplets\n")
stopifnot(nrow(item_counts) == 144, all(item_counts$n == 3))
if (any(is.na(matched$Words))) {
  stop("Missing word counts for: ",
       paste(head(matched$filename[is.na(matched$Words)], 10), collapse = ", "))
}

# --- 2. Text length per condition -----------------------------------------------
cat("\n=== Word counts per condition ===\n")
print(matched %>%
        group_by(Register) %>%
        summarise(Mean = round(mean(Words), 1), Median = median(Words),
                  Min = min(Words), Max = max(Words), .groups = "drop"))

# --- 3. Within-triplet changes, GPT-4 -> humanized ------------------------------
wide <- matched %>%
  select(item_id, Register, Words, all_of(dims)) %>%
  pivot_wider(names_from = Register, values_from = c(Words, all_of(dims)))

wide <- wide %>%
  mutate(len_logratio = log(Words_LOCNESS_Humanised / Words_LOCNESS_GPT),
         len_pct      = 100 * (Words_LOCNESS_Humanised / Words_LOCNESS_GPT - 1))

cat("\nHumanized vs GPT-4 length change (%): median =", round(median(wide$len_pct), 1),
    "| IQR =", paste(round(quantile(wide$len_pct, c(.25, .75)), 1), collapse = " to "),
    "| shorter in", sum(wide$len_pct < 0), "of", nrow(wide), "triplets\n")

# --- 4. Correlations: length change vs score change -----------------------------
cor_tab <- bind_rows(lapply(dims, function(d) {
  score_change <- wide[[paste0(d, "_LOCNESS_Humanised")]] - wide[[paste0(d, "_LOCNESS_GPT")]]
  ct <- suppressWarnings(cor.test(wide$len_logratio, score_change,
                                  method = "spearman", exact = FALSE))
  tibble(Dimension           = d,
         Median_score_change = round(median(score_change), 2),
         rho                 = round(unname(ct$estimate), 3),
         p                   = ct$p.value)
})) %>%
  mutate(p_adj = p.adjust(p, method = "BH"),
         signif = case_when(p_adj < .001 ~ "***", p_adj < .01 ~ "**",
                            p_adj < .05 ~ "*", TRUE ~ ""),
         p = signif(p, 3), p_adj = signif(p_adj, 3))

cat("\n=== Spearman correlations: log length ratio (humanized/GPT-4) vs score change ===\n")
cat("(negative rho = shorter humanized texts show larger score increases)\n")
print(cor_tab, width = Inf)

# --- 5. Output -------------------------------------------------------------------
write_csv(cor_tab, paste0("length_effect_correlations_HumanAIproject_", run_date, ".csv"))
write_csv(wide %>% select(item_id, starts_with("Words_"), len_pct, len_logratio, everything()),
          paste0("length_effect_triplet_data_HumanAIproject_", run_date, ".csv"))

writeLines(capture.output(sessionInfo()), paste0("sessionInfo_end_", Sys.Date(), ".txt"))