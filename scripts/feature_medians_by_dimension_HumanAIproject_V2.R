writeLines(capture.output(sessionInfo()), paste0("sessionInfo_", Sys.Date(), ".txt"))

library(readr)
library(dplyr)
library(stringr)
library(tidyr)

# Medians of each dimension's features per condition, for the 144 complete triplets.
# Shows which features change from GPT-4 to humanized, in which direction on the
# dimension, and how far each one moves towards the student essays.
# Dimensions covered: D1, D2, D4, D5 (final six-factor model).

# --- 1. Load the feature matrix and the item crosswalk ---
features <- read_csv("EFA-readyHumanAIprojectRegistersADDED_V2.csv")
parsed   <- read_csv("item_crosswalk_HumanAIproject.csv")

# Same key cleaning as in the Friedman-Wilcoxon script
features <- features %>%
  mutate(filename = str_trim(filename), Register = str_trim(Register))
parsed <- parsed %>%
  mutate(filename = str_trim(filename), Register = str_trim(Register))

# --- 2. Keep only the 144 complete triplets used in the dimension analysis ---
incomplete_ids <- c("computerandlife_14", "vitro_28")

features_matched <- features %>%
  inner_join(parsed %>% select(filename, Register, item_id),
             by = c("filename", "Register")) %>%
  filter(!item_id %in% incomplete_ids)

item_counts <- features_matched %>% count(item_id)
cat("Matched", nrow(item_counts), "complete triplets\n")  # should be 144
# Stop if nothing (or not everything) matched. all() on an empty vector is TRUE,
# so the check on n alone would let a failed join through.
stopifnot(nrow(item_counts) == 144, all(item_counts$n == 3))

# --- 3. Features per dimension, with pole (+1 positive, -1 negative) ---
# Each feature is listed only on the dimension it was assigned to.
feature_map <- tribble(
  ~Dimension, ~Tag,        ~Feature,                                      ~Pole,
  # D1 Informational density vs stance modality
  "D1", "MDWO",        "Modal would",                                       1,
  "D1", "PASS",        "Passive voice",                                     1,
  "D1", "CUZ",         "Causal conjunctions",                               1,
  "D1", "COND",        "Conditional subordination",                         1,
  "D1", "RFACT",       "Factive adverbs",                                   1,
  "D1", "EX",          "Existential there",                                 1,
  "D1", "XX0",         "Negation",                                          1,
  "D1", "MDWS",        "Modals will/shall",                                 1,
  "D1", "SPLIT",       "Split auxiliaries and infinitives",                 1,
  "D1", "ABLE",        "Semi-modal be able to",                             1,
  "D1", "QUAN",        "Quantifiers",                                       1,
  "D1", "HDG",         "Hedges",                                            1,
  "D1", "RB",          "Adverbs",                                           1,
  "D1", "DWNT",        "Downtoners",                                        1,
  "D1", "DMA",         "Discourse markers",                                 1,
  "D1", "PIT",         "Pronoun it",                                        1,
  "D1", "LDE",         "Lexical density",                                  -1,
  "D1", "NNABSPROC",   "Abstract/process nouns",                           -1,
  "D1", "VBG",         "Present participle/gerund",                        -1,
  "D1", "CC",          "Coordinating conjunctions",                        -1,
  "D1", "JJAT",        "Attributive adjectives",                           -1,
  "D1", "TTR",         "Type-token ratio",                                 -1,
  "D1", "THRC",        "That-relative clauses",                            -1,
  "D1", "CAUSE",       "Facilitation and causative verbs",                 -1,
  "D1", "NCOMP",       "Noun compounds",                                   -1,
  # D2 Personal-experiential involvement
  "D2", "MENTAL",      "Mental verbs",                                      1,
  "D2", "PP2",         "2nd person pronouns",                               1,
  "D2", "NNHUMAN",     "Human-reference nouns",                             1,
  "D2", "PP1S",        "1st person singular pronouns",                      1,
  "D2", "ASPECT",      "Aspectual verbs",                                   1,
  "D2", "PLACE",       "Place adverbials",                                  1,
  "D2", "QUPR",        "Quantifying pronouns",                              1,
  "D2", "ToVDSR",      "To-clauses after verbs of desire",                  1,
  "D2", "THATD",       "That-deletion",                                     1,
  "D2", "DOAUX",       "Do as pro-verb/auxiliary",                          1,
  # D4 Communicative attribution vs event occurrence
  "D4", "ThVCOMM",     "Communication verb + that-clause",                  1,
  "D4", "THSC",        "That-clauses (non-relative)",                       1,
  "D4", "COMM",        "Communication verbs",                               1,
  "D4", "OCCUR",       "Occurrence verbs",                                 -1,
  # D5 Comparative/quantitative emphasis
  "D5", "EMPH",        "Emphatics",                                         1,
  "D5", "NNQUANT",     "Quantifying nouns",                                 1,
  "D5", "COMPAR",      "Comparatives",                                      1,
  "D5", "JJSIZE",      "Size adjectives",                                   1,
  "D5", "MDNE",        "Necessity modals",                                 -1
)

missing <- setdiff(feature_map$Tag, names(features_matched))
if (length(missing) > 0) stop("Columns not found in the feature matrix: ", paste(missing, collapse = ", "))

# --- 4. Medians per condition ---
medians <- features_matched %>%
  select(Register, all_of(unique(feature_map$Tag))) %>%
  pivot_longer(-Register, names_to = "Tag", values_to = "Frequency") %>%
  group_by(Tag, Register) %>%
  summarise(Median = median(Frequency, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Register, values_from = Median)

feature_medians <- feature_map %>%
  left_join(medians, by = "Tag") %>%
  mutate(
    Pole_label = ifelse(Pole == 1, "positive", "negative"),
    # Change in frequency from GPT-4 to humanized (positive = feature increased)
    GPT_to_Humanised = LOCNESS_Humanised - LOCNESS_GPT,
    # Direction of that change on the dimension: a rise in a negative-pole feature
    # moves the text towards the negative pole, so the sign is flipped for those
    Shift_on_dimension = case_when(
      abs(GPT_to_Humanised) < 1e-8          ~ "no change",
      sign(GPT_to_Humanised) * Pole > 0      ~ "towards positive pole",
      TRUE                                   ~ "towards negative pole"
    ),
    # Share of the GPT-4-to-student gap closed by humanization
    # (0 = no movement, 1 = reached student level, >1 = overshot, <0 = moved away).
    # NA when the GPT-4 and student medians are (nearly) equal.
    Gap_closed = ifelse(abs(LOCNESS_Human - LOCNESS_GPT) > 1e-8,
                        GPT_to_Humanised / (LOCNESS_Human - LOCNESS_GPT), NA)
  ) %>%
  select(Dimension, Tag, Feature, Pole_label, LOCNESS_GPT, LOCNESS_Humanised, LOCNESS_Human,
         GPT_to_Humanised, Shift_on_dimension, Gap_closed) %>%
  # Within each dimension, largest changes first
  arrange(Dimension, desc(abs(GPT_to_Humanised)))

# Print one block per dimension
for (d in unique(feature_medians$Dimension)) {
  cat("\n=====", d, "=====\n")
  print(feature_medians %>%
          filter(Dimension == d) %>%
          select(-Dimension) %>%
          mutate(across(where(is.numeric), ~ round(.x, 3))),
        n = Inf, width = Inf)
}

write.csv(feature_medians, "feature_medians_D1_D2_D4_D5_HumanAIproject_V2.csv", row.names = FALSE)

writeLines(capture.output(sessionInfo()), paste0("sessionInfo_end_", Sys.Date(), ".txt"))
