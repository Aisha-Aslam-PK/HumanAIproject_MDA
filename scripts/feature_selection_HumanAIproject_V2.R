library(readr)
library(tidyr)
library(caret)
library(psych)
library(dplyr)

# --- 1. Load the three separately-tagged corpora -----------------------------
# Register label added to each corpus as it is read (the MFTE output has no Register column)

writeLines(capture.output(sessionInfo()), paste0("C:/Users/miss_/OneDrive - The University of Queensland/PhD/side-project/sessionInfo_", Sys.Date(), ".txt"))

#combine three files-----------------
packageVersion("psych")

packageVersion("caret")
corpus_A <- read_csv("GPTcorpus_normed.csv")      %>% mutate(Register = "GPTcorpus")
corpus_B <- read_csv("Humaisedcorpus_normed.csv") %>% mutate(Register = "Humanisedcorpus")
corpus_C <- read_csv("Human_LOCNESS_normed.csv")  %>% mutate(Register = "Human_LOCNESS")

# sanity check columns match before binding
setdiff(names(corpus_A), names(corpus_B))
setdiff(names(corpus_A), names(corpus_C))


# Check what these tags actually are / how rare they are in A and C
summary(corpus_A[, c("PGET", "ThJATT", "ThJEVL")])
summary(corpus_C[, c("PGET", "ThJATT", "ThJEVL")])


combined_stats <- bind_rows(corpus_A, corpus_B, corpus_C)

# > setdiff(names(corpus_A), names(corpus_B))
# [1] "PGET"   "ThJATT" "ThJEVL"
# > setdiff(names(corpus_A), names(corpus_C))
# character(0)

setdiff(names(corpus_A), names(corpus_B))
setdiff(names(corpus_B), names(corpus_A))
setdiff(names(corpus_A), names(corpus_C))
setdiff(names(corpus_C), names(corpus_A))
setdiff(names(corpus_B), names(corpus_C))
setdiff(names(corpus_C), names(corpus_B))



# --- 2. Combine into one pooled dataset --------------------------------------
combined_stats <- bind_rows(corpus_A, corpus_B, corpus_C)

# --- 3. Remove composite / register-inappropriate feature tags --------------
features_to_remove_final <- c(
  "PP1", "PP3", "MDPOSSCall", "MDPREDall", "PASSall",
  "RSTNCall", "ThJSTNCall", "ThNSTNCall", "ThSTNCall", "ThVSTNCall",
  "ToJSTNCall", "ToSTNCall", "ToVSTNCall", "WhVSTNCall",
  "INother", "JJATother", "JJPRother", "NN", "NNother", "RBother",
  "THRCother", "THSCother", "WHSCother",
  "EMO", "HST", "URL", "NNMention",
  "Words", "PP1all", "PP3all"
)
combined_stats <- combined_stats[, !names(combined_stats) %in% features_to_remove_final]

# --- 4. Fill genuine zero-occurrence NAs (from mismatched columns) ----------
# NA here means "this feature never occurred in this corpus when tagged
# separately" — not missing data in the usual sense. Legitimate 0.
non_feature_cols <- c("Filename", "Register")  # first column + register label
feature_cols <- setdiff(names(combined_stats), non_feature_cols)

combined_stats <- combined_stats %>%
  mutate(across(all_of(feature_cols), ~ replace_na(.x, 0)))

# confirm no NAs remain among feature columns
stopifnot(sum(is.na(combined_stats[feature_cols])) == 0)

# --- 5. Split off numeric feature matrix for NZV/KMO pipeline ---------------
raw_data_final <- combined_stats  # keep 'filename' + 'Register' alongside for later grouping
num_df_final <- raw_data_final[, sapply(raw_data_final, is.numeric)]

# --- 6. Near-zero variance removal (log what's dropped) ---------------------
nzv_final <- nearZeroVar(num_df_final, saveMetrics = TRUE)
removed_nzv <- names(num_df_final)[nzv_final$nzv]
num_df_final <- num_df_final[, !nzv_final$nzv]

# --- 7. Iterative KMO/MSA-based feature removal ------------------------------
efa_data <- num_df_final
msa_threshold <- 0.60
removed_low_msa_log <- data.frame(Feature = character(), MSA_at_removal = numeric(), Step = integer())
step <- 0

repeat {
  kmo_iter <- KMO(efa_data)
  worst_msa <- min(kmo_iter$MSAi)
  if (worst_msa >= msa_threshold || length(kmo_iter$MSAi) <= 3) break
  
  worst_var <- names(which.min(kmo_iter$MSAi))
  step <- step + 1
  removed_low_msa_log <- rbind(
    removed_low_msa_log,
    data.frame(Feature = worst_var, MSA_at_removal = round(worst_msa, 4), Step = step)
  )
  efa_data <- efa_data[, !names(efa_data) %in% worst_var, drop = FALSE]
}

num_df_efa_ready_final <- efa_data
kmo_overall <- KMO(num_df_efa_ready_final)
kmo_overall$MSA

# --- 8. Save outputs ----------------------------------------------------------
write_csv(data.frame(removed_nzv = removed_nzv), "removed_nzv_HumanAIproject.csv")
write_csv(removed_low_msa_log, "removed_low_msa_HumanAIproject.csv")
write_csv(data.frame(surviving_cols_final = names(num_df_efa_ready_final)), "surviving_cols_final_HumanAIproject.csv")
write_csv(num_df_efa_ready_final, "EFA_ready_final_HumanAIproject.csv")
write_csv(data.frame(Overall_KMO = kmo_overall$MSA), "overall_KMO_HumanAIproject.csv")

# Keep filename + Register labels aligned with the EFA-ready rows for the
# later dimension-scoring + by-register comparison step
row_idx <- as.integer(rownames(num_df_efa_ready_final))
efa_ready_with_register <- cbind(
  filename = raw_data_final$Filename[row_idx],
  Register = raw_data_final$Register[row_idx],
  num_df_efa_ready_final
)
write_csv(efa_ready_with_register, "EFA-readyHumanAIprojectRegistersADDED_V2.csv")

writeLines(capture.output(sessionInfo()), paste0("C:/Users/miss_/OneDrive - The University of Queensland/PhD/side-project/sessionInfo_end_", Sys.Date(), ".txt"))
