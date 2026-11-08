library(readr)
library(readxl)

writeLines(capture.output(sessionInfo()), paste0("C:/Users/miss_/OneDrive - The University of Queensland/PhD/side-project/sessionInfo_", Sys.Date(), ".txt"))

# --- Load the EFA-ready pooled dataset ---------------------------------------
raw_normalized <- read.csv("EFA-readyHumanAIprojectRegistersADDED_V2.csv")

# --- Z-score all numeric feature columns (everything except Filename/Register) --
linguistic_vars <- names(raw_normalized)[!names(raw_normalized) %in% c("filename", "Register")]

efa_scaled <- as.data.frame(scale(raw_normalized[, linguistic_vars]))
efa_scaled$filename <- raw_normalized$filename
efa_scaled$Register <- raw_normalized$Register

# reorder so Filename/Register lead, matching the original file's layout
efa_scaled <- efa_scaled[, c("filename", "Register", linguistic_vars)]

# --- Save output ---------------------------------------------------------------
write_csv(efa_scaled, "EFA_zscored_HumanAIproject_V2.csv")


# Define factor loadings for each dimension (forced-4, honest re-extraction)------------------
# D1: Informational Density vs. Stance Modality
F1_pos <- c("MDWO", "PASS", "CUZ", "COND", "RFACT", "EX", "XX0", "MDWS", "SPLIT",
            "ABLE", "QUAN", "HDG", "RB", "DWNT", "DMA", "PIT")
F1_neg <- c("LDE", "NNABSPROC", "VBG", "CC", "JJAT", "TTR", "THRC", "CAUSE", "NCOMP")
# D2: Personal-Experiential Involvement
F2_pos <- c("MENTAL", "PP2", "NNHUMAN", "PP1S", "ASPECT", "PLACE", "QUPR",
            "ToVDSR", "THATD", "DOAUX")
F2_neg <- c()
# D3: Narrative Specificity vs. Possibility/Evaluation
F3_pos <- c("MDCA", "ELAB","JJPR")
F3_neg <- c("NNP", "VBD", "CD")
# D4: Communicative Attribution vs. Event Occurrence
F4_pos <- c("ThVCOMM", "COMM", "THSC")
F4_neg <- c("OCCUR")
#D5: Comparative/Quantitative Emphasis
F5_pos <- c("EMPH", "NNQUANT", "COMPAR", "JJSIZE")
F5_neg <- c("MDNE")
#D6: Collective Technical Stance
F6_pos <- c("PP1P", "NNTECH", "JJEPSTother", "MDCO")
F6_neg <- c()


# Calculate dimension scores (z-scores already applied)---------------
efa_scaled$D1 <- rowSums(efa_scaled[, F1_pos]) - rowSums(efa_scaled[, F1_neg, drop = FALSE])
efa_scaled$D2 <- rowSums(efa_scaled[, F2_pos]) - rowSums(efa_scaled[, F2_neg, drop = FALSE])
efa_scaled$D3 <- rowSums(efa_scaled[, F3_pos]) - rowSums(efa_scaled[, F3_neg, drop = FALSE])
efa_scaled$D4 <- rowSums(efa_scaled[, F4_pos]) - rowSums(efa_scaled[, F4_neg, drop = FALSE])
efa_scaled$D5 <- rowSums(efa_scaled[, F5_pos]) - rowSums(efa_scaled[, F5_neg, drop = FALSE])
efa_scaled$D6 <- rowSums(efa_scaled[, F6_pos]) - rowSums(efa_scaled[, F6_neg, drop = FALSE])
write_csv(efa_scaled, "dimension_scores_HumanAIproject_V2.csv")

# Run Shapiro test to figure out possibility of parametric tests---------------

shapiro_w_test <- read_csv("dimension_scores_HumanAIproject_V2.csv")

shapiro.test(shapiro_w_test$D1)
hist(shapiro_w_test$D1)
shapiro.test(shapiro_w_test$D2)
hist(shapiro_w_test$D2)
shapiro.test(shapiro_w_test$D3)
hist(shapiro_w_test$D3)
shapiro.test(shapiro_w_test$D4)
hist(shapiro_w_test$D4)
shapiro.test(shapiro_w_test$D5)
hist(shapiro_w_test$D5)
shapiro.test(shapiro_w_test$D6)
hist(shapiro_w_test$D6)

# run the loop instead to save all the results at once--------
# List of your 6 variables
variables <- c("D1", "D2", "D3", "D4", "D5", "D6")
results_list <- list()

# Loop through each variable to run the test and save its summary
for (var in variables) {
  # Dynamically run the test on the column
  test_res <- shapiro.test(shapiro_w_test[[var]])
  
  # Store both raw and publication-ready values in a data frame
  results_list[[var]] <- data.frame(
    Variable = var,
    # Raw values
    Raw_Statistic_W = test_res$statistic,
    Raw_P_Value = test_res$p.value,
    
    # Publication-ready formatted values
    Pub_Statistic_W = round(test_res$statistic, 3),
    Pub_P_Value = format.pval(test_res$p.value, digits = 3, eps = 0.001),
    
    Method = test_res$method
  )
}

# Combine all individual data frames into one table
final_results <- do.call(rbind, results_list)

# Export the combined table to a CSV file
write.csv(final_results, file = "all_shapiro_results_HumanAIproject-6D_V2.csv", row.names = FALSE)
