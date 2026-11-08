writeLines(capture.output(sessionInfo()), paste0("C:/Users/miss_/OneDrive - The University of Queensland/PhD/side-project/sessionInfo_", Sys.Date(), ".txt"))

# RQ3: does humanised text keep the feature co-occurrence structure of GPT text?
# Uses the 144 matched triplets. For each register a feature-by-feature correlation
# matrix is computed; matrices are compared via the correlation of their upper triangles.
# Uncertainty: bootstrap of the DIFFERENCES between matrix correlations, resampling whole
# triplets (keeps GPT/Humanised/Student texts from the same item together), plus a
# split-half baseline within each register and a Mantel permutation test as a minor check.
#
# EDITS in this version:
#  - Runs the whole analysis twice: Pearson (main) and Spearman (robustness check).
#  - Split-half baseline now also reports the Spearman-Brown corrected value, which
#    estimates the similarity expected for two full-size (n = 144) matrices.
#  - Output files carry the run date, so earlier results are not overwritten.
#  - sessionInfo written at the very top and again at the end.

library(readr)
library(dplyr)
library(vegan)   # install.packages("vegan") if needed
packageVersion("vegan")

set.seed(2026)
B <- 5000        # bootstrap / split-half replicates (lower for a faster run)
run_date <- Sys.Date()

# --- 1. Load data and crosswalk -------------------------------------------------
df        <- read_csv("EFA-readyHumanAIprojectRegistersADDED_V2.csv")
crosswalk <- read_csv("item_crosswalk_HumanAIproject.csv")

# make the filename column name consistent in both files
names(df)[tolower(names(df)) == "filename"]               <- "filename"
names(crosswalk)[tolower(names(crosswalk)) == "filename"] <- "filename"
stopifnot("filename" %in% names(df), "filename" %in% names(crosswalk),
          "Register" %in% names(df), "Register" %in% names(crosswalk),
          "item_id"  %in% names(crosswalk))

# --- 2. Rename registers to match the Friedman/Wilcoxon scripts -----------------
recode_reg <- function(x) {
  key <- tolower(gsub("[^A-Za-z]", "", x))
  case_when(
    key %in% c("gptcorpus", "locnessgpt")             ~ "LOCNESS_GPT",
    key %in% c("humanisedcorpus", "locnesshumanised") ~ "LOCNESS_Humanised",
    key %in% c("humanlocness", "locnesshuman")        ~ "LOCNESS_Human",
    TRUE                                              ~ NA_character_
  )
}

cat("Register values found in the data file:\n")
print(table(df$Register, useNA = "ifany"))

df$Register        <- recode_reg(df$Register)
crosswalk$Register <- recode_reg(crosswalk$Register)
if (any(is.na(df$Register)))        stop("Unmatched Register values in the data file.")
if (any(is.na(crosswalk$Register))) stop("Unmatched Register values in the crosswalk.")

# --- 3. Attach item_id and keep the 144 complete triplets -----------------------
crosswalk <- crosswalk %>% select(filename, Register, item_id) %>% distinct()
n_before  <- nrow(df)
df <- df %>% left_join(crosswalk, by = c("filename", "Register"))
stopifnot(nrow(df) == n_before)   # a join that duplicated rows would break this

if (any(is.na(df$item_id))) {
  stop("No item_id for: ", paste(head(df$filename[is.na(df$item_id)], 10), collapse = ", "))
}

incomplete_ids <- c("computerandlife_14", "vitro_28")
df <- df %>% filter(!item_id %in% incomplete_ids)

stopifnot(all(table(df$item_id) == 3), n_distinct(df$item_id) == 144)
cat("\nMatched triplets:", n_distinct(df$item_id), "\n")
print(table(df$Register))

# --- 4. Final 58 retained features (as in the 6-factor model) -------------------
features_58 <- c(
  "MDWO","PASS","CUZ","COND","RFACT","EX","XX0","MDWS","SPLIT","ABLE","QUAN","HDG",
  "RB","DWNT","DMA","PIT","LDE","NNABSPROC","VBG","CC","JJAT","TTR","THRC","CAUSE","NCOMP",
  "MENTAL","PP2","NNHUMAN","PP1S","ASPECT","PLACE","QUPR","ToVDSR","THATD","DOAUX",
  "MDCA","ELAB","JJPR","NNP","VBD","CD",
  "ThVCOMM","COMM","THSC","OCCUR",
  "EMPH","NNQUANT","COMPAR","JJSIZE","MDNE",
  "PP1P","NNTECH","JJEPSTother","MDCO",
  "TIME","YNQU","ACT","JJTIME"
)
stopifnot(length(features_58) == 58, !anyDuplicated(features_58))
missing_cols <- setdiff(features_58, names(df))
if (length(missing_cols) > 0) stop("Missing columns: ", paste(missing_cols, collapse = ", "))

# --- 5. One feature matrix per register, rows aligned by item_id ----------------
registers <- c("LOCNESS_GPT", "LOCNESS_Humanised", "LOCNESS_Human")

X <- lapply(registers, function(r) {
  d <- df %>% filter(Register == r) %>% arrange(item_id)
  stopifnot(!anyDuplicated(d$item_id))
  m <- as.matrix(d[, features_58])
  rownames(m) <- d$item_id
  m
})
names(X) <- registers
stopifnot(identical(rownames(X[[1]]), rownames(X[[2]])),
          identical(rownames(X[[1]]), rownames(X[[3]])))
n <- nrow(X[[1]])

stopifnot(!any(sapply(X, anyNA)))   # no missing values, so pairwise deletion is not needed

zero_var <- lapply(X, function(m) colnames(m)[apply(m, 2, sd) == 0])
if (any(lengths(zero_var) > 0)) {
  warning("Zero-variance features (cells with NA correlations are skipped): ",
          paste(unique(unlist(zero_var)), collapse = ", "))
}

ut <- function(m) m[upper.tri(m)]
ci <- function(x) quantile(x, c(.025, .975), na.rm = TRUE)
spearman_brown <- function(r) 2 * r / (1 + r)

# --- 6. Full analysis for one correlation method -------------------------------
run_analysis <- function(method) {
  cat("\n--- Running", method, "analysis ---\n")
  # use = "everything" (the default) lets R rank each column once for Spearman, which is
  # far faster than pairwise deletion; the data have no missing values (checked below),
  # and NA cells from zero-variance features are still dropped in mat_r().
  cm    <- function(M) suppressWarnings(cor(M, method = method))
  mat_r <- function(A, B) suppressWarnings(cor(ut(cm(A)), ut(cm(B)), use = "complete.obs"))
  
  # Observed matrix correlations
  obs <- c(GH = mat_r(X[[1]], X[[2]]),   # GPT vs Humanised
           GS = mat_r(X[[1]], X[[3]]),   # GPT vs Student
           HS = mat_r(X[[2]], X[[3]]))   # Humanised vs Student
  
  # Bootstrap over triplets (used for the differences only; see note below)
  cat("  bootstrap...\n")
  boot <- t(replicate(B, {
    i <- sample.int(n, n, replace = TRUE)
    c(GH = mat_r(X[[1]][i, ], X[[2]][i, ]),
      GS = mat_r(X[[1]][i, ], X[[3]][i, ]),
      HS = mat_r(X[[2]][i, ], X[[3]][i, ]))
  }))
  
  # No bootstrap CIs for the individual r values: resampling with replacement duplicates
  # items, which lowers matrix correlations, so those intervals sit below the estimates.
  # The differences are less affected because the bias applies to every term alike.
  
  # Mantel permutation test (minor check; reported p is bounded at 1/(permutations + 1))
  cat("  Mantel tests...\n")
  mantel_p <- c(
    mantel(as.dist(cm(X[[1]])), as.dist(cm(X[[2]])), permutations = 9999)$signif,
    mantel(as.dist(cm(X[[1]])), as.dist(cm(X[[3]])), permutations = 9999)$signif,
    mantel(as.dist(cm(X[[2]])), as.dist(cm(X[[3]])), permutations = 9999)$signif
  )
  
  pair_tab <- tibble(
    Method     = method,
    Comparison = c("GPT vs Humanised", "GPT vs Student", "Humanised vs Student"),
    r          = round(unname(obs), 3),
    Mantel_p   = mantel_p
  )
  
  diff_tab <- tibble(
    Method     = method,
    Difference = c("r(GPT,Humanised) - r(GPT,Student)",
                   "r(GPT,Humanised) - r(Humanised,Student)",
                   "r(Humanised,Student) - r(GPT,Student)"),
    Estimate   = round(c(obs["GH"] - obs["GS"], obs["GH"] - obs["HS"], obs["HS"] - obs["GS"]), 3),
    CI_low     = round(c(ci(boot[, "GH"] - boot[, "GS"])[1],
                         ci(boot[, "GH"] - boot[, "HS"])[1],
                         ci(boot[, "HS"] - boot[, "GS"])[1]), 3),
    CI_high    = round(c(ci(boot[, "GH"] - boot[, "GS"])[2],
                         ci(boot[, "GH"] - boot[, "HS"])[2],
                         ci(boot[, "HS"] - boot[, "GS"])[2]), 3)
  )
  
  # Split-half baseline within each register
  # Each half has n/2 = 72 texts. Mean_r is the similarity of two half-size matrices;
  # SB_corrected applies the Spearman-Brown formula 2r/(1+r) to estimate the similarity
  # expected for two full-size (n = 144) matrices from the same register.
  cat("  split-half baseline...\n")
  half <- floor(n / 2)
  split_tab <- bind_rows(lapply(registers, function(r) {
    M <- X[[r]]
    v <- replicate(B, {
      idx <- sample.int(n)
      mat_r(M[idx[1:half], ], M[idx[(half + 1):(2 * half)], ])
    })
    m_r <- mean(v, na.rm = TRUE)
    tibble(Method       = method,
           Register     = r,
           Mean_r       = round(m_r, 3),
           CI_low       = round(ci(v)[1], 3),
           CI_high      = round(ci(v)[2], 3),
           SB_corrected = round(spearman_brown(m_r), 3))
  }))
  
  list(pairs = pair_tab, diffs = diff_tab, split = split_tab)
}

# --- 7. Run: Pearson (main analysis) and Spearman (robustness check) ------------
res <- lapply(c("pearson", "spearman"), run_analysis)

pair_all  <- bind_rows(lapply(res, `[[`, "pairs"))
diff_all  <- bind_rows(lapply(res, `[[`, "diffs"))
split_all <- bind_rows(lapply(res, `[[`, "split"))

# --- 8. Output ------------------------------------------------------------------
cat("\n=== Matrix correlations (144 matched triplets, 58 features) ===\n"); print(pair_all,  width = Inf)
cat("\n=== Differences between matrix correlations ===\n");               print(diff_all,  width = Inf)
cat("\n=== Split-half baseline within register ===\n");                   print(split_all, width = Inf)

write_csv(pair_all,  paste0("matrix_similarity_pairs_HumanAIproject_",       run_date, ".csv"))
write_csv(diff_all,  paste0("matrix_similarity_differences_HumanAIproject_", run_date, ".csv"))
write_csv(split_all, paste0("matrix_similarity_splithalf_HumanAIproject_",   run_date, ".csv"))

writeLines(capture.output(sessionInfo()), paste0("C:/Users/miss_/OneDrive - The University of Queensland/PhD/side-project/sessionInfo_end_", Sys.Date(), ".txt"))