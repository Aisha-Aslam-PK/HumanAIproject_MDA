writeLines(capture.output(sessionInfo()), paste0("C:/Users/miss_/OneDrive - The University of Queensland/PhD/side-project/sessionInfo_", Sys.Date(), ".txt"))

# Scree plot from JASP's Parallel Analysis table
# Real-data vs simulated-data eigenvalues, built with ggplot2


library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)

# ---- 1. Read the table copied from JASP ----
# Edit this path to where the CSV is saved
csv_path <- "C:/Users/miss_/OneDrive - The University of Queensland/PhD/side-project/Parellel Analysis table for screeplot.csv"

# Excel often saves CSVs in a Windows encoding, not UTF-8, so detect it
enc <- guess_encoding(csv_path)$encoding[1]
if (is.na(enc)) enc <- "windows-1252"
message("Reading file as: ", enc)

# Read everything as text first; numbers are cleaned in step 2
pa_raw <- read_csv(csv_path,
                   locale = locale(encoding = enc),
                   col_types = cols(.default = col_character()),
                   show_col_types = FALSE)

# Rename by position: JASP's headings don't matter (and avoids odd characters in them)
# Assumes JASP's column order: 1 = Factor, 2 = Real data eigenvalues,
# 3 = Simulated data mean eigenvalues
pa_raw <- pa_raw[, 1:3]
names(pa_raw) <- c("Factor", "Real", "Simulated")

# Check the first rows before plotting
print(head(pa_raw, 10))

# ---- 2. Tidy the columns ----
# JASP writes negative numbers with a typographic minus sign (−), which R
# doesn't recognise; convert it (and dashes) to a normal "-" before parsing.
# parse_number() strips text such as "Factor 1" -> 1 and any "*" markers.
clean_num <- function(x) {
  x <- enc2utf8(x)
  x <- gsub("\u2212|\u2013|\u2014", "-", x)
  parse_number(x)
}

# Safety check: if Excel replaced the minus sign with "?", negatives would be lost
if (any(grepl("?", unlist(pa_raw[, 2:3]), fixed = TRUE))) {
  stop("Some values contain '?' - the minus signs were lost when Excel saved the file. ",
       "Re-save it from Excel as 'CSV UTF-8 (Comma delimited)' and run again.")
}

pa <- pa_raw %>%
  mutate(across(everything(), clean_num)) %>%
  filter(!is.na(Factor))

# Compare these against the JASP table, especially any negative values
print(pa, n = Inf)

# Long format: one row per factor per data type
pa_long <- pa %>%
  pivot_longer(c(Real, Simulated),
               names_to = "Data", values_to = "Eigenvalue") %>%
  mutate(Data = factor(Data,
                       levels = c("Real", "Simulated"),
                       labels = c("Real data", "Simulated data")))

# ---- 3. Plot ----
scree_cols <- c("Real data" = "#1F4E9A",       # blue
                "Simulated data" = "#E6A800")  # yellow (darker gold, readable on white)

scree <- ggplot(pa_long, aes(x = Factor, y = Eigenvalue,
                             colour = Data, shape = Data)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_colour_manual(values = scree_cols) +
  scale_shape_manual(values = c("Real data" = 16, "Simulated data" = 17)) +
  scale_x_continuous(breaks = c(0, 20, 40, 60)) +
  scale_y_continuous(breaks = c(-3, 0, 3, 6, 9, 12)) +
  coord_cartesian(xlim = c(0, 60), ylim = c(-3, 12)) +
  labs(x = "Factors", y = "Eigenvalue", colour = NULL, shape = NULL) +
  theme_classic(base_size = 13) +
  theme(legend.position = c(0.85, 0.85),
        legend.background = element_rect(fill = "white", colour = NA),
        axis.text = element_text(colour = "black"))

print(scree)

# ---- 4. Save ----
# Saved next to the CSV; edit the file name or folder if needed
ggsave(file.path(dirname(csv_path), "scree_plot.png"),
       scree, width = 7, height = 5, dpi = 300)

writeLines(capture.output(sessionInfo()), paste0("C:/Users/miss_/OneDrive - The University of Queensland/PhD/side-project/sessionInfo_end_", Sys.Date(), ".txt"))
