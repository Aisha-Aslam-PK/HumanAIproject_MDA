library(dplyr)
library(openxlsx)
library(readr)


# --- Load required files and rebuild the matched dataset ----------------------
dim_scores <- read_csv("dimension_scores_HumanAIproject_V2.csv")
parsed <- read_csv("item_crosswalk_HumanAIproject.csv")

dim_scores_matched <- dim_scores %>%
  left_join(parsed %>% select(filename, Register, item_id),
            by = c("filename", "Register"))

incomplete_ids <- c("computerandlife_14", "vitro_28")

dim_scores_matched <- dim_scores_matched %>%
  filter(!item_id %in% incomplete_ids)

# Sanity check
item_counts <- dim_scores_matched %>% count(item_id)
stopifnot(all(item_counts$n == 3))

dims <- c("D1", "D2", "D3", "D4", "D5", "D6")

top_bottom_list <- list()

for (d in dims) {
  ordered <- dim_scores_matched %>%
    arrange(desc(.data[[d]])) %>%
    select(filename, Register, item_id, all_of(d))
  
  top5 <- ordered %>% slice_head(n = 5) %>% mutate(Position = "Top 5", Dimension = d)
  bottom5 <- ordered %>% slice_tail(n = 5) %>% mutate(Position = "Bottom 5", Dimension = d)
  
  names(top5)[names(top5) == d] <- "Score"
  names(bottom5)[names(bottom5) == d] <- "Score"
  
  top_bottom_list[[d]] <- bind_rows(top5, bottom5)
}

top_bottom_all <- bind_rows(top_bottom_list) %>%
  select(Dimension, Position, filename, Register, item_id, Score)

print(top_bottom_all)
write.xlsx(top_bottom_all, "top_bottom5_by_dimension_MATCHED_HumanAIproject_V2.xlsx")