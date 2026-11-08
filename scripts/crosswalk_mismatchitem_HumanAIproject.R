library(dplyr)
library(tidyr)
library(stringr)
library(readr)

all_files <- read_csv("EFA_ready_with_register_HumanAIproject.csv") %>%
  select(filename, Register)


# Manual topic-family mapping (extend as you find more variants)
topic_map <- c(
  "foxhunting" = "fox", "fox" = "fox",
  "eatingbeef" = "beef", "beef" = "beef",
  "parliamentarysystem" = "parliament", "parliament" = "parliament", "parliamentary" = "parliament",
  "transportation" = "transport", "transport" = "transport",
  "computerandlife" = "computerandlife", "computer" = "computerandlife",  # confirmed same topic
  "vitro" = "vitro",
  "boxing" = "boxing", "europe" = "europe", "lottery" = "lottery",
  "monarchy" = "monarchy"
)

parse_filename <- function(fn) {
  fn_clean <- fn %>% str_remove("\\.txt$") %>% str_to_lower() %>% str_remove_all(" ")
  number <- str_extract(fn_clean, "\\d+$")
  topic_raw <- str_remove(fn_clean, "\\d+$")
  topic_family <- topic_map[topic_raw]
  tibble(topic_raw = topic_raw, topic_family = topic_family, number = number)
}

# Apply to your full filename list (all_files = your Filename + Register dataframe)
parsed <- all_files %>%
  rowwise() %>%
  mutate(parsed = list(parse_filename(filename))) %>%
  unnest(parsed)



# Build item_id = topic_family + number, shared across registers
parsed <- parsed %>% mutate(item_id = paste0(topic_family, "_", number))

# Check: does every item_id appear exactly once per register?
parsed %>% count(item_id, Register) %>% filter(n > 1)  # should be empty
crosswalk_check <- parsed %>% count(item_id, Register) %>% tidyr::pivot_wider(names_from = Register, values_from = n, values_fill = 0)
print(crosswalk_check)  # rows where any register ≠ 1 need manual attention

write_csv(parsed, "item_crosswalk_HumanAIproject.csv")

incomplete_items <- crosswalk_check %>%
  filter(GPTcorpus != 1 | Humanisedcorpus != 1 | Human_LOCNESS != 1)
print(incomplete_items)

write_csv(parsed, "item_crosswalk_HumanAIproject.csv")
write_csv(incomplete_items, "crosswalk_incomplete_items_HumanAIproject.csv")

parse_filename


