writeLines(capture.output(sessionInfo()), paste0("C:/Users/miss_/OneDrive - The University of Queensland/PhD/side-project/sessionInfo_", Sys.Date(), ".txt"))

library(officer)
library(stringr)

# Highlights the features of each dimension in an MFTE-tagged text.
# Bold = positive pole; italic + underlined = negative pole.
# Tag lists follow the final six-factor model: each feature is listed only on the
# dimension it was assigned to (cross-loading features on their stronger factor).
#
# NOTE: LDE (lexical density) and TTR (type-token ratio) are text-level measures,
# not word-level tags, so they cannot be highlighted. They are kept in the D1 list
# for completeness; the tag check below will report them as not found.

dim_tags <- list(
  D1 = list(
    pos = c("MDWO", "PASS", "CUZ", "COND", "RFACT", "EX", "XX0", "MDWS", "SPLIT",
            "ABLE", "QUAN", "HDG", "RB", "DWNT", "DMA", "PIT"),
    neg = c("LDE", "NNABSPROC", "VBG", "CC", "JJAT", "TTR", "THRC", "CAUSE", "NCOMP")
  ),
  D2 = list(
    pos = c("MENTAL", "PP2", "NNHUMAN", "PP1S", "ASPECT", "PLACE", "QUPR",
            "ToVDSR", "THATD", "DOAUX"),
    neg = character(0)
  ),
  D3 = list(
    pos = c("ELAB", "MDCA", "JJPR"),
    neg = c("NNP", "VBD", "CD")
  ),
  D4 = list(
    pos = c("ThVCOMM", "THSC", "COMM"),
    neg = c("OCCUR")
  ),
  D5 = list(
    pos = c("EMPH", "NNQUANT", "COMPAR", "JJSIZE"),
    neg = c("MDNE")
  ),
  D6 = list(
    pos = c("PP1P", "NNTECH", "JJEPSTother", "MDCO"),
    neg = character(0)
  )
)

# Choose the text to highlight. If the file name contains the dimension
# (e.g. "Europe6_D1_top.txt"), only that dimension is highlighted;
# otherwise set dims_to_highlight by hand.
text_file <- "computer12_D6_bottom.txt"
dim_in_name <- str_extract(basename(text_file), "D[1-6]")
dims_to_highlight <- if (!is.na(dim_in_name)) dim_in_name else c("D1", "D2", "D3", "D4", "D5", "D6")

ft     <- fp_text(bold = FALSE)
ft_pos <- fp_text(bold = TRUE)
ft_neg <- fp_text(italic = TRUE, underline = TRUE)

lines <- readLines(text_file)

# Parse each line once: word + its tags
parsed <- lapply(lines, function(line) {
  parts <- str_split(line, "_")[[1]]
  tag_string <- paste(parts[-1], collapse = " ")
  list(word = parts[1], tags = str_split(tag_string, "\\s+")[[1]])
})
all_tags <- unique(unlist(lapply(parsed, `[[`, "tags")))

text_id <- tools::file_path_sans_ext(basename(text_file))

# Stop early if the file doesn't look MFTE-tagged (word_TAG on each line);
# a plain-text excerpt would produce a document with nothing highlighted.
if (!any(grepl("_", lines))) {
  stop(text_file, " has no word_TAG lines. Use the MFTE-tagged version of the excerpt.")
}

for (d in dims_to_highlight) {
  pos_tags <- dim_tags[[d]]$pos
  neg_tags <- dim_tags[[d]]$neg

  # Check: which of this dimension's tags never occur in this text?
  # A tag listed here that is spelled differently in the tagged file would
  # silently never be highlighted, so this makes mismatches visible.
  missing <- setdiff(c(pos_tags, neg_tags), all_tags)
  if (length(missing) > 0) {
    cat(d, "- tags not found in", text_file, ":", paste(missing, collapse = ", "), "\n")
  }

  runs <- list()
  for (tok in parsed) {
    is_pos <- any(tok$tags %in% pos_tags)
    is_neg <- any(tok$tags %in% neg_tags)
    # If a word carries tags from both poles, it is shown as positive
    style <- if (is_pos) ft_pos else if (is_neg) ft_neg else ft
    runs[[length(runs) + 1]] <- ftext(paste0(tok$word, " "), style)
  }

  legend <- fpar(
    ftext(paste0(d, ": "), ft),
    ftext("positive pole", ft_pos),
    ftext(" | ", ft),
    ftext("negative pole", ft_neg)
  )

  doc <- read_docx()
  doc <- body_add_fpar(doc, legend)
  doc <- body_add_par(doc, "")
  doc <- body_add_fpar(doc, fpar(values = runs))
  # Add the dimension to the output name only if the file name doesn't already contain it
  out_name <- if (!is.na(dim_in_name)) paste0(text_id, "_highlighted.docx") else paste0(text_id, "_", d, "_highlighted.docx")
  print(doc, target = out_name)
}


writeLines(capture.output(sessionInfo()), paste0("C:/Users/miss_/OneDrive - The University of Queensland/PhD/side-project/sessionInfo_end_", Sys.Date(), ".txt"))
