# PACKAGES
library(scholar)
library(dplyr)
library(tidytext)
library(stringr)
library(wordcloud2)

# 1. DOWNLOAD GOOGLE SCHOLAR PUBLICATIONS
scholar_id <- "omB-4zQAAAAJ"

papers <- scholar::get_publications(scholar_id)

# Check number of publications retrieved
cat("Publications retrieved:", nrow(papers), "\n")

# 2. CLEAN PUBLICATION TITLES
# Remove missing and duplicate titles.
# This includes journal papers and conference abstracts.

papers_clean <- papers %>%
  dplyr::filter(!is.na(title), title != "") %>%
  dplyr::mutate(
    title_key = stringr::str_squish(
      stringr::str_to_lower(title)
    )
  ) %>%
  dplyr::distinct(title_key, .keep_all = TRUE)

# 3. EXTRACT AND COUNT WORDS
words <- papers_clean %>%
  dplyr::select(title) %>%
  tidytext::unnest_tokens(word, title) %>%
  dplyr::anti_join(
    tidytext::stop_words,
    by = "word"
  ) %>%
  dplyr::mutate(
    # Combine singular and plural forms
    word = dplyr::recode(
      word,
      "soils" = "soil",
      "communities" = "community",
      "plants" = "plant",
      "roots" = "root"
    )
  ) %>%
  dplyr::filter(
    nchar(word) > 3,
    !word %in% c(
      "implications",
      "affect",
      "altered",
      "change",
      "effects",
      "versus"
    )
  ) %>%
  dplyr::count(word, sort = TRUE)

# Inspect the most frequent words
print(head(words, 20))

# 4. CHOOSE A COLOR PALETTE
# Change palette_name to try different options:
# "random", "ocean", "earth", "viridis", "warm"

palette_name <- "earth"

palettes <- list(
  ocean = c(
    "#003049", "#0077B6", "#00B4D8",
    "#2A9D8F", "#52B69A"
  ),
  earth = c(
    "#31572C", "#4F772D", "#90A955",
    "#8B5E3C", "#582F0E"
  ),
  viridis = c(
    "#440154", "#3B528B", "#21918C",
    "#5EC962", "#FDE725"
  ),
  warm = c(
    "#6A040F", "#9D0208", "#DC2F02",
    "#F48C06", "#FFBA08"
  )
)

# Assign a color to each word
set.seed(123)

if (palette_name == "random") {
  word_colors <- "random-dark"
} else {
  word_colors <- sample(
    palettes[[palette_name]],
    size = nrow(words),
    replace = TRUE
  )
}

# 5. CREATE THE WORD CLOUD
set.seed(123)

cloud <- wordcloud2::wordcloud2(
  data = words,
  size = 0.8,
  color = word_colors,
  backgroundColor = "white",
  shape = "circle",
  rotateRatio = 0.3,
  minRotation = -pi/2,
  maxRotation = pi/2,
  fontWeight = "bold",
  ellipticity = 0.65
)

# Display in RStudio Viewer
cloud
