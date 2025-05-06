library(stringr)
library(dplyr)
library(wordcloud2)
library(scales)  # for color mapping
library(readr)

movie_with_ratings <- read_csv("cleaned_dataset_sahil.csv")

# Step 1: Compute average IMDb rating per director
director_stats <- movie_with_ratings %>%
  filter(!is.na(director), !is.na(imdb_rating)) %>%
  mutate(director = str_squish(director)) %>%
  group_by(director) %>%
  summarise(
    freq = n(),
    avg_rating = mean(imdb_rating, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(freq))

# Step 2: Normalize ratings to colors
# Create gradient from blue (low rating) to red (high rating)
director_stats$color <- col_numeric(
  palette = c("black", "red"),
  domain = range(director_stats$avg_rating, na.rm = TRUE)
)(director_stats$avg_rating)

# Step 3: Build wordcloud2 with color
wordcloud2(
  data = director_stats %>% select(word = director, freq, color),
  color = director_stats$color,
  size = 0.7,  # adjust for compactness
  backgroundColor = "white"
)

#-------------------------------------------------------------------------------

# Step 1: Expand cast column into long format
cast_stats <- movie_with_ratings %>%
  select(cast, imdb_rating) %>%
  separate_rows(cast, sep = ",\\s*") %>%
  mutate(cast = str_squish(cast)) %>%
  filter(cast != "", !is.na(imdb_rating))

# Step 2: Calculate frequency and average IMDb rating
actor_summary <- cast_stats %>%
  group_by(cast) %>%
  summarise(
    freq = n(),
    avg_rating = mean(imdb_rating, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(freq)) %>%
  slice_max(freq, n = 1000)  # Limit to top 200 actors for clarity

# Step 3: Map ratings to color scale
actor_summary$color <- col_numeric(
  palette = c("black", "red"),
  domain = range(actor_summary$avg_rating, na.rm = TRUE)
)(actor_summary$avg_rating)

# Step 4: Generate word cloud
wordcloud2(
  data = actor_summary %>% select(word = cast, freq, color),
  color = actor_summary$color,
  size = 0.75,
  backgroundColor = "white"
)
