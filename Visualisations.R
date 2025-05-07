library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

# Create decade column and summarize
merged <- movies_with_ratings
a <- merged %>%
  mutate(decade = floor(as.numeric(year) / 10) * 10) %>%
  group_by(decade) %>%
  summarise(total = n(), .groups = "drop")

# Plot the results
ggplot(a, aes(x = as.factor(decade), y = total)) +
  geom_col(fill = "darkorange") +
  labs(
    title = "Number of Movies per Decade",
    x = "Decade",
    y = "Movie Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

top_directors <- merged %>%
  group_by(director) %>%
  filter(n() >= 5) %>%
  summarise(avg_rating = mean(imdb_rating, na.rm = TRUE)) %>%
  arrange(desc(avg_rating)) %>%
  slice_head(n = 15)

ggplot(top_directors, aes(x = reorder(director, avg_rating), y = avg_rating)) +
  geom_col(fill = "lightblue") +
  coord_flip() +
  labs(title = "Top 15 Directors by Average IMDb Rating (min 5 movies)",
       x = "Director", y = "Average IMDb Rating") + theme_minimal()

# Step 2: Filter original dataset for only those directors
top_director_movies <- merged %>%
  filter(director %in% top_directors$director)

# Step 3: Create boxplot
ggplot(top_director_movies, aes(x = reorder(director, imdb_rating, FUN = median)
                                , y = imdb_rating)) +
  geom_boxplot(fill = "lightgreen", outlier.color = "black") +
  coord_flip() +
  labs(title = "IMDb Rating Distributions for Top 15 Directors (min 5 movies)",
       x = "Director", y = "IMDb Rating") +
  theme_minimal()

merged <- merged %>%
  mutate(year = as.numeric(year))

decade <- merged %>%
  mutate(decade = floor(year / 10) * 10)

ggplot(decade, aes(x = as.factor(decade), y = imdb_rating)) +
  geom_boxplot() +
  scale_x_discrete(name = "Decade") +
  labs(title = "IMDb Ratings by Decade", y = "IMDb Rating") +
  theme_minimal()

library(dplyr)
library(ggplot2)
library(stringr)

# Extract primary genre and compute decade
genre_decade <- merged %>%
  mutate(year = as.numeric(year),
         decade = floor(year / 10) * 10,
         primary_genre = str_trim(str_split_fixed(genre, "•", 2)[, 1])) %>%
  filter(!is.na(primary_genre), !is.na(decade))

# Count genres per decade
genre_counts <- genre_decade %>%
  group_by(decade, primary_genre) %>%
  summarise(count = n(), .groups = "drop")

# Plot top genres (filtering top 7 overall for clarity)
top_genres <- genre_counts %>%
  group_by(primary_genre) %>%
  summarise(total = sum(count)) %>%
  arrange(desc(total)) %>%
  slice_head(n = 7) %>%
  pull(primary_genre)

ggplot(genre_counts %>% filter(primary_genre %in% top_genres), 
       aes(x = decade, y = count, color = primary_genre)) +
  geom_line(size = 1.2) +
  scale_x_continuous(breaks = seq(1890, 2020, by = 20), limits = c(1880, 2025)) +
  labs(title = "Top Genre Trends by Decade",
       x = "Decade", y = "Number of Movies", color = "Genre") +
  theme_minimal()

runtime_decade <- merged %>%
  mutate(year = as.numeric(year),
         duration = as.numeric(duration),
         decade = floor(year / 10) * 10) %>%
  filter(!is.na(duration), duration > 20, duration < 300) # filtering unrealistic values

runtime_trend <- runtime_decade %>%
  group_by(decade) %>%
  summarise(avg_runtime = mean(duration, na.rm = TRUE), .groups = "drop")

ggplot(runtime_trend, aes(x = decade, y = avg_runtime)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(1890, 2020, by = 10), limits = c(1880, 2025)) +
  labs(title = "Average Movie Runtime by Decade",
       x = "Decade", y = "Average Runtime (minutes)") +
  theme_minimal()


# Prepare data
runtime_by_genre <- merged %>%
  mutate(
    year = as.numeric(year),
    duration = as.numeric(duration),
    decade = floor(year / 10) * 10,
    primary_genre = str_trim(str_split_fixed(genre, "•", 2)[, 1])
  ) %>%
  filter(!is.na(duration), duration > 20, duration < 300, !is.na(primary_genre), !is.na(decade))

# Focus on top 5 most common genres
top_genres <- runtime_by_genre %>%
  count(primary_genre, sort = TRUE) %>%
  slice_head(n = 5) %>%
  pull(primary_genre)

# Compute average runtime per genre per decade
avg_runtime_genre <- runtime_by_genre %>%
  filter(primary_genre %in% top_genres) %>%
  group_by(decade, primary_genre) %>%
  summarise(avg_runtime = mean(duration, na.rm = TRUE), .groups = "drop")

# Plot
ggplot(avg_runtime_genre, aes(x = decade, y = avg_runtime, color = primary_genre)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(1880, 2020, by = 10), limits = c(1880, 2025)) +
  labs(
    title = "Average Runtime by Genre and Decade",
    x = "Decade", y = "Average Runtime (minutes)",
    color = "Genre"
  ) +
  theme_minimal()


ggplot(avg_runtime_genre, aes(x = decade, y = avg_runtime)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "black", size = 2) +
  facet_wrap(~ primary_genre) +
  scale_x_continuous(breaks = seq(1880, 2020, by = 10), limits = c(1880, 2025)) +
  labs(
    title = "Average Movie Runtime by Genre and Decade (Faceted)",
    x = "Decade", y = "Average Runtime (minutes)"
  ) +
  theme_minimal()

ggplot(runtime_by_genre %>% filter(primary_genre %in% top_genres),
       aes(x = duration, y = imdb_rating, color = primary_genre)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    title = "IMDb Rating vs Runtime by Genre",
    x = "Runtime (minutes)", y = "IMDb Rating", color = "Genre"
  ) +
  theme_minimal()


# Step 1: Get top 15 directors by average rating
top_15_directors <- merged %>%
  group_by(director) %>%
  filter(n() >= 5) %>%
  summarise(avg_rating = mean(imdb_rating, na.rm = TRUE)) %>%
  arrange(desc(avg_rating)) %>%
  slice_head(n = 15) %>%
  pull(director)

# Step 2: Filter dataset and separate genres
ratings_by_genre <- merged %>%
  filter(director %in% top_15_directors) %>%
  select(director, title, genre, imdb_rating) %>%
  mutate(genre = str_squish(genre)) %>%
  separate_rows(genre, sep = " • ") %>%
  filter(!is.na(imdb_rating) & genre != "") %>%
  group_by(director, genre) %>%
  summarise(avg_rating = mean(imdb_rating, na.rm = TRUE), n = n(), .groups = "drop") %>%
  filter(n >= 2)  # Only keep genres with at least 2 movies per director

ggplot(ratings_by_genre, aes(x = reorder(genre, avg_rating), y = avg_rating, fill = genre)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  coord_flip() +
  facet_wrap(~ director, scales = "free_y") +
  labs(
    title = "Average IMDb Rating by Genre for Top 15 Directors",
    x = "Genre", y = "Average IMDb Rating", fill = "Genre"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# Step 1: Split genres into separate rows
genre_diversity <- merged %>%
  filter(director %in% top_15_directors) %>%
  select(director, genre) %>%
  separate_rows(genre, sep = "•") %>%
  mutate(genre = str_squish(genre)) %>%
  distinct(director, genre) %>%
  count(director, name = "genre_count") %>%
  arrange(desc(genre_count))

# Step 2: Plot
ggplot(genre_diversity, aes(x = reorder(director, genre_count), y = genre_count)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Genre Diversity of Top 15 Directors",
       x = "Director",
       y = "Number of Unique Genres") +
  theme_minimal()

# Step 1: Genre diversity
genre_diversity <- merged %>%
  filter(director %in% top_15_directors) %>%
  select(director, genre) %>%
  separate_rows(genre, sep = "•") %>%
  mutate(genre = str_squish(genre)) %>%
  distinct(director, genre) %>%
  count(director, name = "genre_count")

# Step 2: IMDb rating variance + average
rating_stats <- merged %>%
  filter(director %in% top_15_directors) %>%
  group_by(director) %>%
  summarise(
    rating_sd = sd(imdb_rating, na.rm = TRUE),
    avg_rating = mean(imdb_rating, na.rm = TRUE)
  )

# Step 3: Merge all
diversity_vs_variance <- left_join(genre_diversity, rating_stats, by = "director")

# Step 4: Plot with color by average rating
ggplot(diversity_vs_variance, aes(x = genre_count, y = rating_sd, label = director, color = avg_rating)) +
  geom_point(size = 4) +
  geom_text(vjust = -0.7, size = 3) +
  scale_color_gradient(low = "skyblue", high = "red") +
  labs(title = "Genre Diversity vs IMDb Rating Variance (Colored by Avg. Rating)",
       x = "Number of Unique Genres",
       y = "Standard Deviation of IMDb Ratings",
       color = "Avg. IMDb Rating") +
  theme_minimal()

library(tidyverse)

cast_long <- merged %>%
  select(title, year, imdb_rating, cast) %>%
  separate_rows(cast, sep = ",\\s*") %>%
  mutate(cast = str_squish(cast)) %>%
  filter(cast != "")
actor_counts <- cast_long %>%
  count(cast, sort = TRUE)
actor_ratings <- cast_long %>%
  group_by(cast) %>%
  summarise(
    num_movies = n(),
    avg_rating = mean(imdb_rating, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(num_movies >= 5) %>%
  arrange(desc(avg_rating))

top_actors <- actor_counts %>% slice_max(n, n = 15)

ggplot(top_actors, aes(x = reorder(cast, n), y = n)) +
  geom_col(fill = "skyblue") +
  coord_flip() +
  labs(title = "Top 15 Most Frequent Actors", x = "Actor", y = "Number of Movies") +
  theme_minimal()


top_rated_actors <- actor_ratings %>% slice_max(avg_rating, n = 15)

ggplot(top_rated_actors, aes(x = reorder(cast, avg_rating), y = avg_rating)) +
  geom_col(fill = "lightgreen") +
  coord_flip() +
  labs(title = "Top 15 Actors by Avg IMDb Rating (min 5 movies)", x = "Actor", y = "Avg IMDb Rating") +
  theme_minimal()


top_actor_names <- top_rated_actors$cast

cast_long %>%
  filter(cast %in% top_actor_names) %>%
  ggplot(aes(x = imdb_rating, y = reorder(cast, imdb_rating))) +
  geom_boxplot(fill = "plum") +
  labs(title = "IMDb Rating Distributions for Top Actors", x = "IMDb Rating", y = "Actor") +
  theme_minimal()

cast_decade <- cast_long %>%
  filter(cast %in% top_actor_names) %>%
  mutate(decade = floor(as.numeric(year) / 10) * 10)
actor_decade_counts <- cast_decade %>%
  group_by(cast, decade) %>%
  summarise(movie_count = n(), .groups = "drop")


ggplot(actor_decade_counts, aes(x = decade, y = movie_count, fill = cast)) +
  geom_col(position = "dodge") +
  scale_x_continuous(breaks = seq(1880, 2020, by = 10)) +
  labs(title = "Movies Released per Decade for Top 15 Actors",
       x = "Decade", y = "Number of Movies") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 3))

cast_long <- cast_long %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(!is.na(year))

# Get top 15 most frequent actors
top_actors <- cast_long %>%
  count(cast, sort = TRUE) %>%
  slice_max(n, n = 15) %>%
  pull(cast)

# Filter for those actors and calculate decade
actor_decades <- cast_long %>%
  filter(cast %in% top_actors) %>%
  mutate(decade = floor(year / 10) * 10)

# Group and count
actor_decade_counts <- actor_decades %>%
  group_by(cast, decade) %>%
  summarise(movie_count = n(), .groups = "drop")



ggplot(actor_decade_counts, aes(x = decade, y = movie_count)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~ cast, scales = "free_y") +
  scale_x_continuous(breaks = seq(1880, 2020, by = 10)) +
  labs(title = "Movies Released per Decade for Top 15 Frequent Actors",
       x = "Decade", y = "Number of Movies") +
  theme_minimal()

# Step 1: Ensure `year` is numeric
cast_long <- cast_long %>%
  mutate(year = as.numeric(year)) %>%
  filter(!is.na(year))

# Step 2: Top 15 most frequent actors
top_actors <- cast_long %>%
  count(cast, sort = TRUE) %>%
  slice_max(n, n = 15) %>%
  pull(cast)

# Step 3: Movies per decade per actor + avg IMDb rating
actor_decade_stats <- cast_long %>%
  filter(cast %in% top_actors) %>%
  mutate(decade = floor(year / 10) * 10) %>%
  group_by(cast, decade) %>%
  summarise(
    movie_count = n(),
    avg_rating = mean(imdb_rating, na.rm = TRUE),
    .groups = "drop"
  )

# Step 4: Plot
ggplot(actor_decade_stats, aes(x = decade, y = movie_count, fill = avg_rating)) +
  geom_col() +
  facet_wrap(~ cast, scales = "free_y") +
  scale_x_continuous(breaks = seq(1880, 2020, by = 10)) +
  scale_fill_gradient(low = "skyblue", high = "darkred", na.value = "grey80") +
  labs(
    title = "Movies per Decade Colored by Avg IMDb Rating (Top 15 Actors)",
    x = "Decade", y = "Number of Movies", fill = "Avg IMDb Rating"
  ) +
  theme_minimal()


df <- merged

df <- df %>%
  mutate(title_lower = str_to_lower(title))

# Function to extract base title (removes sequel indicators)
extract_base_title <- function(title) {
  title <- str_remove_all(title,
                          "\\b(part\\s+[ivx]+|part\\s+\\d+|\\bii\\b|\\biii
                          \\b|\\biv\\b|\\bv\\b|\\bvi\\b|\\d+)\\b")
  str_trim(title)
}

# Apply the function
df <- df %>%
  mutate(base_title = extract_base_title(title_lower))

# Count base title occurrences
base_counts <- df %>%
  count(base_title) %>%
  filter(n > 1)

# Filter the original dataset to only movies with sequels
sequels_df <- df %>%
  filter(base_title %in% base_counts$base_title)

# Arrange by base title and year, assign order number
sequels_clustered <- sequels_df %>%
  arrange(base_title, year) %>%
  group_by(base_title) %>%
  mutate(sequel_number = row_number()) %>%
  ungroup() %>%
  select(base_title,title, sequel_number, year, director, imdb_rating,cast) 


# Load the data
df <- sequels_clustered

# Convert cast column to list of character vectors
df <- df %>%
  mutate(cast_list = str_split(cast, ","))

# Group and filter
filtered_df <- df %>%
  group_by(base_title) %>%
  filter(
    n() > 1,
    {
      # Check if director is same
      same_director <- n_distinct(director) == 1
      
      # Check for at least one common cast member
      common_cast <- Reduce(intersect, cast_list)
      has_common_cast <- length(common_cast) > 0
      
      same_director | has_common_cast
    }
  ) %>%
  ungroup() %>%
  select(title, sequel_number, year, director, cast, imdb_rating)


# Remove rows with missing IMDB ratings
sequels_clean <- filtered_df %>%
  filter(!is.na(imdb_rating))

# Extract base series title by removing common sequel indicators
sequels_clean <- sequels_clean %>%
  mutate(series = str_remove(title, "( Part.*| II| III| IV| V| 
                             VI| 2| 3| 4| 5| 6| 7| 8| 9)$"))

# Count number of entries per sequel number
sequel_counts <- sequels_clean %>%
  count(sequel_number)

# Calculate median ratings per sequel number
medians <- sequels_clean %>%
  group_by(sequel_number) %>%
  summarise(median_rating = median(imdb_rating), .groups = "drop")

# Plot
ggplot(sequels_clean, aes(x = factor(sequel_number), y = imdb_rating)) +
  geom_boxplot() +
  geom_text(data = sequel_counts, aes(x = factor(sequel_number), y = 9.5,
                                      label = paste0("n=", n)),
            inherit.aes = FALSE, size = 3.5, vjust = -0.5) +
  geom_line(data = medians, aes(x = sequel_number, y = median_rating),
            group = 1, color = "blue", size = 1.2) +
  geom_point(data = medians, aes(x = sequel_number, y = median_rating),
             color = "blue", size = 2.5) +
  labs(
    title = "IMDB Rating Trends for Sequel Movie Series",
    subtitle = "Annotated with counts (n) and median rating trendline",
    x = "Sequel Number",
    y = "IMDB Rating"
  ) +
  ylim(4, 10) +
  theme_minimal()


# Normalize and cluster by title
df <- df %>%
  mutate(normalized_title = str_to_lower(str_trim(title)))

duplicate_titles <- df %>%
  count(normalized_title) %>%
  filter(n > 1)

same_title_movies <- df %>%
  filter(normalized_title %in% duplicate_titles$normalized_title) %>%
  arrange(normalized_title, year) %>%
  group_by(normalized_title) %>%
  mutate(
    title_group = cur_group_id(),          # Group ID for each title
    sequel_number = row_number()           # 1, 2, 3... within each title
  ) %>%
  ungroup() %>% select(-8,-9)

# Count number of entries per sequel number
sequel_counts <- same_title_movies %>%
  count(sequel_number)

# Calculate median ratings per sequel number
medians <- same_title_movies %>%
  group_by(sequel_number) %>%
  summarise(median_rating = median(imdb_rating), .groups = "drop")

# Plot
ggplot(same_title_movies, aes(x = factor(sequel_number), y = imdb_rating)) +
  geom_boxplot() +
  geom_text(data = sequel_counts, aes(x = factor(sequel_number), y = 9.5, 
                                      label = paste0("n=", n)), 
            inherit.aes = FALSE, size = 3.5, vjust = -0.5) +
  geom_line(data = medians, aes(x = sequel_number, y = median_rating), 
            group = 1, color = "blue", size = 1.2) +
  geom_point(data = medians, aes(x = sequel_number, y = median_rating), 
             color = "blue", size = 2.5) +
  labs(
    title = "IMDB Rating Trends for Reboots with Same Title",
    subtitle = "Annotated with counts (n) and median rating trendline",
    x = "Sequel Number",
    y = "IMDB Rating"
  ) +
  ylim(4, 10) +
  theme_minimal()

