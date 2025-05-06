# Load required libraries
library(rvest)
library(dplyr)
library(readr)
library(tidyverse)
library(stringr)
library(purrr)

# Define function to scrape one page of Title
scrape_flickchart_page <- function(page_num) {
  url <- paste0("https://www.flickchart.com/charts.aspx?perpage=10000&page=", page_num)
  message("Scraping page: ", page_num)
  
  page <- read_html(url)
  
  titles <- page %>%
    html_elements("#chart a span") %>%  
    html_text(trim = TRUE) %>%
    .[!str_detect(., "http[s]?://")] %>%  # Remove any link-like text
    .[. != ""]                            # Remove empty strings
  
  tibble(title = titles, page = page_num)
}

# Scrape pages 1 to 40
all_titles <- map_dfr(1:40, scrape_flickchart_page)


# Define function to scrape one page of Year
scrape_flickchart_page <- function(page_num) {
  url <- paste0("https://www.flickchart.com/charts.aspx?perpage=10000&page=", page_num)
  message("Scraping page: ", page_num)
  
  page <- read_html(url)
  
  titles <- page %>%
    html_elements(".chartsYear") %>%
    html_text(trim = TRUE) %>%
    .[!str_detect(., "http[s]?://")] %>%  # Remove any link-like text
    .[. != ""]                            # Remove empty strings
  
  tibble(title = titles, page = page_num)
}

# Scrape pages 1 to 40
all_years <- map_dfr(1:40, scrape_flickchart_page)


# Define function to scrape one page of Duration in min
scrape_flickchart_page <- function(page_num) {
  url <- paste0("https://www.flickchart.com/charts.aspx?perpage=10000&page=", page_num)
  message("Scraping page: ", page_num)
  
  page <- read_html(url)
  
  titles <- page %>%
    html_elements(".minutes") %>%
    html_text(trim = TRUE) %>%
    .[!str_detect(., "http[s]?://")] %>%  # Remove any link-like text
    .[. != ""]                            # Remove empty strings
  
  tibble(title = titles, page = page_num)
}

# Scrape pages 1 to 40
all_duration <- map_dfr(1:40, scrape_flickchart_page)


# Define function to scrape one page of Director name
scrape_flickchart_page <- function(page_num) {
  url <- paste0("https://www.flickchart.com/charts.aspx?perpage=10000&page=", page_num)
  message("Scraping page: ", page_num)
  
  page <- read_html(url)
  
  titles <- page %>%
    html_elements(".director .filterLink:nth-child(1)") %>%
    html_text(trim = TRUE) %>%
    .[!str_detect(., "http[s]?://")] %>%  # Remove any link-like text
    .[. != ""]                            # Remove empty strings
  
  tibble(title = titles, page = page_num)
}

# Scrape pages 1 to 40
all_director <- map_dfr(1:40, scrape_flickchart_page)

# Define function to scrape one page of Genre
scrape_flickchart_page <- function(page_num) {
  url <- paste0("https://www.flickchart.com/charts.aspx?perpage=10000&page=", page_num)
  message("Scraping page: ", page_num)
  
  page <- read_html(url)
  
  titles <- page %>%
    html_elements(".genre") %>%
    html_text(trim = TRUE) %>%
    .[!str_detect(., "http[s]?://")] %>%  # Remove any link-like text
    .[. != ""]                            # Remove empty strings
  
  tibble(title = titles, page = page_num)
}

# Scrape pages 1 to 40
all_genre <- map_dfr(1:40, scrape_flickchart_page)


# ----------------------------
# Diagnose issue: Check how many genres per page
# ----------------------------
df <- all_genre %>%
  group_by(page) %>%
  summarise(count = n())

# ----------------------------
# Fix issue: Page 30 has a missing genre at position 75
# We'll manually insert "Comedy • Comedy Drama" there
# and shift subsequent rows down by one position
# ----------------------------

# Take rows 1 to 74 as-is
all_genre_1 <- all_genre %>% slice(1:74)

# Manually add the missing 75th genre
all_genre_2 <- data.frame(title = "Comedy • Comedy Drama", page = 30)

# Take the remaining rows from 75 onwards
all_genre_3 <- all_genre %>% slice(75:9999)

# Combine all into final corrected dataset
all_genre_final <- bind_rows(all_genre_1, all_genre_2, all_genre_3)

# Define function to scrape one page of Cast names
scrape_flickchart_page <- function(page_num) {
     url <- paste0("https://www.flickchart.com/charts.aspx?perpage=10000&page=", page_num)
     message("Scraping page: ", page_num)
     
     page <- read_html(url)
     
     titles <- page %>%
         html_elements(".director") %>%
         html_text(trim = TRUE) %>%
         .[!str_detect(., "http[s]?://")] %>%  # Remove any link-like text
         .[. != ""]                            # Remove empty strings
     
     tibble(title = titles, page = page_num)
 }
 
# ----------------------------
# Step 1: Clean up the raw 'title' text from all_cast
# ----------------------------
all_cast_clean <- all_cast %>%
  mutate(
    # Remove unwanted control characters: carriage returns (\r), new lines (\n), tabs (\t)
    title = str_replace_all(title, "[\r\n\t]", ""),
    
    # Remove extra white spaces from start, end, and collapse multiple spaces in between
    title = str_squish(title)
  )

# ----------------------------
# Step 2: Extract only the cast part from the cleaned titles
# ----------------------------
all_cast_clean1 <- all_cast_clean %>%
  mutate(
    # Extract everything that comes after the phrase "Starring: "
    title = str_extract(title, "(?<=Starring: ).*")
  )


# Select and rename relevant columns from each dataset
cast <- all_cast_clean1 %>% select(title) %>% rename(cast = title)
director <- all_director %>% select(title) %>% rename(director = title)
duration <- all_duration %>% select(title) %>% rename(duration = title)
genre <- all_genre_final %>% select(title) %>% rename(genre = title)
year <- all_years %>% select(title) %>% rename(year = title)
title <- all_titles %>% select(title)  # Title remains unchanged

# Combine all components into a single dataset
movie_data <- bind_cols(title, year, director, duration, cast, genre)

# Step 3: Clean the 'duration' column to make it numeric-ready
movie_data <- movie_data %>%
  mutate(
    duration = str_replace_all(duration, ",", ""),          # Remove commas
    duration = str_remove(duration, "\\s*min\\.?$"),        # Remove "min." or "min" at end (with optional space)
    duration = str_squish(duration),                        # Squash any extra spaces
    duration = as.numeric(duration)                         # Convert to numeric
  )

# Save this cleaned dataset to CSV
write_csv(movie_data, "cleaned_dataset_lohith.csv")          
