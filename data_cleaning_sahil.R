
# Load required libraries
library(tidyverse)
library(stringr)
library(rvest)
library(readr)
library(httr)
library(jsonlite)

data_0 = read_csv("cleaned_dataset_lohith.csv")

data = data_0 |> filter(year >= 1950 & year <= 2024) |> arrange(year)

# API key
#api_key = "48ff0941"
api_key = "3b100719"

get_rating = function(title, year) {
  title = str_squish(title)
  year = as.integer(year)
  
  tryCatch(
  {
    # First try with year
    url1 = paste0("http://www.omdbapi.com/?t=",
                   URLencode(title), "&y=", year, "&apikey=", api_key)
    res1 = httr::GET(url1)
    content1 = fromJSON(content(res1, as = "text"))
    
    if (!is.null(content1$imdbRating) && content1$imdbRating != "N/A")
    {
      #print(paste0(title,"  ",year," - 1"))
      return(as.numeric(content1$imdbRating))
    }
    
    # Fallback: try without year
    url2 = paste0("http://www.omdbapi.com/?t=", URLencode(title), "&apikey=", api_key)
    res2 = httr::GET(url2)
    content2 = fromJSON(content(res2, as = "text"))
    
    if (!is.null(content2$imdbRating) && content2$imdbRating != "N/A") 
    {
      #print(paste0(title,"  ",year," - 2"))
      return(as.numeric(content2$imdbRating))
    }
    print(paste0(title,"  ",year," - NA"))
    return(NA)
  }, 
  error = function(e) 
  {
    #print("error")
    return(NA)
  })
}

movies_with_ratings = data |>
  rowwise() |>
  mutate(imdb_rating = get_rating(title, year)) |>
  ungroup()

na_ratings = movies_with_ratings |> filter(is.na(imdb_rating))

na_ratings = na_ratings |>
  mutate(title2 = str_trim(str_remove(title, ":.*$")))  # remove colon and everything after it
  
movies_with_ratings_1 = na_ratings |>
  rowwise() |>
  mutate(imdb_rating = get_rating(title2, year)) |>
  ungroup()

na_ratings2 = movies_with_ratings_1 |> filter(is.na(imdb_rating))

# Left join to bring in updated ratings
merged = movies_with_ratings |>
  left_join(movies_with_ratings_1, by = "title", suffix = c("", "_new")) |>
  mutate(imdb_rating = coalesce(imdb_rating, imdb_rating_new)) |>
  select(-imdb_rating_new)

movie_data_rating = merged |>
  select(-which(str_detect(names(merged), "_new")),-title2)

movie_data_rating_NoNa <- na.omit(movie_data_rating)

write_csv(movie_data_rating_NoNa, "cleaned_dataset_sahil.csv")

