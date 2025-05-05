
library(tidyverse)

cleaned_data = read_csv("cleaned_dataset_sahil.csv")

final_data = cleaned_data |> filter(!is.na(imdb_rating))

#Dataset transformation for director per row
final_data_per_dir <- final_data |>
  separate_rows(director, sep = ",") |> mutate(director = str_trim(director))

write_csv(final_data_per_dir, "cleaned_dataset_director_perROW.csv")      

#Dataset transformation for cast per row
final_data_per_cast <- final_data |>
  separate_rows(cast, sep = ",") |> mutate(cast = str_trim(cast))

write_csv(final_data_per_cast, "cleaned_dataset_cast_perROW.csv")      

#Dataset transformation for genre per row
final_data_per_genre <- final_data |>
  separate_rows(genre, sep = "•") |> mutate(genre = str_trim(genre))

write_csv(final_data_per_genre, "cleaned_dataset_genre_perROW.csv")      

#Dataset transformation for atomic values for each cell
final_data_per_dir_per_cast_per_genre = final_data |>
  separate_rows(director, sep = ",") |> mutate(director = str_trim(director)) |>
  separate_rows(cast, sep = ",") |> mutate(cast = str_trim(cast)) |>
  separate_rows(genre, sep = "•") |> mutate(genre = str_trim(genre))

write_csv(final_data_per_dir_per_cast_per_genre, "cleaned_dataset__master.csv")      
