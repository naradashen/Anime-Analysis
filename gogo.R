# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Sample data frame (replace with actual data loading)
# df <- read.csv("your_anime_data.csv") # Uncomment this line to load your data

# Example dataset (assuming 'genre' and 'released' columns exist in df)
# Replace this with your actual data loading process
df <- data.frame(
  genre = c("Action, Adventure", "Comedy, Fantasy", "Action, Sci-Fi", "Drama, Adventure", "Comedy"),
  released = c(1995, 2001, 1995, 2005, 2010)
)

# --- Step 1: Genre Frequency Analysis ---

# Split genres and count occurrences
genre_df <- df %>%
  separate_rows(genre, sep = ", ") %>% # Split genre column
  group_by(genre) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# Bar Plot of Genre Frequencies
ggplot(genre_df, aes(x = reorder(genre, count), y = count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Genre Frequency Distribution",
       x = "Genre",
       y = "Count") +
  theme_minimal()

# --- Step 2: Releases Over Time Analysis ---

# Ensure 'released' column is numeric
df <- df %>% mutate(released = as.numeric(released))

# Group by release year and count occurrences
release_counts <- df %>%
  filter(released >= 1970 & released <= 2030) %>%  # Filter year range
  group_by(released) %>%
  summarize(count = n())

# Line Plot for Releases Over Years
ggplot(release_counts, aes(x = released, y = count)) +
  geom_line(color = "darkblue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Number of Anime Releases Over Time",
       x = "Year",
       y = "Number of Releases") +
  theme_minimal()

