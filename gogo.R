# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Load the CSV data (replace the file path with the correct one on your system)
df <- read.csv("/path/to/yourfile.csv")

# --- Step 1: Genre Frequency Analysis ---

# Split genres and count occurrences
genre_df <- df %>%
  separate_rows(genre, sep = ", ") %>% # Split genre column by commas
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
