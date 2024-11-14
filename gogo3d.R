# Load required libraries
library(dplyr)
library(tidyr)
library(plotly)

# Load your actual CSV data (replace the path with the correct one)
df <- read.csv("/path/tp/yourfile.csv")

# --- Debugging: Check if the data is loaded correctly
print(df)

# --- Step 1: Genre Frequency Analysis in 3D ---
# Split genres and check the result
genre_df <- df %>%
  separate_rows(genre, sep = ", ") %>%  # Split genre column
  group_by(genre) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# Debugging: Print the genre_df to inspect the data
print(genre_df)

# 3D Mesh Plot of Genre Frequencies
fig1 <- plot_ly(
  data = genre_df,
  x = ~genre,
  y = ~count,
  z = ~count,
  type = "mesh3d",  # Use mesh3d for 3D plotting
  marker = list(color = 'rgba(30, 144, 255, 0.8)', line = list(color = 'rgb(0,0,0)', width = 1)),
  name = "Genre Frequency"
) %>%
  layout(
    title = list(text = "3D Genre Frequency Distribution", font = list(size = 20, color = "darkblue")),
    scene = list(
      xaxis = list(title = list(text = "Genre", font = list(size = 14, color = "darkblue")), tickangle = 45),
      yaxis = list(title = list(text = "Count", font = list(size = 14, color = "darkblue"))),
      zaxis = list(title = list(text = "Frequency", font = list(size = 14, color = "darkblue"))),
      camera = list(eye = list(x = 1.7, y = 1.7, z = 1.7)),
      annotations = list(
        list(text = "Top Genres", x = 0.5, y = 1.1, xref = "paper", yref = "paper", showarrow = FALSE)
      )
    ),
    margin = list(l = 80, r = 80, b = 80, t = 80)
  )

# Display the first plot (3D Mesh Plot)
print(fig1)

# --- Step 2: Releases Over Time Analysis in 3D ---
# Ensure 'released' column is numeric
df <- df %>% mutate(released = as.numeric(released))

# Group by release year and count occurrences
release_counts <- df %>%
  filter(released >= 1970 & released <= 2030) %>%  # Filter year range
  group_by(released) %>%
  summarize(count = n())

# 3D Scatter Plot for Releases Over Years with Enhanced Aesthetics
fig2 <- plot_ly(
  data = release_counts,
  x = ~released,
  y = ~count,
  z = ~count,
  type = "scatter3d",
  mode = "markers+lines",
  marker = list(color = 'rgba(255, 69, 0, 0.9)', size = 6, symbol = 'circle'),
  line = list(color = 'rgb(255, 165, 0)', width = 2)
) %>%
  layout(
    title = list(text = "Number of Anime Releases Over Time", font = list(size = 20, color = "darkorange")),
    scene = list(
      xaxis = list(title = list(text = "Year", font = list(size = 14, color = "darkorange")), gridcolor = "rgb(200,200,200)"),
      yaxis = list(title = list(text = "Number of Releases", font = list(size = 14, color = "darkorange")), gridcolor = "rgb(200,200,200)"),
      zaxis = list(title = list(text = "Count", font = list(size = 14, color = "darkorange")), gridcolor = "rgb(200,200,200)"),
      camera = list(eye = list(x = 1.5, y = 1.5, z = 1.8)),
      annotations = list(
        list(text = "Release Trends", x = 0.9, y = 1.1, xref = "paper", yref = "paper", showarrow = FALSE)
      )
    ),
    margin = list(l = 80, r = 80, b = 80, t = 80)
  )

# Display the second plot (3D Scatter Plot)
print(fig2)

