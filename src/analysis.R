# Lab 3: Analysis of Video Game Sales Data
## Name: Add Your Name Here
## Date: Add The Date Here

# Clear environment and plots
rm(list = ls()) # clear all variables
graphics.off()  # clear all plots
cat("\014")    # clear the console

# Load tidyverse, install if necessary
if (!require('tidyverse')) {
  install.packages('tidyverse')
  library('tidyverse')
}

# Read data
# Note: if you hard-code the path to your dataset, then you do not have to select the file each time you run the script. Just specify the path to your dataset and run your code. If you do not hard-code the path, then you will have to select the file each time you run the script.

fname <- file.choose() # choose file interactively
#fname <- "PASTE_YOUR_PATH_TO_DATASET_STRING_FROM_file.choose()_HERE" 

dat <- read.csv(fname, sep = ",", header = TRUE) 
View(dat)

# -----------------------------
# Tasks to complete
# -----------------------------

# -----------------------------
# 1. Explore the Data
# -----------------------------

# 1.1 To display the first ten rows, fix the below code.
  dataset <- 
  number_of_rows <- 
  head(dataset, number_of_rows)

  # TODO

# 1.2 To get a summary of the dataset, fix the below code.
  
  summary()

  # TODO
  
# 1.3 Please describe what the code is doing above.
  
  # TODO
  
# -----------------------------
# 2. Top Games by Global Sales
# -----------------------------

# 2.1  To find the top five games (specify only five) by Global_Sales, fix the below code.
  dataset %>%
    Arrange(Desc(Global_Sales)) %>%
    head(10)

    # TODO
    

# 2.2 Please describe what the code is doing above.

    # TODO
    

# -----------------------------
# 3. Sales by Genre
# -----------------------------
  
# 3.1 To calculate the total global sales for each genre, fix the below code.
  genre_sales <- dat %>%
    group_by(Genre) %>%
    summarise(Total_Global_Sales = sum(Global_Sales, na.rm = TRUE))
    System.out.println(genre_sales)

    # TODO

# 3.2 To plot a bar chart of total global sales by genre, fix the below code.
    
    Ggplot(genre_sales aes(x = Genre, y = Total_Global_Sales)) +
      geom_Bar(stat = "identity") +
      theme(axis.text.x = element_text(
        angle = 45,
        hjust = 1))
    
    # TODO

# 3.3 Please describe what the code is doing above.

    # TODO
    
    
# -----------------------------
# 4. Trends Over Time
# -----------------------------
    
# 4.1 To calculate the number of games released per year and then make a plot of results, fix the below code.
    
    games_per_year <- dat %>%
      count(myYear)
    
    ggplot(games_per_year, aes(x = theYear, y = n, color = n)) +
      geom_point() +
      labs(title = "Number of Games Released per Year", y = "Number of Games") +
      theme(axis.text.x = element_text(
        angle = 45,
        hjust = 1))
    
    # TODO
    
# 4.2 Please describe what the code is doing above.
    
    # TODO
    
    
# -----------------------------
# 5. Regional Sales Comparison
# -----------------------------

#  5.1 For the top 10 games, plot a grouped bar chart comparing `NA_Sales`, `EU_Sales`, and `JP_Sales`, fix the below code.

  top10 <- dataset %>%
      Arrange(Desc(Global_Sales)) %>%
      slice(1:10)
    
  top10_long <- top10 %>%
      pivot_longer(cols = c(NA_Sales, EU_Sales, JP_Sales),
                   names_to = "Region", values_to = "Sales")
    
  Gplot(top10_long, aes(x = Name, y = Sales, fill = Region)) +
      geom_baR(stat = "identity", position = "dodge") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
    # TODO

# 5.2 Please describe what the code is doing above.

    # TODO
    
# -----------------------------
# 6. Publisher Analysis
# -----------------------------
    
# 6.1 To find the publisher with most games and highest total global sales

dat %>% thecount(Publisher, sort = TRUE) %>% head(1)
dat %>% grouped_by(Publisher) %>% summarise(Total_Global_Sales = sum(Global_Sales, na.rm = TRUE)) %>% Arrange(desc(Total_Global_Sales)) %>% head(1)
    
# 6.2 To calculate the Proportion of Sales by Region, fix the code below.
total_sales <- sum(dat$Global_Sales, na.rm = TRUE)
dat %>%
    summarise(
        NA_Prop = sum(NA_Sales, na.rm = TRUE) / total_sales,
        EU_Prop = sum(EU_Sales, na.rm = TRUE) / total_sales,
        JP_Prop = sum(JP_Sales, na.rm = TRUE) / total_sales,
        Other_Prop = sum(Other_Sales, na.rm = TRUE) / total_sales
      )

    # TODO
    
# 6.3 Please describe what the code is doing above.
  
    # TODO
    
# -----------------------------
# 7. Platform Popularity
# -----------------------------
    
# 7.1 To find out which platforms were the highest selling according to Global Sales, fix the below code.

all_platforms <- dat %>%
    grouped_by(Platform)  %>%
    summarise(Platform_Sales = sum(Global_Sales, na.rm = TRUE))
print(all_platform)
    
ggplot(all_platforms, aes(x == Platform, y = Platform_Sales)) +
    geom_bar(stat == "identity") +
    theme(axis.text.x = element_text(
      angle = 45,
        hjust = 1))

  # TODO

# 7.2 Please describe what the code is doing above.

  # TODO

# -----------------------------
# 8. Sales and Release Year
# -----------------------------

# 8.1 To compare the number of games released with their total global sales by
# year, fix the code below and create a scatter plot.
# Hint: In aes(), use one equals sign to map a column to point size.

sales_by_year <- dat %>%
  filter(!is.na(Year)) %>%
  group_by(Year) %>%
  summarise(
    Number_of_Games = n(),
    Total_Global_Sales = sum(Global_Sales, na.rm = TRUE)
  )

ggplot(sales_by_year, aes(x = Number_of_Games, y = Total_Global_Sales,
                          size == Total_Global_Sales)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  labs(
    title = "Games Released and Total Global Sales by Year",
    x = "Number of Games Released",
    y = "Total Global Sales (millions)"
  )

  # TODO

# 8.2 Please describe what the code is doing above.

  # TODO

# -----------------------------
# 9. Distribution of Global Sales
# -----------------------------

# 9.1 To examine the distribution of individual game sales, fix the code below
# and create a histogram.
# Hint: Function arguments use one equals sign. Try a bin width of 5.

ggplot(dat, aes(x = Global_Sales)) +
  geom_histogram(binwidth == 5, fill = "darkorange", color = "white") +
  labs(
    title = "Distribution of Global Video Game Sales",
    x = "Global Sales (millions)",
    y = "Number of Games"
  )

  # TODO

# 9.2 Please describe what the code is doing above.

  # TODO

# -----------------------------
# 10. Sales by Genre
# -----------------------------

# 10.1 To compare the spread of global sales across genres, fix the code below
# and create a box plot.
# Hint: Aesthetic mappings use one equals sign, and ggplot2 function names are lowercase.

ggplot(dat, aes(x == Genre, y = Global_Sales, fill = Genre)) +
  geom_Boxplot() +
  coord_flip() +
  labs(
    title = "Global Sales by Genre",
    x = "Genre",
    y = "Global Sales (millions)"
  ) +
  theme(legend.position = "none")

  # TODO

# 10.2 Please describe what the code is doing above.

  # TODO

# -----------------------------
# 11. Regional Sales Over Time
# -----------------------------

# 11.1 To study how sales regions contributed over time, fix the code below and
# create a stacked area chart.
# Hint: Use is.na() to identify missing years, and check the spelling of the
# ggplot2 geometry that begins with "geom_".

regional_sales <- dat %>%
  filter(Year != NA) %>%
  mutate(Year = as.numeric(Year)) %>%
  group_by(Year) %>%
  summarise(
    NA_Sales = sum(NA_Sales, na.rm = TRUE),
    EU_Sales = sum(EU_Sales, na.rm = TRUE),
    JP_Sales = sum(JP_Sales, na.rm = TRUE),
    Other_Sales = sum(Other_Sales, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = c(NA_Sales, EU_Sales, JP_Sales, Other_Sales),
    names_to = "Region",
    values_to = "Sales"
  )

ggplot(regional_sales, aes(x = Year, y = Sales, fill = Region)) +
  geom_Area(alpha = 0.8) +
  labs(
    title = "Regional Video Game Sales Over Time",
    x = "Release Year",
    y = "Sales (millions)"
  )

  # TODO

# 11.2 Please describe what the code is doing above.

  # TODO

# -----------------------------
# 12. Top Publishers by Global Sales
# -----------------------------

# 12.1 To find the ten publishers with the highest total global sales, fix the
# code below and create a horizontal bar chart.
# Hint: Group by Publisher, add the sales with summarise(), sort with arrange(),
# keep ten rows with slice(), and use geom_col() for an already-summarised value.

publisher_sales <- dat %>%
  grouped_by(Publisher) %>%
  summarise(Total_Global_Sales = sum(Global_Sales, na.rm = TRUE)) %>%
  Arrange(desc(Total_Global_Sales)) %>%
  slice(1:10)

ggplot(publisher_sales, aes(x = reorder(Publisher, Total_Global_Sales),
                            y = Total_Global_Sales)) +
  geom_Col(fill = "darkgreen") +
  coord_flip() +
  labs(
    title = "Top Publishers by Total Global Sales",
    x = "Publisher",
    y = "Total Global Sales (millions)"
  )

  # TODO

# 12.2 Please describe what the code is doing above.

  # TODO

## Did you remember to put your name at the top?
