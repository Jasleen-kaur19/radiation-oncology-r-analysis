#### Radiation Oncology Scraper & Analysis:

#---------------------------------------------------------------------------

### 1. INSTALL & LOAD PACKAGES
install.packages(c("rvest","httr","xml2","dplyr","stringr","ggplot2","igraph"))
library(rvest)        # for web scraping
library(httr)         # for HTTP requests
library(xml2)         # HTML parsing
library(dplyr)        # data manipulation
library(stringr)     # string helpers
library(ggplot2)      # plotting
library(igraph)       # network graphs

#---------------------------------------------------------------------------

### Section 2a: Count Pages for a Volume

get_npages <- function(volume) {
  # If volume < 1, it’s not valid for this journal
  if (volume < 1) {
    message("  • Volume ", volume, " is invalid (skipping).")
    return(0)
  }
  
  url <- paste0(
    "https://ro-journal.biomedcentral.com/articles?tab=keyword",
    "&searchType=journalSearch&sort=PubDate&volume=", volume, "&page=1"
  )
  page <- read_html(url)
  
  # Grab the total‐entries node; if it's missing, return 0
  nodes <- page %>% html_nodes(".c-listing-title strong")
  if (length(nodes) == 0) {
    return(0)
  }
  
  entries <- as.integer(nodes %>% html_text())
  entries <- ifelse(is.na(entries), 0, entries)
  
  # Compute pages
  pages <- ceiling(entries / 50)
  return(pages)
}

### Section 2b: Scrape All Articles in One Year

scrape_year <- function(year) {
  volume <- year - 2005
  npages <- get_npages(volume)
  
  # If no pages, bail out early with an empty template
  if (npages == 0) {
    message("  → No articles found for year ", year, ".\n")
    return(data.frame(
      name                         = character(),
      authors                      = character(),
      corresponding_authors        = character(),
      corresponding_authors_email  = character(),
      published_date               = character(),
      abstract                     = character(),
      keywords                     = character(),
      stringsAsFactors             = FALSE
    ))
  }
  
  message("  → Found ", npages, " page(s) for year ", year, "\n")
  all_articles <- data.frame(
    name                         = character(),
    authors                      = character(),
    corresponding_authors        = character(),
    corresponding_authors_email  = character(),
    published_date               = character(),
    abstract                     = character(),
    keywords                     = character(),
    stringsAsFactors             = FALSE
  )
  
  for (i in seq_len(npages)) {
    message("    Scraping year ", year, ", page ", i, " of ", npages, " …")
    page_url <- paste0(
      "https://ro-journal.biomedcentral.com/articles?tab=keyword",
      "&searchType=journalSearch&sort=PubDate",
      "&volume=", volume,
      "&page=", i
    )
    page <- read_html(page_url)
    
    titles   <- page %>% html_nodes(".c-listing__title a")         %>% html_text()
    dates    <- page %>% html_nodes(".c-listing__metadata span+ span") %>% html_text()
    authors  <- page %>% html_nodes(".c-listing__authors-list")    %>% html_text()
    links    <- page %>% html_nodes(".c-listing__title a")         %>% html_attr("href")
    links    <- paste0("https://ro-journal.biomedcentral.com", links)
    
    abstracts <- sapply(links, function(link) {
      read_html(link) %>%
        html_node("#Abs1-content") %>%
        html_text() %>%
        paste(collapse = ",")
    })
    keywords <- sapply(links, function(link) {
      read_html(link) %>%
        html_node(".c-article-subject-list") %>%
        html_nodes("a") %>%
        html_text() %>%
        paste(collapse = ", ")
    })
    corresp <- sapply(links, function(link) {
      read_html(link) %>% html_node("#corresp-c1") %>% html_text()
    })
    emails <- sapply(links, function(link) {
      read_html(link) %>%
        html_node("#corresp-c1") %>%
        html_attr("href") %>%
        str_remove("^mailto:")
    })
    
    page_df <- data.frame(
      name                         = titles,
      authors                      = authors,
      corresponding_authors        = corresp,
      corresponding_authors_email  = emails,
      published_date               = str_remove(dates, "Published on: "),
      abstract                     = abstracts,
      keywords                     = keywords,
      stringsAsFactors             = FALSE
    )
    
    all_articles <- bind_rows(all_articles, page_df)
  }
  
  return(all_articles)
}

### Section 2c: Clean Data by Dropping NAs

clean_data <- function(df) {
  # na.omit() removes any row with one or more NA values
  df_clean <- na.omit(df)
  return(df_clean)
}

#---------------------------------------------------------------------------

### Section 3: Main — Prompt, Scrape, and Save

if (interactive()) {
  # 3a. Ask for year or range
  repeat {
    years_input <- readline(
      prompt = "Enter a year (e.g. 2023) or range (e.g. 2020-2023): "
    )
    years_input <- trimws(years_input)
    
    # If form is "YYYY-YYYY"
    if (grepl("^\\d{4}\\s*-\\s*\\d{4}$", years_input)) {
      parts <- as.integer(strsplit(years_input, "\\s*-\\s*")[[1]])
      if (any(is.na(parts))) {
        cat("  ✗ Invalid range. Try again (e.g. 2020-2023).\n")
        next
      }
      years <- seq(parts[1], parts[2])
      break
      
      # If form is single "YYYY"
    } else if (grepl("^\\d{4}$", years_input)) {
      year <- as.integer(years_input)
      if (is.na(year)) {
        cat("  ✗ Invalid year. Try again (e.g. 2023).\n")
        next
      }
      years <- year
      break
      
      # If Neither matched then retry
    } else {
      cat("  ✗ Unrecognized input. Please type 4 digits or a range like 2020-2023.\n")
    }
  }
  
  cat("Okay! Scraping for year(s):", paste(years, collapse = ", "), "\n\n")
  
  # 3b: Build RadOnc safely
  
  RadOnc <- data.frame()  # start empty
  
  for (y in years) {
    raw_df   <- scrape_year(y)
    clean_df <- clean_data(raw_df)
    
    # If nothing was scraped, skip to next year
    if (nrow(clean_df) == 0) {
      message("No articles for year ", y, ". Skipping.\n")
      next
    }
    
    # Safe to tag Year
    clean_df$Year <- y
    
    # Append
    RadOnc <- bind_rows(RadOnc, clean_df)
  }
  
  
  # 3c: Inspect and persist the final dataset
  View(RadOnc)  # It opens a spreadsheet‑like view
  write.csv(RadOnc, "~/Documents/NJIT sems DOCS/sem3 R/R labs/RadOnc_articles.csv", row.names = FALSE)
  message("All done! Your data is saved to RadOnc_articles.csv")
}

#---------------------------------------------------------------------------

### Section 4a: Articles by Month

# 1. Turn the scraped “Published on: DD MMMM YYYY” strings into Date objects
dates <- as.Date(RadOnc$published_date, format = "%d %B %Y")

# 2. Count how many articles appeared in each month
month_counts <- table(format(dates, "%B"))

# 3. Make sure months go January → December
months_ordered <- factor(names(month_counts), levels = month.name)

# 4. Plot a simple bar chart
library(ggplot2)
ggplot(
  data.frame(month = months_ordered, count = as.numeric(month_counts)),
  aes(x = month, y = count)
) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = "Articles Published by Month",
    x     = "Month",
    y     = "Number of Articles"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#By Yearly Month

library(dplyr)
library(ggplot2)

# Prepare a Year–Month summarised table
month_year_counts <- RadOnc %>%
  mutate(
    Date  = as.Date(published_date, "%d %B %Y"),
    Month = format(Date, "%B")
  ) %>%
  group_by(Year, Month = factor(Month, levels = month.name)) %>%
  summarise(Count = n(), .groups = "drop")

# Plot with one small chart per year
ggplot(month_year_counts, aes(x = Month, y = Count)) +
  geom_col(fill = "skyblue") +
  facet_wrap(~ Year, ncol = 2) +
  labs(
    title = "Monthly Article Counts by Year",
    x     = "Month",
    y     = "Count"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(fill = "lightgray")
  )

### Section 4b: Top 5 Authors

# 1. Split each paper’s author string into individual names
all_authors <- unlist(strsplit(RadOnc$authors, ",\\s*"))

# 2. Count how often each author appears
author_counts <- table(all_authors)

# 3. Turn into a data frame and sort descending
author_df <- as.data.frame(author_counts, stringsAsFactors = FALSE)
colnames(author_df) <- c("author", "count")
top5_authors <- author_df[order(author_df$count, decreasing = TRUE), ][1:5, ]

# 4. Plot a horizontal bar chart of the Top 5
library(ggplot2)
ggplot(top5_authors, aes(x = count, y = reorder(author, count))) +
  geom_col(fill = "salmon") +
  labs(
    title = "Top 5 Authors by Number of Articles",
    x     = "Number of Articles",
    y     = "Author"
  ) +
  theme_minimal()

### Section 4c: Top 5 Keywords

# 1. Split each paper’s comma‑separated keywords into individual terms
all_keywords <- unlist(strsplit(RadOnc$keywords, ",\\s*"))

# 2. Count how often each keyword appears
keyword_counts <- table(all_keywords)

# 3. Convert to data frame and sort in descending order
keyword_df <- as.data.frame(keyword_counts, stringsAsFactors = FALSE)
colnames(keyword_df) <- c("keyword", "count")
top5_keywords <- keyword_df[order(keyword_df$count, decreasing = TRUE), ][1:5, ]

# 4. Plot a horizontal bar chart of the Top 5 keywords
library(ggplot2)
ggplot(top5_keywords, aes(x = count, y = reorder(keyword, count))) +
  geom_col(fill = "lightgreen") +
  labs(
    title = "Top 5 Keywords by Frequency",
    x     = "Number of Occurrences",
    y     = "Keyword"
  ) +
  theme_minimal()

### Section 4d: Top 5 Disease‑Related Keywords

# 1. Define a small list of disease indicators
disease_terms <- c(
  "cancer", "carcinoma", "tumor", "tumour",
  "leukemia", "lymphoma", "sarcoma",
  "melanoma", "glioma", "neoplasm"
)

# 2. Filter your full keyword list to only those containing any disease term
disease_keywords <- all_keywords[
  grepl(
    paste(disease_terms, collapse = "|"),
    tolower(all_keywords)
  )
]

# 3. Count frequencies and pick the Top 5
disease_counts <- sort(table(disease_keywords), decreasing = TRUE)
top5_diseases  <- head(disease_counts, 5)

# 4. Plot the results
library(ggplot2)

disease_df <- data.frame(
  keyword = names(top5_diseases),
  count   = as.numeric(top5_diseases),
  stringsAsFactors = FALSE
)

ggplot(disease_df, aes(x = count, y = reorder(keyword, count))) +
  geom_col(fill = "orchid") +
  labs(
    title = "Top 5 Disease‑Related Keywords",
    x     = "Number of Occurrences",
    y     = "Keyword"
  ) +
  theme_minimal()

### Section 4e: Scatter Plot of Top 30 Keywords

# 1. Re‑use the keyword frequency data frame from Section 4c

# 2. Select the top 30 most frequent keywords
top30_keywords <- head(
  keyword_df[order(keyword_df$count, decreasing = TRUE), ],
  30
)

# 3. Plot a scatter: x = count, y = keyword
library(ggplot2)
ggplot(top30_keywords, aes(x = count, y = reorder(keyword, count))) +
  geom_point(size = 3) +
  labs(
    title = "Top 30 Keywords Scatter Plot",
    x     = "Number of Occurrences",
    y     = "Keyword"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),  # shrink long labels if needed
    plot.title  = element_text(hjust = 0.5)
  )

#---------------------------------------------------------------------------

### Section 5: Multi‑Year Article Trend

# 1. Count how many articles we have in each Year
#    RadOnc$Year was tagged in Section 3
year_counts <- table(RadOnc$Year)

# 2. Turn that into a small data frame for plotting
year_df <- as.data.frame(year_counts, stringsAsFactors = FALSE)
colnames(year_df) <- c("Year", "Count")

# 3. Convert Year from character to numeric so it plots on a proper axis
year_df$Year <- as.integer(year_df$Year)

# 4. Draw a simple line + point chart of articles per year
library(ggplot2)
ggplot(year_df, aes(x = Year, y = Count)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 3) +
  labs(
    title = "Number of Articles Published per Year",
    x     = "Year",
    y     = "Number of Articles"
  ) +
  theme_minimal()

#---------------------------------------------------------------------------

### Section 6: Co‑Authorship Network

# 1. Split each paper’s authors into individual names
authors_list <- strsplit(RadOnc$authors, ",\\s*")

# 2. Build an “edge list” of all co‑author pairs per paper
edge_pairs <- do.call(rbind, lapply(authors_list, function(a) {
  if (length(a) > 1) {
    t(combn(a, 2))
  }
}))

# 3. Put those pairs into a data frame
edges_df <- data.frame(
  from = edge_pairs[,1],
  to   = edge_pairs[,2],
  stringsAsFactors = FALSE
)

#---------------------------------------------------------------------------

### Section 7: Top 10 Email Domains

# 1. Extract domains
domains <- sub(".*@", "", RadOnc$corresponding_authors_email)

# 2. Tally counts into a data frame
domain_df <- as.data.frame(table(domains), stringsAsFactors = FALSE)
colnames(domain_df) <- c("domain", "count")

# 3. Take the top 10 by count
top10_domains <- domain_df[order(domain_df$count, decreasing = TRUE), ][1:10, ]

# 4. Plot with ggplot, flipping coordinates for long labels
library(ggplot2)
ggplot(top10_domains, aes(x = reorder(domain, count), y = count)) +
  geom_col(fill = "steelblue") +
  coord_flip() + 
  labs(
    title = "Top 10 Corresponding Author Email Domains",
    x     = "Email Domain",
    y     = "Number of Articles"
  ) +
  theme_minimal()

#---------------------------------------------------------------------------

### Section 8: Trend of Top 5 Keywords Over Years

library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Split each row’s comma‑separated keywords into one keyword per row
kw_df <- RadOnc %>%
  mutate(keywords = strsplit(keywords, ",\\s*")) %>%
  unnest(keywords)

# 2. Identify the top 5 keywords overall by total count
top5 <- kw_df %>%
  dplyr::count(keywords, sort = TRUE) %>%
  slice_max(n, n = 5) %>%
  pull(keywords)

# 3. For only those top 5, count occurrences per Year
kw_trends <- kw_df %>%
  filter(keywords %in% top5) %>%
  dplyr::count(Year, keywords)

# 4. Plot a multi‑line chart: Year on x‑axis, frequency on y, one line per keyword
ggplot(kw_trends, aes(x = Year, y = n, color = keywords)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = sort(unique(kw_trends$Year))) +
  labs(
    title    = "Yearly Trends of Top 5 Research Keywords",
    x        = "Year",
    y        = "Number of Articles",
    color    = "Keyword"
  ) +
  theme_minimal()

#---------------------------------------------------------------------------

### Section 9: Collaboration Trend (Avg Authors/Paper)

library(dplyr)
library(ggplot2)

# 1. Compute number of authors on each paper
RadOnc <- RadOnc %>%
  mutate(
    num_authors = lengths(strsplit(authors, ",\\s*"))
  )

# 2. Average that by year
auth_trend <- RadOnc %>%
  group_by(Year) %>%
  summarise(avg_authors = mean(num_authors), .groups="drop")

# 3. Plot the trend
ggplot(auth_trend, aes(x = Year, y = avg_authors)) +
  geom_line(color = "darkorange", size = 1) +
  geom_point(color = "darkorange", size = 3) +
  scale_x_continuous(breaks = auth_trend$Year) +
  labs(
    title = "Average Number of Authors per Paper Over Time",
    x     = "Year",
    y     = "Avg. # of Authors"
  ) +
  theme_minimal()

#---------------------------------------------------------------------------

### Section 10: Topic Category Share Over Time

library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Unnest keywords so one row per (paper × keyword)
kw_df <- RadOnc %>%
  mutate(keywords = strsplit(keywords, ",\\s*")) %>%
  unnest(keywords)

# 2. Categorize each keyword into Planning, Disease, Treatment, or Other
kw_df <- kw_df %>%
  mutate(
    category = case_when(
      grepl("radiotherapy|therapy", keywords, ignore.case = TRUE) ~ "Treatment",
      grepl("cancer|tumor|carcinoma|sarcoma|lymphoma", keywords, ignore.case = TRUE) ~ "Disease",
      grepl("volume", keywords, ignore.case = TRUE) ~ "Planning",
      TRUE ~ "Other"
    )
  )

# 3. Count how many keywords in each category by year
cat_trend <- kw_df %>%
  group_by(Year, category) %>%
  summarise(n = n(), .groups = "drop")

# 4. Convert to proportions per year
cat_trend <- cat_trend %>%
  group_by(Year) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

# 5. Plot a stacked area chart of category share over time
ggplot(cat_trend, aes(x = Year, y = prop, fill = category)) +
  geom_area(alpha = 0.8) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  labs(
    title = "Share of Research Topics Over Time",
    x     = "Year",
    y     = "Percentage of Keywords",
    fill  = "Topic Category"
  ) +
  theme_minimal()
