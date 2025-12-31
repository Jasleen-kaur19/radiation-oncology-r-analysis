# 📊 Radiation Oncology Research Articles Analysis (R)

## 📌 Project Overview
This project performs **web scraping, data cleaning, and exploratory data analysis** of research articles published in the *Radiation Oncology* journal (BioMed Central). Using **R**, the project collects article metadata across multiple years and analyzes publication trends, authorship patterns, research keywords, and collaboration networks.

The project was completed as part of an **R programming and data analysis course** and demonstrates an end-to-end analytical workflow from data acquisition to visualization.

---

## 🧠 Key Objectives
- Scrape Radiation Oncology research articles by year
- Clean and structure article metadata
- Analyze publication trends over time
- Identify top authors, keywords, and disease-related topics
- Study collaboration and authorship patterns
- Visualize insights using `ggplot2`

---

## 📂 Project Structure

├── Radio_oncology_2025.R # Main R script (scraping + analysis)
├── RadOnc_articles.csv # Scraped and cleaned dataset
├── README.md # Project documentation


---

## 🛠️ Tools & Libraries Used
- **R**
- `rvest`, `httr`, `xml2` – Web scraping
- `dplyr`, `stringr`, `tidyr` – Data manipulation
- `ggplot2` – Data visualization
- `igraph` – Network and co-authorship analysis

---

## 🔍 Data Collected
For each article, the following fields were extracted:
- Article title
- Authors
- Corresponding author
- Corresponding author email
- Publication date
- Abstract
- Keywords
- Year of publication

---

## 📈 Analyses Performed
- Articles published **by month and by year**
- **Top 5 authors** by number of publications
- **Top 5 research keywords**
- Disease-related keyword analysis
- Keyword frequency scatter plots
- Multi-year publication trend analysis
- **Co-authorship network construction**
- Corresponding author email domain analysis
- Topic category trends over time
- Average number of authors per paper (collaboration trend)

---

## ▶️ How to Run the Project

1. Open **RStudio**
2. Set your working directory to the project folder:
   ```r
   setwd("path/to/project-folder")
   ```
3. Install required packages:
   ```r
   install.packages(c("rvest","httr","xml2","dplyr","stringr","ggplot2","igraph","tidyr"))
   ```
4. Run the script:
   ```r
   source("Radio_oncology_2025.R")
   ```
5. Enter a year or year range when prompted (e.g., 2020-2023)
   The script will scrape articles, generate visualizations, and save the dataset as a CSV file.

---

## 📊 Example Outputs

- Bar charts of articles published by month
- Line charts showing publication trends over years
- Top authors and keyword frequency plots
- Keyword trend analysis across years
- Collaboration and authorship trend visualizations

---

## ⚠️ Notes

- Web scraping depends on the website’s structure and may stop working if the site layout changes.
- This project is intended for educational and research purposes only.

## 👩‍💻 Author
Jasleen Kaur
