rm(list = ls())

# PACKAGES ----
library(bundesbank)
library(lubridate)
library(dplyr)
library(tibble)

# SET-UP ----
#_____________________________________________________#
#_specify vintage
#_____________________________________________________#
vintage <- c("2008-01-15")

# Convert vintage to Date
vintage_date <- as.Date(vintage)  

# Compute the last day of the current quarter
last_day_q <- ceiling_date(vintage_date, "quarter") - days(1)

if (vintage_date < last_day_q) {
  # Quarter is not complete
  # Get the quarter number for vintage_date
  q <- quarter(vintage_date)
  if (q == 1) {
    # If vintage is in Q1, previous quarter is Q4 of last year
    cutoff_date <- as.Date(paste0(year(vintage_date) - 1, "-12-01"))
  } else {
    # For other quarters, previous quarter is (q - 1) and its last month is (q - 1)*3
    cutoff_date <- as.Date(paste0(year(vintage_date), "-", sprintf("%02d", (q - 1) * 3), "-01"))
  }
} else {
  # Quarter is complete
  # In this case, use the first day of the vintage month
  cutoff_date <- as.Date(paste0(year(vintage_date), "-", sprintf("%02d", month(vintage_date)), "-01"))
}

# LOAD QUARTERLY ECONOMIC VARIABLES ----
series_codes <- c("BBKRT.Q.DE.Y.A.AG1.CA010.A.I", # Gross Domestic Product (GDP)
                  "BBKRT.Q.DE.Y.A.CA1.BA100.A.I", # Private Consumption Expenditure
                  "BBKRT.Q.DE.Y.A.CD1.CA010.A.I"  # Gross Capital Formation (overall economy)
)

# Mnemonics
series_names <- c("GDP", "Consumption", "Investment")

# Transformations
transform <- c(3, 3, 3)

# Ensure the "data_quarterly" directory exists; if not, create it
if(!dir.exists("data_quarterly")){
  dir.create("data_quarterly")
}

# Loop through each series code and download data
for (i in seq_along(series_codes)) {
  code <- series_codes[i]
  short_name <- series_names[i]
  
  # Get the series
  economic_data <- getSeries(code)
  
  # Assign this data to a new variable with the short name in the global environment
  assign(short_name, economic_data, envir = .GlobalEnv)
  
  # Save to an .Rda file in the "data_quarterly" folder using the short name
  save(list = short_name, file = file.path("data_quarterly", paste0(short_name, ".Rda")))
}

# Loop to load each .Rda file from the "data_quarterly" directory using the short names
for (name in series_names) {
  load(file = file.path("data_quarterly", paste0(name, ".Rda")))
}

series_list <- list()

for (name in series_names) {
  
  # Retrieve the dataframe with all vintages for one series
  series <- get(name)
  
  # Select vintage
  dates_vintages <- as.Date(names(series))
  ind_vintage <- sum(dates_vintages <= vintage)
  series <- series[, ind_vintage, drop = FALSE]
  
  # Convert row names (dates) into a column
  series <- series %>%
    rownames_to_column(var = "date")
  
  # Adjust the date to be the first day of the quarter's last month
  series <- series %>%
    # Convert the date string (e.g. "1991-01") into a Date object (1991-01-01)
    mutate(date = as.Date(paste0(date, "-01"), format = "%Y-%m-%d"),
           # Now adjust the date to be the first day of the quarter's last month
           date = as.Date(paste0(year(date), "-", sprintf("%02d", quarter(date) * 3), "-01"),
                          format = "%Y-%m-%d"))
  
  # Rename the column with the vintage date
  series <- series %>%
    select(date, value = tail(names(.), 1)) %>%
    rename(!!name := value)
  
  # Store the dataframe with the selected vintage in the list
  series_list[[name]] <- series
}

# Create a full quarterly date sequence from 1991-03-01 to the cutoff date
all_dates <- seq.Date(
  from = as.Date("1991-03-01"),
  to = cutoff_date,
  by = "quarter"
)

# Initialize the final dataframe with the full date sequence
final_df <- data.frame(date = all_dates)

# Merge each series into final_df by left joining on the "date" column
for (name in series_names) {
  final_df <- final_df %>% left_join(series_list[[name]], by = "date")
}

# Remove unnecessary objects
rm(list = c(series_names, "series", "series_list", "economic_data") , envir = .GlobalEnv)

# Format the date column as m/d/yyyy
final_df$date <- as.Date(final_df$date, format = "%m/%d/%Y")
final_df$date <- paste0(
  as.integer(format(final_df$date, "%m")), "/",
  as.integer(format(final_df$date, "%d")), "/",
  format(final_df$date, "%Y")
)

# Create the transform row
transform_row <- final_df[1, ]
transform_row[, -1] <- c(as.numeric(transform))
transform_row$date <- "Transform:"

# Insert the transform row at the top of final_df
final_df <- rbind(transform_row, final_df)
final_df[, -1] <- lapply(final_df[, -1], as.numeric)

# Convert vintage to a Date object
vintage_date <- as.Date(vintage)
# Compute the file vintage
file_vintage <- format(vintage_date + days(1), "%Y-%m-%d")
# Write to CSV
file_name <- paste0(file_vintage, ".csv")
write.csv(final_df, file_name, row.names = FALSE)

