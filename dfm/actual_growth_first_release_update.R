# This script constructs three series of first-release annualized quarterly 
# GDP/Consumption/Investment growth rates using real-time vintages. 
# It identifies the earliest available non-missing values for each quarter and 
# calculates the corresponding growth rates.

library(dplyr)
library(tidyr)
library(lubridate)
library(tibble)  

# Function to get the first non-NA value after the given date
get_first_value <- function(row, data_obj) {
  # Extract dates from the column names of data_obj
  vintage_dates <- as.Date(names(data_obj), format = "%Y-%m-%d")
  
  # Only consider vintages after the current row date
  relevant_vintages <- vintage_dates[vintage_dates > as.Date(row["date"])]
  
  # If there are no relevant vintages, return NA
  if (length(relevant_vintages) == 0) {
    return(NA)
  }
  
  # Extract the earliest available non-NA value after the given date
  earliest_value <- row[match(as.character(relevant_vintages), names(data_obj)) + 1] # +1 to adjust for the "date" column
  return(earliest_value[!is.na(earliest_value)][1])
}

# Helper function to get the end date of the previous quarter
previous_quarter_end <- function(date) {
  date <- as.Date(date)
  month <- as.numeric(format(date, "%m"))
  year <- as.numeric(format(date, "%Y"))
  
  # Determine previous quarter
  if (month <= 3) {
    month <- 12
    day <- 31
    year <- year - 1
  } else if (month <= 6) {
    month <- 3
    day <- 31
  } else if (month <= 9) {
    month <- 6
    day <- 30
  } else {
    month <- 9
    day <- 30
  }
  
  return(as.Date(paste(year, sprintf("%02d", month), sprintf("%02d", day), sep = "-")))
}

# Function to get lag value from the same vintage
get_lag <- function(row, data_obj, data_fr) {
  # Extract dates from the column names of data_obj
  vintage_dates <- as.Date(names(data_obj), format = "%Y-%m-%d")
  
  # For the very first date, return NA for the lag
  if(row["date"] == min(data_fr$date)) {
    return(NA)
  }
  
  # For dates before "2005-06-30", return the lag from the first vintage
  if(as.Date(row["date"]) < as.Date("2005-06-30")) {
    prev_row_date <- previous_quarter_end(row["date"])
    prev_row <- data_fr[data_fr$date == prev_row_date, ]
    return(prev_row[2]) # 2nd column (1st vintage) because 1st column is "date"
  }
  
  prev_row_date <- previous_quarter_end(row["date"])
  prev_row <- data_fr[data_fr$date == prev_row_date, ]
  
  # Only consider vintages after the current row date
  relevant_vintages <- vintage_dates[vintage_dates > as.Date(prev_row[["date"]])]
  
  # If there are no relevant vintages, return NA
  if (length(relevant_vintages) == 0) {
    return(NA)
  }
  
  # Extract the second available non-NA value after the given date
  second_value <- prev_row[match(as.character(relevant_vintages), names(data_obj)) + 1] # +1 to adjust for the "date" column
  return(second_value[!is.na(second_value)][2])
}

extract_growth_series <- function(var_name, start_date = "1991-06-30", end_date = "2018-12-31") {
  # Construct file path based on var_name
  file_path <- paste0("../data/data_quarterly_update/", var_name, ".Rda")
  load(file_path)   # This loads an object with the name var_name (e.g., GDP)
  data_obj <- get(var_name)  # Retrieve the loaded object
  
  # Create an object for the first-release data 
  data_obj %>% 
    rownames_to_column(var = "date") %>%
    mutate(year = as.numeric(substr(date, 1, 4)), 
           month = as.numeric(substr(date, 6, 7)) + 3, 
           date_tmp = make_date(year = ifelse(month > 12, year + 1, year), month = ifelse(month > 12, 1, month), day = 1),
           date = date_tmp - days(1)) %>%
    select(-date_tmp, -year, -month) -> data_fr

  # Extract the first available non-NA value for each row (first release)
  data_fr[[tolower(var_name)]] <- apply(data_fr, 1, get_first_value, data_obj = data_obj)
  
  # Extract the lag value for each row
  data_fr[[paste0(tolower(var_name), "_lag")]] <- apply(data_fr, 1, get_lag, data_obj = data_obj, data_fr = data_fr)
  
  # Select only the date, variable(t) and variable(t-1)
  data_fr <- data_fr[,c("date", tolower(var_name), paste0(tolower(var_name), "_lag"))] 
  
  data_fr <- data_fr %>% 
    filter(date < as.Date("2019-01-01"))

  # Convert values to numeric in case they are not
  data_fr[[tolower(var_name)]] <- as.numeric(as.character(data_fr[[tolower(var_name)]]))
  data_fr[[paste0(tolower(var_name), "_lag")]] <- as.numeric(as.character(data_fr[[paste0(tolower(var_name), "_lag")]]))
  
  # Calculate annualized quarterly growth rate
  data_fr <- data_fr %>%
    mutate(growth = ifelse(is.na(get(tolower(var_name))) | is.na(get(paste0(tolower(var_name), "_lag"))),
                           NA,
                           400 * (log(get(tolower(var_name))) - log(get(paste0(tolower(var_name), "_lag"))))))
  
  # Select only the date and growth rate, filter by date range, and format the date as "YYYY-MM"
  data_fr <- data_fr %>%
    select(date, growth) %>%
    filter(date >= as.Date(start_date) & date <= as.Date(end_date)) %>%
    mutate(date = format(date, "%Y-%m"))
  
  return(data_fr)
}


# Variable names
vars <- c("GDP", "Consumption", "Investment")

for (var in vars) {
  first_release <- extract_growth_series(var)
  file_name <- paste0(var, "_growth_actual_update.csv")
  write.csv(first_release, file_name, row.names = FALSE)
}