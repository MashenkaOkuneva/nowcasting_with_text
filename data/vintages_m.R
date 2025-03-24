rm(list = ls())

# PACKAGES ----
library(bundesbank)
library(lubridate)
library(dplyr)
library(tibble)
library(readxl)
library(parallel)

# THE MAIN FUNCTION ----

prepare_vintage <- function(vintage) {
  #_____________________________________________________#
  # vintage: when the forecast is produced
  #_____________________________________________________#
  
  # Convert vintage to Date
  vintage_date <- as.Date(vintage)  
  
  # Compute the last day of the current month
  last_day_m <- ceiling_date(vintage_date, "month") - days(1)
  
  if (vintage_date < last_day_m) {
    # Month is not complete
    # Get the month number for vintage_date
    m <- month(vintage_date)
    if (m == 1) {
      # If vintage is in M1, previous month is M12 of last year
      cutoff_date <- as.Date(paste0(year(vintage_date) - 1, "-12-01"))
    } else {
      # For other months, previous month is (m - 1)
      cutoff_date <- as.Date(paste0(year(vintage_date), "-", sprintf("%02d", m - 1), "-01"))
    }
  } else {
    # Month is complete
    # In this case, use the first day of the vintage month
    cutoff_date <- as.Date(paste0(year(vintage_date), "-", sprintf("%02d", month(vintage_date)), "-01"))
  }
  
  # LOAD MONTHLY ECONOMIC VARIABLES ----
  
  # Mnemonics
  series_names <- c("ConstrProd", "IP", "ConstrNO", "INO", 
                    "ConstrTurn", "ITurn", "RetTurn", "CPI", "CPIEN",
                    "PPI", "PPIEN", "EPI", "IPI", "HW", "ConstrHW", "Empl",
                    "GWMan", "GWConstr")
  
  # Transformations
  transform <- c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3)
  
  # Loop to load each .Rda file from the "data_monthly" directory using the short names
  for (name in series_names) {
    load(file = file.path("data_monthly", paste0(name, ".Rda")))
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
    
    # Adjust the date to be the first day of the month
    series <- series %>% 
      mutate(date = as.Date(paste0(date, "-01"), format = "%Y-%m-%d"))
    
    # Rename the column with the vintage date
    series <- series %>%
      select(date, value = tail(names(.), 1)) %>%
      rename(!!name := value)
    
    # Store the dataframe with the selected vintage in the list
    series_list[[name]] <- series
  }
  
  # Create a full monthly date sequence from 1991-01-01 to the cutoff date
  all_dates <- seq.Date(
    from = as.Date("1991-01-01"),
    to = cutoff_date,
    by = "month"
  )
  
  # Initialize the final dataframe with the full date sequence
  final_df <- data.frame(date = all_dates)
  
  # Merge each series into final_df by left joining on the "date" column
  for (name in series_names) {
    final_df <- final_df %>% left_join(series_list[[name]], by = "date")
  }
  
  # Remove unnecessary objects
  rm(list = c(series_names, "series", "series_list") , envir = .GlobalEnv)
  
  # LOAD MONTHLY FINANCIAL VARIABLES ----
  # The CDAX performance index (code: CDAXGEN) was downloaded from LSEG Datastream
  CDAX <- read_excel(path = file.path("data_monthly", "CDAX.xlsx"),
                     col_types = c("date", "numeric"))
  
  # Ensure the date column is a Date object
  CDAX$date <- as.Date(CDAX$date, format = "%Y-%m-%d")
  
  CDAX <- CDAX %>%
    # 1. Construct 'year', 'month', and 'day' columns
    mutate(
      year = year(date),
      month = month(date),
      day = day(date)
    ) %>%
    # 2. Filter data to be <= vintage
    filter(date <= vintage_date) %>%
    # 3. Arrange columns
    select(date, year, month, day, CDAX)
  
  # Extend series with NA to 7-day week 
  dates_tmp <- data.frame(date = seq(min(CDAX$date), 
                                     vintage_date, 
                                     by = "days")
  )
  
  dates_tmp %>% 
    mutate(year = year(date),
           month = month(date),
           day = day(date)) %>%
    merge(CDAX, by = c("date", "year", "month", "day"), all.x = T) -> CDAX
  
  # Get rid of dates_tmp
  rm(dates_tmp)
  
  # Aggregate daily data to monthly frequency
  CDAX <- CDAX %>%
    group_by(year, month) %>%
    summarize(
      # Keep the last value for the index at the end of each month
      CDAX = ifelse(all(is.na(CDAX)), NA, last(na.omit(CDAX)))
    ) %>%
    ungroup() %>%  # Remove grouping to ensure continuity across the whole dataset
    arrange(year, month) %>%  # Arrange data to ensure proper order
    mutate(
      # Create a date column corresponding to the first day of the month
      date = as.Date(paste(year, month, "01"), format = "%Y %m %d"),
    ) %>%
    select(date, CDAX)
  
  # Merge CDAX dataframe with final_df by left joining on the "date" column
  final_df <- final_df %>% left_join(CDAX, by = "date")
  
  # Remove CDAX dataframe
  rm(CDAX, envir = .GlobalEnv)
  
  # Mnemonics
  series_names_financial <- c("Bond1", "Bond5", "Bond10", "EERN", 
                              "EERB", "TotalDebt", "BankDebt", "CorpDebt", "PublicDebt")
  
  # Transformations
  transform_financial <- c(3, 2, 2, 2, 3, 3, 2, 2, 2, 2)
  
  # Loop to load each .Rda file from the "data_monthly" directory using the short names
  for (name in series_names_financial) {
    load(file = file.path("data_monthly", paste0(name, ".Rda")))
  }
  
  series_list <- list()
  
  for (name in series_names_financial) {
    
    # Retrieve the dataframe with one series
    series <- get(name)
    
    # Adjust the date to be the first day of the month
    series <- series %>% 
      mutate(date = as.Date(paste0(date, "-01"), format = "%Y-%m-%d"))%>% 
      filter(date <= cutoff_date, date >= as.Date("1991-01-01"))
    
    # Store the dataframe in the list
    series_list[[name]] <- series
  }
  
  # Merge each series into final_df by left joining on the "date" column
  for (name in series_names_financial) {
    final_df <- final_df %>% left_join(series_list[[name]], by = "date")
  }
  
  # Remove unnecessary objects
  rm(list = c(series_names_financial, "series", "series_list") , envir = .GlobalEnv)
  
  # LOAD SURVEYS ----
  # The survey data from ifo, GfK, and European Commission
  surveys <- read_excel(path = file.path("data_monthly", "Surveys.xlsx"),
                        col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "date"),
                        na = c("NA", ""))
  
  # Convert the date columns to Date objects
  surveys$date <- as.Date(paste0("01/", surveys$date), format = "%d/%m/%Y")
  surveys$pub_date_ESI <- as.Date(surveys$pub_date_ESI, format = "%d-%m-%Y")
  
  # For each monthly ESI value, if its publication date is later than vintage_date,
  # then set its value to NA
  surveys <- surveys %>%
    mutate(ESI = ifelse(is.na(pub_date_ESI) | pub_date_ESI <= vintage_date, ESI, NA))
  
  # Use short variable names
  surveys <- surveys %>%
    rename(
      ifoIndTradeClimate = `ifo: industry and trade, climate`,
      ifoIndTradeCurrent = `ifo: industry and trade, current situation`,
      ifoIndTradeExp     = `ifo: industry and trade, expectations`,
      GfKBCE             = `GfK: business cycle expectations`,
      GfKIE              = `GfK: income expectations`,
      GfKWtB             = `GfK: willingness-to-buy`,
      GfKCCI             = `GfK: consumer climate indicator`
    )
  
  # Remove the column pub_date_ESI
  surveys <- surveys %>%
    select(-pub_date_ESI)
  
  # Left join final_df with surveys by "date"
  final_df <- final_df %>%
    left_join(surveys, by = "date")
  
  # Remove unnecessary objects
  rm(surveys , envir = .GlobalEnv)
  
  # Transformations
  transform_surveys <- c(2, 2, 2, 2, 2, 2, 2, 2)
  
  # Format the date column as m/d/yyyy
  final_df$date <- as.Date(final_df$date, format = "%m/%d/%Y")
  final_df$date <- paste0(
    as.integer(format(final_df$date, "%m")), "/",
    as.integer(format(final_df$date, "%d")), "/",
    format(final_df$date, "%Y")
  )
  
  # Create the transform row
  transform_row <- final_df[1, ]
  transform_row[, -1] <- c(as.numeric(transform), as.numeric(transform_financial), as.numeric(transform_surveys))
  transform_row$date <- "Transform:"
  
  # Insert the transform row at the top of final_df
  final_df <- rbind(transform_row, final_df)
  final_df[, -1] <- lapply(final_df[, -1], as.numeric)
  
  # Convert vintage to a Date object
  vintage_date <- as.Date(vintage)
  # Compute the file vintage
  file_vintage <- format(vintage_date + days(1), "%Y-%m-%d")
  # Write to CSV
  file_name <- paste0("./vintages_monthly/", file_vintage, ".csv")
  write.csv(final_df, file_name, row.names = FALSE)
}

# GENERATE VINTAGES ----

# Define start and end dates (as strings)
start_date <- "2007-12-31"
end_date   <- "2018-12-31"

# Convert them to Date objects
start_date <- as.Date(start_date)
end_date   <- as.Date(end_date)

# Generate a sequence of the first day of each month between start and end
month_seq <- seq(from = floor_date(start_date, "month"),
                 to   = floor_date(end_date, "month"),
                 by   = "month")

# For each month, get the last day
last_day_seq <- ceiling_date(month_seq, "month") - days(1)

# For each month after the first, get the 15th day
mid_month_seq <- as.Date(paste0(format(month_seq[-1], "%Y-%m"), "-15"))

# Combine the dates:
# First element is December's last day,
# then for each subsequent month, we add the 15th and the last day.
vintages <- c(last_day_seq[1],
              as.vector(rbind(mid_month_seq, last_day_seq[-1])))

# Format the dates as strings
vintages <- format(vintages, "%Y-%m-%d")

run_prepare_vintage <- function(i) {
  v <- vintages[i]
  prepare_vintage(v)
}

num_cores <- detectCores() - 4
cl <- makeCluster(num_cores)

# Load necessary libraries in each cluster node
clusterEvalQ(cl, {
  library(bundesbank)
  library(lubridate)
  library(dplyr)
  library(tibble)
  library(readxl)
})

clusterEvalQ(cl, setwd(getwd()))
clusterExport(cl, c("vintages", "prepare_vintage"))

if(!dir.exists("./vintages_monthly")) {
  dir.create("./vintages_monthly")
}

# Measure the time taken
timing <- system.time({
  result <- parLapply(cl, 1:length(vintages), run_prepare_vintage)
})

stopCluster(cl)

print(timing)
