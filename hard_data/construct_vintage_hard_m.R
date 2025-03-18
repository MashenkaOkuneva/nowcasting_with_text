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
vintage <- c("2008-01-31")

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
series_codes <- c("BBKRT.M.DE.Y.I.IP1.AA031.C.I", # Production in main construction industry (ConstrProd)
                  "BBKRT.M.DE.Y.I.IP1.ACM01.C.I", # Industrial production index (IP)
                  "BBKRT.M.DE.Y.I.IO1.AA031.C.I", # New orders for main construction industry (ConstrNO)
                  "BBKRT.M.DE.Y.I.IO1.ACM01.C.I", # New orders for industry (INO)
                  "BBKRT.M.DE.Y.I.IT1.AA031.V.A", # Main construction industry turnover (ConstrTurn)
                  "BBKRT.M.DE.Y.I.IT1.ACM01.V.I", # Industry turnover (ITurn)
                  "BBKRT.M.DE.Y.I.IT1.AGA01.C.I", # Retail turnover excluding cars (RetTurn)
                  "BBKRT.M.DE.Y.P.PC1.PC100.R.I", # Consumer price index (CPI)
                  "BBKRT.M.DE.S.P.PC1.PC110.R.I", # Consumer price index, excluding energy (CPIEN)
                  "BBKRT.M.DE.S.P.PP1.PP100.R.I", # Producer price index (PPI)
                  "BBKRT.M.DE.S.P.PP1.PP200.R.I", # Producer price index, excluding energy (PPIEN)
                  "BBKRT.M.DE.S.P.CX1.PP000.R.I", # Export price index (EPI)
                  "BBKRT.M.DE.S.P.CM1.PP000.R.I", # Import price index (IPI)
                  "BBKRT.M.DE.Y.L.BE2.AA022.H.I", # Hours worked: manufacturing (HW)
                  "BBKRT.M.DE.Y.L.BE2.AA031.H.A", # Hours worked: construction (ConstrHW)
                  "BBKRT.M.DE.S.L.BE1.CA010.P.A", # Employment (Empl)
                  "BBKRT.M.DE.Y.L.DE2.AA022.V.I", # Gross wages and salaries: manufacturing and mining (GWMan)
                  "BBKRT.M.DE.Y.L.DE2.AA031.V.A"  # Gross wages and salaries: construction (GWConstr)
)

# Mnemonics
series_names <- c("ConstrProd", "IP", "ConstrNO", "INO", 
                  "ConstrTurn", "ITurn", "RetTurn", "CPI", "CPIEN",
                  "PPI", "PPIEN", "EPI", "IPI", "HW", "ConstrHW", "Empl",
                  "GWMan", "GWConstr")

# Ensure the "data_monthly" directory exists; if not, create it
if(!dir.exists("data_monthly")){
  dir.create("data_monthly")
}

# Loop through each series code and download data
for (i in seq_along(series_codes)) {
  code <- series_codes[i]
  short_name <- series_names[i]
  
  # Get the series
  economic_data <- getSeries(code)
  
  # Assign this data to a new variable with the short name in the global environment
  assign(short_name, economic_data, envir = .GlobalEnv)
  
  # Save to an .Rda file in the "data_monthly" folder using the short name
  save(list = short_name, file = file.path("data_monthly", paste0(short_name, ".Rda")))
}

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

# Format the date column as m/d/yyyy
final_df$date <- as.Date(final_df$date, format = "%m/%d/%Y")
final_df$date <- paste0(
  as.integer(format(final_df$date, "%m")), "/",
  as.integer(format(final_df$date, "%d")), "/",
  format(final_df$date, "%Y")
)

# Create the transform row
transform_row <- final_df[1, ]
transform_row[] <- 1
transform_row$date <- "Transform:"

# Insert the transform row at the top of final_df
final_df <- rbind(transform_row, final_df)

# Convert vintage to a Date object
vintage_date <- as.Date(vintage)
# Compute the file vintage
file_vintage <- format(vintage_date + days(1), "%Y-%m-%d")
# Write to CSV
file_name <- paste0(file_vintage, ".csv")
write.csv(final_df, file_name, row.names = FALSE)

