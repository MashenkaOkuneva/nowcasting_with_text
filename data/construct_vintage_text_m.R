rm(list = ls())

# PACKAGES ----
library(lubridate)
library(dplyr)
library(tidyr)

# FUNCTIONS ----
#_____________________________________________________#
#_ roll_mean 
#_ bw_filter
#_____________________________________________________#

rollmean <- function(x, k){
  xroll <- array(NA, c(length(x)))
  for (t in seq(k, length(x)))
    xroll[t] <- mean(x[(t-k+1):t], na.rm = TRUE)
  
  return(xroll)
}

bw_filter <- function(y, bw)
{
  # compute un-normalized weights
  j <- seq(-bw, bw, 1) 
  omeg = (1 - (j/bw) ^ 2) ^ 2  
  
  # check for NA's in y
  n_NA <- sum(is.na(y)) 
  y_noNA <- y[(n_NA + 1) : length(y)]
  Nt <- length(y_noNA)
  
  # loop over t
  tau <- mat.or.vec(length(y_noNA), 1)
  for (t in 1 : length(y_noNA)) {
    # case distinction
    if (t <= bw) {
      
      indY <- c(1 : (2 * bw - (bw - t)))
      indOmeg <- c((bw - t + 2):(2 * bw + 1)) 
      kap <- 1 / ( sum(omeg[indOmeg]))
      tau[ t ] <- kap * omeg[indOmeg] %*% y_noNA[indY] 
      
    } else if (t > (Nt - bw)) {
      
      indY <- c((t - bw) : Nt)
      indOmeg <- c(1 : (bw + 1 + (Nt - t)))
      kap <- 1 / ( sum( omeg[indOmeg] ) )
      tau[t] <- kap * omeg[indOmeg] %*% y_noNA[indY] 
      
    } else {
      
      indY <- seq(t - bw, t + bw, 1)
      indOmeg <- c( 1 : (2 * bw + 1))
      kap <- 1 / (sum(omeg[indOmeg]))
      tau[t] <- kap * omeg[indOmeg] %*% y_noNA[indY]  
    }
  }
  return(c(rep(NA, times = n_NA), tau))
}

# SET-UP ----
#_____________________________________________________#
#_specify vintage,
#_sample_start,
#_window of moving average,
#_bandwidth
#_name of the file with topics
#_____________________________________________________#
vintage <- c("2008-01-15")
sample_start = c("1991-04-01")
K = 30
bw = 1200
topics_file <- "../sentiment/sentiment_adjusted_daily_topics.csv"

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

# LOAD TOPICS ----
#_____________________________________________________#
#_select sample,
#_extend to 7-day week
#_____________________________________________________#
df_raw <- read.csv(topics_file) %>%
  select(-any_of("X"))

# add date and quarter variable
df_raw %>%
  mutate(date = make_date(year = df_raw$year, 
                          month = df_raw$month, 
                          day = df_raw$day),
         quarter = ceiling(month / 3)) %>% 
  select(date, year, quarter, month, day, everything()) -> df_topics 

# get rid of raw df
rm(df_raw)

# extend series with NA to 7-day week 
dates_tmp <- data.frame(date = seq(min(df_topics$date), 
                                   max(df_topics$date), 
                                   by = "days")
)

dates_tmp %>% 
  mutate(year = year(date),
         month = month(date),
         quarter = ceiling(month / 3),
         day = day(date)) %>%
  merge(df_topics, by = c("date", "year", "quarter", "month", "day"), all.x = T) -> df_topics

# get rid of dates_tmp
rm(dates_tmp)

# col indices corresponding to topics
ind_topics <- which(grepl("T", names(df_topics)))

# adjust sample (leaving K additional rows at start which will be removed after smoothing)
df_topics %>% filter(date >= as.Date(sample_start) - days(K)) -> df_topics

# TRANSFORM TOPICS ---- 
#_____________________________________________________#
#_moving average,
#_detrend using biweight filter
#_reimpose NA pattern
#_adjust sample
#_____________________________________________________#

# select only topics
dat <- df_topics[, ind_topics]

# store pattern of missings
ind_NA <- is.na(dat)
colnames(ind_NA) <- names(df_topics)[grepl("T", names(df_topics))]

# moving average
dat_ma <- apply(dat, c(2), rollmean, k = K)

# detrend with biweight filter
dat_bw <- apply(dat_ma, c(2), bw_filter, bw = bw)
dat_detrend <- dat_ma - dat_bw # de-trended topics

# reimpose NA pattern
dat_detrend_NA <- dat_detrend
dat_detrend_NA[ind_NA] <- NA

# store in df_topics_trafo
df_topics_trafo <- df_topics
df_topics_trafo[ind_topics] <- dat_detrend_NA

# rm first K rows
df_topics_trafo <- df_topics_trafo[seq(K+1, nrow(df_topics_trafo)), ]

# convert transformed topics to monthly frequency
df_topics_trafo_M <- df_topics_trafo %>% 
  pivot_longer(
    cols = -c(date, year, quarter, month, day), 
    names_to = "topic", 
    values_to = "vals"
  ) %>%
  group_by(topic, year, month) %>%
  summarise(avg_vals = mean(vals, na.rm = TRUE), .groups = "drop") %>%
  # Create a date column as the first day of the month in "YYYY-MM-01" format
  mutate(date = as.Date(paste0(year, "-", sprintf("%02d", month), "-01"), format = "%Y-%m-%d")) %>%
  # Filter to only include dates from sample_start to cutoff_date
  filter(date >= as.Date(sample_start) & date <= cutoff_date) %>%
  # Select only the needed columns: date and the averaged topics
  select(date, topic, avg_vals) %>%
  # Pivot wider so that each topic becomes a separate column
  pivot_wider(id_cols = date, names_from = topic, values_from = avg_vals)

# 10 most correlated and meaningful topics (GDP, <2008, non-adjusted)
#list_topics_select <- c("T50", "T150", "T29", "T21", "T38", "T108", "T59",
#                        "T120", "T91", "T134")

# 10 most correlated and meaningful sentiment-adjusted topics (BPW) (GDP, <2008, sentiment-adjusted, BPW)
list_topics_select <- c("T50", "T183", "T120", "T29", "T150", "T154", "T167",
                        "T21", "T112", "T7")

df_topics_trafo_M <- df_topics_trafo_M %>%
  select(date, all_of(list_topics_select))

final_df <- df_topics_trafo_M

# Remove unnecessary objects
rm(list = c("dat", "dat_bw", "dat_detrend", "dat_detrend_NA", "dat_ma", "df_topics", 
            "df_topics_trafo", "ind_NA", "df_topics_trafo_M") , envir = .GlobalEnv)

# Transformations
transform <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

# Format the date column as m/d/yyyy
final_df$date <- as.Date(final_df$date, format = "%m/%d/%Y")
final_df$date <- paste0(
  as.integer(format(final_df$date, "%m")), "/",
  as.integer(format(final_df$date, "%d")), "/",
  format(final_df$date, "%Y")
)

# Create the transform row
transform_row <- final_df[1, ]
transform_row[, -1] <- as.list(as.numeric(transform))
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