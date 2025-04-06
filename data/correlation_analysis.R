rm(list = ls())

# PACKAGES ----
library(lubridate)
library(dplyr)
library(tidyr)
library(readxl)
library(purrr)

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

# PARAMETERS ----
sample_start = c("1991-04-01")
K = 30
bw = 1200

# LOAD TOPICS ----
#_____________________________________________________#
#_select sample,
#_extend to 7-day week
#_____________________________________________________#

df_raw <- read.csv("../topics/daily_topics.csv")

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

# convert transformed topics to quarterly frequency
df_topics_trafo %>% 
  pivot_longer(cols = -c(date, year, quarter, month, day), 
               names_to = "topic", 
               values_to = "vals") %>%
  group_by(topic, year, quarter) %>%
  summarise(avg_vals = mean(vals, na.rm = T)) %>%
  pivot_wider(id_cols = c(year, quarter), 
              names_from = topic, 
              values_from = avg_vals) -> df_topics_trafo_Q

# convert transformed topics to monthly frequency
df_topics_trafo %>% 
  pivot_longer(cols = -c(date, year, quarter, month, day), 
               names_to = "topic", 
               values_to = "vals") %>%
  group_by(topic, year, quarter, month) %>%
  summarise(avg_vals = mean(vals, na.rm = T)) %>%
  pivot_wider(id_cols = c(year, month), 
              names_from = topic, 
              values_from = avg_vals) -> df_topics_trafo_M

# CORRELATION ANALYSIS ---- 
# Create a date column in df_topics_trafo_Q that matches the economic series date format
# Q1 -> "03", Q2 -> "06", Q3 -> "09", Q4 -> "12"
df_topics_trafo_Q <- df_topics_trafo_Q %>%
  mutate(date = paste(year, 
                      ifelse(quarter == 1, "03",
                      ifelse(quarter == 2, "06",
                      ifelse(quarter == 3, "09", "12"))),
                      sep = "-"))

# Given a file and a variable name ("growth"), merge an economic variable with the topics data 
# and compute correlations for each topic column. Then, return the top 20 topics sorted 
# by descending absolute correlation.
calc_topic_corr <- function(file, econ_var = "growth", topics_df = df_topics_trafo_Q) {
  econ <- read.csv(file, stringsAsFactors = FALSE)
  
  # Merge topics and economic series on date
  merged <- topics_df %>% inner_join(econ, by = "date")
  
  # Identify topic columns (topics start with "T")
  topic_cols <- names(merged)[grepl("^T", names(merged))]
  
  # Compute correlation for each topic
  corr_df <- lapply(topic_cols, function(topic) {
    corr_val <- cor(merged[[topic]], merged[[econ_var]], use = "complete.obs")
    data.frame(topic = topic, corr = corr_val)
  }) %>% bind_rows()
  
  # Sort descending by absolute correlation and take the top 20 topics
  corr_df <- corr_df %>% arrange(desc(abs(corr))) %>% slice(1:20)
  return(corr_df)
}

# 1. Load and compute correlations for GDP
gdp_corr <- calc_topic_corr("../dfm/GDP_growth_actual.csv", econ_var = "growth")
# 2. For Consumption
cons_corr <- calc_topic_corr("../dfm/Consumption_growth_actual.csv", econ_var = "growth")
# 3. For Investment
inv_corr <- calc_topic_corr("../dfm/Investment_growth_actual.csv", econ_var = "growth")

# Combine the three results
# Do a full join so that every topic that appears in at least one top-20 is retained
combined_corr <- full_join(gdp_corr %>% rename(GDP = corr),
                           cons_corr %>% rename(Consumption = corr),
                           by = "topic") %>%
  full_join(inv_corr %>% rename(Investment = corr),
            by = "topic")

## SURVEYS ##
# Load surveys data and variable definitions
surveys <- read_excel("./data_monthly/Surveys.xlsx") %>%
  select(-pub_date_ESI)

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

survey_vars <- setdiff(names(surveys), "date")

# Create a date column in df_topics_trafo_M that matches the surveys date format
# 04/1991, 05/1991 etc.
df_topics_trafo_M <- df_topics_trafo_M %>%
  mutate(date = paste(sprintf("%02d", month), year, sep = "/"))

# Given a variable name, merge a survey indicator with the topics data 
# and compute correlations for each topic column. Then, return the top 20 topics sorted 
# by descending absolute correlation.
calc_topic_corr_monthly <- function(surveys_df, survey_var, topics_df = df_topics_trafo_M) {
  
  # Merge topics and surveys on date 
  merged <- topics_df %>% inner_join(surveys_df, by = "date")
  
  # Ensure the survey variable column is numeric
  merged[[survey_var]] <- as.numeric(as.character(merged[[survey_var]]))
  
  # Identify topic columns (topics start with "T")
  topic_cols <- names(merged)[grepl("^T", names(merged))]
  
  # Compute correlation for each topic with the given survey variable
  corr_df <- lapply(topic_cols, function(topic) {
    corr_val <- cor(merged[[topic]], merged[[survey_var]], use = "complete.obs")
    data.frame(topic = topic, corr = corr_val)
  }) %>% bind_rows()
  
  # Sort by absolute correlation and select the top 20 topics
  corr_df <- corr_df %>% arrange(desc(abs(corr))) %>% slice(1:20)
  
  return(corr_df)
}

# Create correlation dfs for each of the survey indicators
for(sv in survey_vars) {
  df_name <- paste0(sv, "_corr")
  # Calculate the correlation 
  corr_df <- calc_topic_corr_monthly(surveys_df = surveys, survey_var = sv, topics_df = df_topics_trafo_M)
  assign(df_name, corr_df)
}

# Create an empty list to store survey correlation data frames
survey_corr_list <- list()

for(sv in survey_vars) {
  # Calculate the correlation 
  corr_df <- calc_topic_corr_monthly(surveys_df = surveys, survey_var = sv, topics_df = df_topics_trafo_M)
  # Rename the 'corr' column to the variable name
  corr_df <- corr_df %>% rename(!!sv := corr)
  # Store in the list 
  survey_corr_list[[sv]] <- corr_df
}

# Combine correlations of economic series and surveys
final_corr <- reduce(survey_corr_list, full_join, .init = combined_corr, by = "topic")

