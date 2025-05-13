rm(list = ls())

# PACKAGES ----
library(lubridate)
library(dplyr)
library(tidyr)
library(readxl)
library(purrr)
library(knitr)
library(kableExtra)
library(stringr)
library(openxlsx)

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

df_raw <- read.csv("../sentiment/sign_adjusted_daily_topics_format.csv") %>%
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
# The second option is to subset to only a given set of topics.
calc_topic_corr <- function(file, econ_var = "growth", topics_df = df_topics_trafo_Q, selected_topics = NULL) {
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
  
  if (!is.null(selected_topics)) {
    # Only return those topics
    corr_df <- filter(corr_df, topic %in% selected_topics) %>%
      # reorder rows to match "selected_topics"
      slice(match(selected_topics, topic))
  } else {
    # Or sort descending by absolute correlation and take the top 20 topics
    corr_df <- arrange(corr_df, desc(abs(corr))) %>% slice(1:20)
  }

  return(corr_df)
}

selected_topics <- c("T27", "T127", "T11", "T81", "T77", 
                     "T74", "T52", "T131", "T138", "T100")

# 1. Load and compute correlations for GDP
#gdp_corr <- calc_topic_corr("../dfm/GDP_growth_actual.csv", econ_var = "growth", selected_topics = selected_topics)
gdp_corr <- calc_topic_corr("../dfm/GDP_growth_actual.csv", econ_var = "growth", selected_topics = NULL)
# 2. For Consumption
#cons_corr <- calc_topic_corr("../dfm/Consumption_growth_actual.csv", econ_var = "growth", selected_topics = selected_topics)
cons_corr <- calc_topic_corr("../dfm/Consumption_growth_actual.csv", econ_var = "growth", selected_topics = NULL)
# 3. For Investment
#inv_corr <- calc_topic_corr("../dfm/Investment_growth_actual.csv", econ_var = "growth", selected_topics = selected_topics)
inv_corr <- calc_topic_corr("../dfm/Investment_growth_actual.csv", econ_var = "growth", selected_topics = NULL)

# Combine the three results
# Do a full join so that every topic that appears in at least one top-20 is retained
# The second option is to use "selected_topics"
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

# CORRELATION TABLES FOR THE TEXT ----
make_corr_table <- function(
    econ_var,
    n_top,
    surveys_to_include,
    topics_to_cross,
    topic_type,         # e.g. "topics", "topics_BPW", "topics_uncertainty", "topics_BCC"
    estimation_period,  # e.g. "2007", "2009", or "2018"
    num_topics,         # e.g. "200", "100"
    source,             # e.g. "all", "dpa", "hb", "sz", "welt"
    selected,           # e.g., "_selected", ""
    output_dir = "correlations",
    selected_topics = NULL
) {
  # 1) Define topic labels
  topic_labels <- c(
    "T27"  = "\\makecell[tc]{ Economic Crises \\\\ and Recessions}",
    "T127" = "\\makecell[tc]{ Major Banks and \\\\ Investment Banking}",
    "T11"  = "Mergers and Acquisitions",
    "T81"  = "\\makecell[tc]{ Corporate Restructuring and \\\\ Job Cuts in Germany}",
    "T77"  = "Private Investment",
    "T74"  = "\\makecell[tc]{ Concerns about Economic\\\\ Bubbles and Recessions}",
    "T52"  = "\\makecell[tc]{ German Automobile Industry \\\\ and Major Manufacturers}",
    "T131" = "\\makecell[tc]{German Investments in \\\\ Emerging Markets}",
    "T138" = "\\makecell[tc]{ Financial and Economic \\\\ Performance}",
    "T100" = "\\makecell[tc]{ Market Reactions to \\\\News}",
    "T73"  = "\\makecell[tc]{ International Relations and \\\\ Diplomacy}",
    "T151" = "\\makecell[tc]{ Balkan Conflicts and \\\\ International Responses}",
    "T105" = "\\makecell[tc]{ Democracy and Leadership \\\\ in History }",
    "T180" = "\\makecell[tc]{International Summits \\\\ and Conferences}",
    "T191" = "\\makecell[tc]{Monetary Policy and \\\\ Central Banking}",
    "T94"  = "Religion",
    "T45"  = "\\makecell[tc]{Political Dynamics of the \\\\ FDP and Coalition Government}",
    "T95"  = "\\makecell[tc]{Middle East Politics \\\\ and Israeli-Palestinian Conflict}",
    "T35"  = "\\makecell[tc]{Russia and Post-Soviet \\\\ Dynamics}",
    "T166" = "\\makecell[tc]{ Infrastructure and \\\\ Transportation Projects}",
    "T102" = "\\makecell[tc]{  Taxation and Fiscal \\\\ Policy}",
    "T93"  = "\\makecell[tc]{Government Budget and \\\\ Fiscal Policy}",
    "T30"  = "\\makecell[tc]{ Urban Development \\\\ and Real Estate}",
    "T126" = "\\makecell[tc]{ Unemployment and Labor \\\\ Market Policies}",
    "T28"  = "Uncertainty and Expectations",
    "T7"   = "\\makecell[tc]{ Culture, Arts \\\\ and Literature}",
    "T147" = "\\makecell[tc]{ Business Financing and \\\\ Credit Solutions}",
    "T15"  = "General Commentary",
    "T168" = "\\makecell[tc]{ Derivatives and Financial \\\\ Instruments}",
    "T157" = "\\makecell[tc]{French Politics and \\\\ Finance}",
    "T50"  = "\\makecell[tc]{ Personal Opinions and \\\\ Beliefs in Interviews}"
  )
  
  # 2) build the dataframe of top correlations
  df <- final_corr %>%
    select(topic, all_of(econ_var), all_of(surveys_to_include)) %>%
    arrange(desc(abs(.data[[econ_var]]))) %>%
    slice(1:n_top) %>%
    mutate(RawLabel = topic_labels[topic]) %>%
    rowwise() %>%
    mutate(
      Label = if (topic %in% topics_to_cross) {
        # crossed-out topics
        if (str_detect(RawLabel, "\\\\makecell")) {
          # extract the inside of \makecell[tc]{…}
          body <- str_match(RawLabel, "\\\\makecell\\[tc\\]\\{(.*)\\}")[,2]
          parts <- str_split(body, "\\\\\\\\")[[1]]
          # wrap each line in \sout{…}
          crossed <- paste0("\\sout{", parts, "}", collapse=" \\\\ ")
          # reassemble as a single \makecell
          paste0("\\makecell[tl]{", crossed, "}")
        } else {
          paste0("\\sout{", RawLabel, "}")
        }
      } else if (str_detect(RawLabel, "\\\\makecell\\[tc\\]")) {
        # any non-crossed makecell[tc] -> makecell[tl]
        str_replace(RawLabel, "\\\\makecell\\[tc\\]\\{", "\\\\makecell[tl]{")
      } else {
        RawLabel
      }
    ) %>%
    ungroup() %>%
    mutate(across(all_of(c(econ_var, surveys_to_include)), ~ round(.,3))) %>%
    select(
      ID = topic,
      Label = Label,
      all_of(econ_var),
      all_of(surveys_to_include)
    )
  
  # 3) a single lookup for all possible survey‐column renames
  survey_renames <- c(
    ifoIndTradeClimate = "ifo\\_Climate",
    ifoIndTradeCurrent = "ifo\\_Current",
    ifoIndTradeExp     = "ifo\\_Exp",
    ESI                = "ESI",
    GfKBCE             = "GfKBCE",
    GfKIE              = "GfKIE",
    GfKWtB             = "GfKWtB",
    GfKCCI             = "GfKCCI"
  )
  
  # 4) apply those renames to the df's names
  new_names <- names(df)
  for (sv in surveys_to_include) {
    new_names[new_names == sv] <- survey_renames[sv]
  }
  
  # 5) figure out how many right‐aligned 'r' columns
  n_nums <- ncol(df) - 2
  
  # 6) grab raw LaTeX tabular from kable()
  raw_tab <- df %>%
    kable(
      format   = "latex",
      booktabs = TRUE,
      escape   = FALSE,
      align    = c("l","l", rep("r", n_nums)),
      col.names= new_names
    ) %>%
    as.character()
  
  # 7) replace booktabs rules with \hline
  raw_tab <- gsub("\\\\toprule",    "\\\\hline", raw_tab)
  raw_tab <- gsub("\\\\midrule",    "\\\\hline", raw_tab)
  raw_tab <- gsub("\\\\bottomrule", "\\\\hline", raw_tab)
 
  # 8) build dynamic footnote definitions
  defs <- list(
    ifoIndTradeClimate = "ifo business climate for the industry \\& trade (balances)",
    ifoIndTradeCurrent = "current business situation for the industry \\& trade (balances)",
    ifoIndTradeExp     = "ifo business cycle expectations for the industry \\& trade (balances)",
    ESI                = "Economic Sentiment Indicator",
    GfKBCE             = "GfK: business cycle expectations",
    GfKIE              = "GfK: income expectations",
    GfKWtB             = "GfK: willingness-to-buy",
    GfKCCI             = "GfK: consumer climate indicator"
  )
  
  foot_items <- vapply(
    surveys_to_include,
    function(sv) paste0("‘", survey_renames[sv], "’ = ", defs[[sv]]),
    character(1)
  )
  
  caption_text <- if (topic_type == "topics") {
    sprintf("  \\caption{Topics Most Correlated with %s and Selected Surveys}\n", econ_var)
  } else if (topic_type == "topics_BPW") {
    sprintf("  \\caption{Sentiment-adjusted Topics (BPW) Most Correlated with %s and Selected Surveys}\n", econ_var)
  } else if (topic_type == "topics_BCC" && !is.null(selected_topics)) {
    sprintf("  \\caption{Correlations of Selected Sign-adjusted Topics (BCC) with %s and Surveys}\n", econ_var)
  } else if (topic_type == "topics_BCC" && is.null(selected_topics)) {
    sprintf("  \\caption{Sign-adjusted Topics (BCC) Most Correlated with %s and Selected Surveys}\n", econ_var)
  } else {
    sprintf("  \\caption{%s Most Correlated with %s and Selected Surveys}\n",
            topic_type, econ_var)
  }
  
  label_text <- sprintf(
    "  \\label{tab:cor_%s_%s_%s_%s_%s%s}\n",
    tolower(econ_var),
    topic_type,
    estimation_period,
    num_topics,
    source,
    selected
  )
  
  note_body <- if (topic_type == "topics") {
    sprintf(
      "For survey correlations, the coefficient is shown only if that topic is among the top 20 in absolute correlation with that survey; otherwise it is NA. A topic is crossed out if its relationship with %s was judged difficult to explain economically.",
      econ_var
    )
  } else if (topic_type == "topics_BPW") {
    sprintf(
      "For survey correlations, the coefficient is shown only if that sentiment-adjusted topic (BPW) is among the top 20 in absolute correlation with that survey; otherwise it is NA. A sentiment-adjusted topic (BPW) is crossed out if its relationship with %s was judged difficult to explain economically.",
      econ_var
    )
  } else if (topic_type == "topics_BCC") {
    sprintf(
      "For survey correlations, the coefficient is shown only if that sign-adjusted topic (BCC) is among the top 20 in absolute correlation with that survey; otherwise it is NA."
    )
  } else {
    sprintf(
      "For survey correlations, the coefficient is shown only if that topic (%s) is among the top 20 in absolute correlation with that survey; otherwise it is NA. A topic is crossed out if its relationship with %s was judged difficult to explain economically.",
      topic_type,
      econ_var
    )
  }
  
  footnote_text <- paste0(
    "Note: ", 
    paste(foot_items, collapse="; "), ". ",
    note_body
  )
  
  # 9) assemble final .tex
  full_tex <- paste0(
    "\\begin{table}[h!]\n",
    "  \\centering\n",
    "  \\footnotesize\n",
    "  \\renewcommand{\\arraystretch}{1.3}\n",
    caption_text,
    label_text, "\n",
    paste(raw_tab, collapse="\n"), "\n\n",
    "  \\begin{minipage}{\\textwidth}\n",
    "    \\vspace{0.2cm}\n",
    "    \\small\n",
    "    \\setlength{\\baselineskip}{0.8\\baselineskip}\n",
    footnote_text, "\n",
    "  \\end{minipage}\n",
    "\\end{table}\n"
  )
  
  # 10) write it out
  
  # ensure the subfolder exists
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # build the output path
  output_file <- file.path(
    output_dir,
    paste0(
      "correlation_table_",
      econ_var, "_",
      topic_type, "_",
      estimation_period, "_",
      num_topics, "_",
      source,
      selected,
      ".tex"
    )
  )
  
  writeLines(full_tex, output_file)
  
  # 11) extract the non-crossed topics for the XLSX
  defs_df <- df %>%
    filter(!ID %in% topics_to_cross) %>%
    mutate(
      Description = map_chr(ID, function(mnemonic) {
        label <- topic_labels[mnemonic]
        if (str_detect(label, "\\\\makecell")) {
          # pull out text inside the braces
          body <- str_match(label, "\\\\makecell\\[.*?\\]\\{(.*)\\}")[,2]
          # collapse any LaTeX line-breaks and remove runs of more than one space
          clean <- body %>%
            str_replace_all("\\\\\\\\", " ") %>%  # turn \\ into space
            str_squish()                          # collapse runs of spaces
          clean
        } else {
          str_squish(label)
        }
      })
    ) %>%
    select(Mnemonic = ID, Description) %>%
    mutate(Group = "Text")
  
  # 12) write to the XLSX
  if (!dir.exists("data_text")) dir.create("data_text", recursive = TRUE)
  xlsx_file <- file.path(
    "data_text",
    paste0("variables_definitions_",
           econ_var, "_",
           topic_type, "_",
           estimation_period, "_",
           num_topics, "_",
           source,
           selected, ".xlsx")
  )
  wb <- createWorkbook()
  addWorksheet(wb, "Tabelle1")
  # header style: normal font, no bold, left aligned
  headerStyle <- createStyle(
    fontName    = "Calibri",
    fontSize    = 11,
    textDecoration = NULL,
    halign      = "left"
  )
  writeData(
    wb, 
    sheet = "Tabelle1", 
    x     = defs_df, 
    headerStyle = headerStyle
  )
  # auto‐size columns
  setColWidths(wb, sheet = "Tabelle1", cols = 1:ncol(defs_df), widths = "auto")
  saveWorkbook(wb, xlsx_file, overwrite = TRUE)
}

## 10 SIGN-ADJUSTED PRE-SELECTED TOPICS (BCC) ##
# For GDP:
make_corr_table(
  econ_var = "GDP",
  n_top = 10,
  surveys_to_include = c("ifoIndTradeClimate","ifoIndTradeCurrent","ifoIndTradeExp","ESI"),
  topics_to_cross      = c(),
  topic_type          = "topics_BCC",
  estimation_period   = "2009",
  num_topics          = "200",
  source              = "all",
  selected            = "_selected",
  selected_topics = selected_topics
)

# For Consumption:
make_corr_table(
  econ_var = "Consumption",
  n_top = 10,
  surveys_to_include = c("GfKBCE","GfKIE","GfKWtB","GfKCCI"),
  topics_to_cross      = c(),
  topic_type          = "topics_BCC",
  estimation_period   = "2009",
  num_topics          = "200",
  source              = "all",
  selected            = "_selected",
  selected_topics = selected_topics
)

# For Investment:
make_corr_table(
  econ_var = "Investment",
  n_top = 10,
  surveys_to_include = c("ifoIndTradeClimate","ifoIndTradeCurrent","ifoIndTradeExp","ESI"),
  topics_to_cross      = c(),
  topic_type          = "topics_BCC",
  estimation_period   = "2009",
  num_topics          = "200",
  source              = "all",
  selected            = "_selected",
  selected_topics = selected_topics
)

## SIGN-ADJUSTED TOPICS (BCC) MOST CORRELATED WITH AN ECONOMIC VARIABLE##
# For Consumption:
make_corr_table(
  econ_var = "Consumption",
  n_top = 15,
  surveys_to_include = c("GfKBCE","GfKIE","GfKWtB","GfKCCI"),
  topics_to_cross      = c("T151", "T105", "T94", "T45", "T30"),
  topic_type          = "topics_BCC",
  estimation_period   = "2009",
  num_topics          = "200",
  source              = "all",
  selected            = "",
  selected_topics = NULL
)

# For Investment:
make_corr_table(
  econ_var = "Investment",
  n_top = 12,
  surveys_to_include = c("ifoIndTradeClimate","ifoIndTradeCurrent","ifoIndTradeExp","ESI"),
  topics_to_cross      = c("T7", "T15"),
  topic_type          = "topics_BCC",
  estimation_period   = "2009",
  num_topics          = "200",
  source              = "all",
  selected            = "",
  selected_topics = NULL
)


