#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#-----------------VIX DATA RECONCILIATION SCRIPT------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

###############################################################
#-------------------------READ.ME------------------------------
# VIX Data Reconciliation Script
#
# I had ChatGPT write this READ ME (it's really good at that), but if you notice 
# anything wierd in this READ ME, that's why! (ive proofread it, but still, stuff happens)
#
# Purpose:
# This script performs a multi-step reconciliation between two independent VIX datasets:
#   - Kaggle-sourced VIX dataset
#   - YFinance VIX dataset (via yfinance pull)
# to pull this data yourself:
# URL (KAGGLE): https://www.kaggle.com/datasets/rohanroy/vix-historical-data-cboe-volatility-index
# USE YFINANCE library in Python and download csv that way!
#
# Focus Year: 2023
#
# Reconciliation Objectives:
# - Standardize schema and data types across both sources
# - Align date ranges to ensure a fair comparison
# - Detect and flag missing dates or inconsistent records
# - Compare and validate:
#     - OHLC data between the two files
#
# Key Sections of Script:
# 1. Setup and Library Load
# 2. Data set Loading
# 3. Column Name Standardization
# 4. Data Type Normalization
# 5. Value Scale Normalization 
# 6. Date Range Alignment
# 7. Missing Value Checks
# 8. Data Diffing (detect drift)
# 9. Control Totals Comparison
#
# Outputs:
# - Summary statistics of reconciliation
# - Flagged rows (if discrepancies exist)
# - Optional saved diff reports
#
# Assumptions:
# - Kaggle and Yahoo datasets use daily VIX closing data.
# - No major calendar mismatches (e.g., holidays, half-days).
# - Minor rounding/precision drift between vendors is acceptable within ±0.1 tolerance (configurable if needed).
#
# NOTE:
# The Yfinance Date structure was not in datetime, that will have to be investigated
# and changed!
# this is good practice for cleaning messy scraped data!
#
# Version 1.0
# Author: Grayson
# Date: 2025-05-15
#----------------------END READ.ME-----------------------------

###############################################################
#----------------------------Setup------------------------------
# Load required libraries (tidyverse)
#install.packages("tidyverse")
library(tidyverse)

#Load the data sets as raw-versions
#Kaggle Data Set:
kaggle_vix_raw <- read_csv("path_to_csv")
#yFinance Data Set:
yfinance_vix_raw <- read_csv("path_to_csv")

#initialize the working copies
kaggle_vix <- kaggle_vix_raw
yfinance_vix <- yfinance_vix_raw

#inspect the tables 
view(head(kaggle_vix))
view(head(yfinance_vix))
#we immediately notice several distinct difference between the tables
#that we will need to address before we reconcile the data
#--------------------------END Setup---------------------------

###############################################################
#-------------------Standardize Column Names--------------------
#Make sure both datasets have consistent, lowercase, snake_case column names
#install.packages("janitor")
library(janitor)



kaggle_vix <- kaggle_vix %>% clean_names()
yfinance_vix <- yfinance_vix %>% clean_names()

#rename the price column to be date
yfinance_vix <- yfinance_vix %>%
  rename(date = price)

#drop the volume column
yfinance_vix <- yfinance_vix %>%
  select(-volume)

#reorder the yfinance table column names 
yfinance_vix <- yfinance_vix %>%
  select(date, open, high, low, close)

#run the inspection lines in the set up to check the changes against each other
#----------------END Standardize Column Names-------------------

###############################################################
#------------------Ensure Column Types Match--------------------
# we'll want to slice the first two rows of the y finace data as well
yfinance_vix <- yfinance_vix %>%
  slice(-1, -2)

#because the YFinance data had a lot of junk is the top two rows,
#the data types of our OHLC data for that are out of whack
#It's reading them as CHAR! let's change that!
#this will also round them to 2 decimals! but under the hood maintains all that information 
#for our usecase, we'll do a manual round() later

yfinance_vix <- yfinance_vix %>%
  mutate(
    open = as.numeric(open),
    high = as.numeric(high),
    low = as.numeric(low),
    close = as.numeric(close)
  )

#---------------END Ensure Column Types Match-------------------

###############################################################
#---------------------Normalize Values-------------------------

#adjust date to match kaggle set

#yfinance_vix <- yfinance_vix %>%
# mutate(
#   date = as.Date(date, format = "%m/%d/%Y")
# )

#woah! what happened? looks like mutate read those dates as something else
#and turns them all into NAs!

str(yfinance_vix$date)
str(kaggle_vix$date)

yfinance_vix <- yfinance_vix %>%
  mutate(
    date = as.Date(date)
  )
kaggle_vix <- kaggle_vix %>%
  mutate(
    date = as.Date(date, format = "%m/%d/%Y")
  )
#now both dat columns are matching in proper ISO format!


yfinance_vix <- yfinance_vix %>%
  mutate(
    across(c(open, high, low, close), ~ round(., 2))
  )
#run the inspection lines in the set up to check the changes against each other
#------------------END Normalize Values------------------------
###############################################################
#-----------------Slice to Common Date Range---------------------
kaggle_vix <- kaggle_vix %>%
  filter(date >= as.Date("2023-01-01") & date <= as.Date("2023-12-31"))

#---------------END Slice to Common Date Range-------------------

###############################################################
#-------------------------Missing Values-------------------------
#set up a table with values = missingno
# Check missing values in Kaggle
kaggle_vix %>%
  summarise(
    missing_date = sum(is.na(date)),
    missing_open = sum(is.na(open)),
    missing_high = sum(is.na(high)),
    missing_low = sum(is.na(low)),
    missing_close = sum(is.na(close))
  )

# Check missing values in yfinance
yfinance_vix %>%
  summarise(
    missing_date = sum(is.na(date)),
    missing_open = sum(is.na(open)),
    missing_high = sum(is.na(high)),
    missing_low = sum(is.na(low)),
    missing_close = sum(is.na(close))
  )

#should be 0 but always nice to check!
#if you get something other than 0 you might have a different data set than 
#what i used while writing this
#if you do, I would record what rows have missing data, and drop them before the next step

#------------------------END Missing Values----------------------

###############################################################
#-------------------------Data Diffing-------------------------
#in order to diff the rows, we'll perform an inner join by date
#we'll suffix the columns with the data set's respective names to keep track

vix_joined <- inner_join(
  kaggle_vix, 
  yfinance_vix, 
  by = "date", 
  suffix = c("_kaggle", "_yahoo")
)
#you should have a nice joined table by date!
view(vix_joined)
#now lets do a mutate to make new columns by field
#that will tell us how much they differ row by row

vix_diff <- vix_joined %>%
  mutate(
    open_diff = open_kaggle - open_yahoo,
    high_diff = high_kaggle - high_yahoo,
    low_diff = low_kaggle - low_yahoo,
    close_diff = close_kaggle - close_yahoo
  )
view(vix_diff)

#now we will flag the mismatches
vix_flags <- vix_diff %>%
  mutate(
    open_mismatch = abs(open_diff) > 0.1,
    high_mismatch = abs(high_diff) > 0.1,
    low_mismatch = abs(low_diff) > 0.1,
    close_mismatch = abs(close_diff) > 0.1
  )
view(vix_flags)

vix_mismatches <- vix_flags %>%
  filter(open_mismatch | high_mismatch | low_mismatch | close_mismatch)

view(vix_mismatches)
#wow, incredible! no mismatches found!
#i was almost hoping for some mismatches, but at least we've done the work and 
#established that both tables have accurate and reliable data

#now we can export the mismatches csv as a report!
#this one will be pretty barren, but its best practice to always keep a report
#having no mismatches is still reportable
#down the road, no one will know that a missing report means good data!
write_csv(vix_mismatches, "vix_differences_report.csv")

#----------------------END Data Diffing------------------------
###############################################################
#---------------------Control Totals Check---------------------
# for control totals, we'll compare the sum and mean of each column 
# between the two data sets 

#kaggle totals
kaggle_totals <- kaggle_vix %>%
  summarise(
    total_close = sum(close, na.rm = TRUE),
    avg_close = mean(close, na.rm = TRUE),
    total_open = sum(open, na.rm = TRUE),
    avg_open = mean(open, na.rm = TRUE),
    total_high = sum(high, na.rm = TRUE),
    avg_high = mean(high, na.rm = TRUE),
    total_low = sum(low, na.rm = TRUE),
    avg_low = mean(low, na.rm = TRUE)
  )

#yfinace totals
yfinance_totals <- yfinance_vix %>%
  summarise(
    total_close = sum(close, na.rm = TRUE),
    avg_close = mean(close, na.rm = TRUE),
    total_open = sum(open, na.rm = TRUE),
    avg_open = mean(open, na.rm = TRUE),
    total_high = sum(high, na.rm = TRUE),
    avg_high = mean(high, na.rm = TRUE),
    total_low = sum(low, na.rm = TRUE),
    avg_low = mean(low, na.rm = TRUE)
  )
#veiw the tables and see just how different they actually appear to be at the 
#control level
view(yfinance_totals)
view(kaggle_totals)

#calculate the percentage difference 
abs(kaggle_totals$total_close - yahoo_totals$total_close) / yahoo_totals$total_close * 100

#we do the na.rm here even though we've already checked and dealth with nas
#this is just to make sure it all works!

#so we see signifgant drift ~2.67% between control totals
#we can summize that, because  our diffing only checks dates that exist in both
#there must be rows that exist in one but not the other
# this is likely due to how the different data set creators deal with holidays,
# off-market days etc.

#lets check the number of rows in each
nrow(kaggle_vix)
nrow(yfinance_vix)
#Yup! the kaggle data set has 7 more rows!

#let's take a closer look at those dates!
kaggle_extra_dates <- anti_join(kaggle_vix, yfinance_vix, by = "date")
#and save it for the records!
write_csv(kaggle_extra_dates, "kaggle_extra_dates.csv")
#-------------------END Control Totals Check-------------------
###############################################################

