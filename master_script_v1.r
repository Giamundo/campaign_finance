options(encoding = "UTF-8")

## Libraries and cURL setup ====================================================
library("tidyverse")
library("magrittr")
library("lubridate")
library("jsonlite")
library("httr")
library("RCurl")
library("data.table")
library("stringr")


# Set cURL timeout
curlSetOpt(timeout = 200)

# Function to check internet connectivity
has_internet <- function() {
  !is.null(curl::nslookup("r-project.org", error = FALSE))
}

## Define API keys =============================================================
key1 <- "NbwsAqfO3bLLzRnKvVLQmKTCf3MsEGe0LdV8Svrh"
key2 <- "wFVS5SkyOkqgXDzbaBGV8qSyy9CM9qBpMgw4Y07u"

# Function to rotate between two API keys
get_api_key <- function(week) {
  if (week %% 2 == 0) {
    return(key1)
  } else {
    return(key2)
  }
}

## Define election cycles and weeks ============================================
cycles <- seq(2020, 2024, 4)  # Define election cycles
weeks <- seq(1, 52)           # Assume 52 weeks in a year

## Create directories for all relevant years ===================================
for (cycle in cycles) {
  # Include prior year (cycle - 1) and current cycle year
  for (year in c(cycle - 1, cycle)) {
    path <- paste0("year", year)
    if (!dir.exists(path)) {
      dir.create(path)
    }
    for (week in weeks) {
      subpath <- file.path(path, paste0("week", str_pad(week, 3, pad = "0")))
      if (!dir.exists(subpath)) {
        dir.create(subpath)
      }
    }
  }
}

## Setup date ranges for API calls =============================================
get_date_ranges <- function(cycle) {
  start_date <- as.Date(paste0(cycle - 1, "-01-01"))
  end_date <- as.Date(paste0(cycle + 1, "-01-01"))
  
  first_dates <- seq(start_date, end_date, by = "1 week")
  last_dates <- pmin(first_dates + 6, end_date - 1)
  
  list(first_dates = first_dates, last_dates = last_dates)
}

## Fetch and save data with full pagination =====================================
fetch_data <- function(cycle, week) {
  date_ranges <- get_date_ranges(cycle)
  
  first_date <- date_ranges$first_dates[week]
  last_date <- date_ranges$last_dates[week]
  
  print(paste("Cycle:", cycle, "Week:", week, "First Date:", first_date, "Last Date:", last_date))  # Debug print
  
  # Construct initial API URL
  baseURL <- paste0(
    "https://api.open.fec.gov/v1/schedules/schedule_a/",
    "?contributor_type=individual&contributor_type=committee",
    "&api_key=", get_api_key(week), 
    "&per_page=100&sort=contribution_receipt_date",
    "&min_date=", first_date,
    "&max_date=", last_date,
    "&two_year_transaction_period=", cycle
  )
  
  has_more_pages <- TRUE
  page <- 1
  last_index <- NULL
  last_contribution_receipt_date <- NULL
  
  while (has_more_pages) {
    # Update URL with the current page and pagination parameters
    url <- baseURL
    if (!is.null(last_index)) {
      url <- paste0(url, "&last_index=", last_index, "&last_contribution_receipt_date=", last_contribution_receipt_date)
    }
    
    # Fetch data
    response <- GET(url)
    print(paste("Fetching page:", page, "Response status:", http_status(response)$category))  # Debug print
    
    if (http_status(response)$category == "Success") {
      content_text <- content(response, as = "text", encoding = "UTF-8")
      file <- paste0("year", ifelse(year(first_date) < cycle, cycle - 1, cycle), 
                     "/week", str_pad(week, 3, pad = "0"), 
                     "/contrib-", cycle, "-", week, "-", page, ".json")
      #writeLines(content_text, file, useBytes = TRUE, append = TRUE)
    con <- file(file, open = "a")
        writeLines(content_text, con)
        close(con)
      
      # Print first 500 characters of the content for debugging
      print(substr(content_text, 1, 500))
      
      # Parse content to check pagination
      content_parsed <- fromJSON(content_text, flatten = TRUE)
      if ("pagination" %in% names(content_parsed)) {
        has_more_pages <- content_parsed$pagination$page < content_parsed$pagination$pages
        last_index <- content_parsed$pagination$last_indexes$last_index
        last_contribution_receipt_date <- content_parsed$pagination$last_indexes$last_contribution_receipt_date
        page <- page + 1
      } else {
        has_more_pages <- FALSE
      }
      
      Sys.sleep(2)  # Avoid hitting rate limits
    } else {
      print(paste("Error fetching data for cycle", cycle, "week", week, "page", page, "Status:", http_status(response)$category))
      has_more_pages <- FALSE
    }
  }
}

## Main script execution =======================================================
for (cycle in cycles) {
  print(paste("Processing cycle:", cycle))  # Debug print
  for (week in weeks) {
    print(paste("Processing week:", week))  # Debug print
    tryCatch({
      fetch_data(cycle, week)
      print(paste("Downloaded data for cycle", cycle, "week", week))
    }, error = function(e) {
      print(paste("Error occurred for cycle", cycle, "week", week, ":", e$message))
    })
    Sys.sleep(runif(1, 1, 5))  # Random sleep to avoid hitting rate limits
  }
}
