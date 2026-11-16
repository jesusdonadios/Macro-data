# EUROSTAT - MONTHLY INDUSTRIAL PRODUCTION

# 1. Only if running independently: 
rm(list = ls()) 
library(httr2)
library(readr)
library(dplyr)

# setwd("C:/Users/jdona/Documents/BdE") #Set working directory,

 source("Config.R")  # Define countries and assign IDs  

# 3. Define API URL
base_url <- paste0(
  "https://ec.europa.eu/eurostat/api/dissemination/",
  "sdmx/3.0/data/dataflow/ESTAT/sts_inpr_m/1.0/"
)
# Dataset URL: https://ec.europa.eu/eurostat/databrowser/bookmark/5d9cf983-cf34-40dd-a586-78feca3e554f?lang=en&createdAt=2026-10-23T10:25:57Z

# 4. Define API parameters
query <- list(
  "c[freq]" = "M", #Monthly frequency
  "c[indic_bt]" = "PRD", #Production index
  "c[nace_r2]" = "B-D", #NACE Rev.2 sections B, C and D
  "c[s_adj]" = "CA", #Calendar adjusted data, not seasonally adjusted data
  "c[unit]" = "PCH_SM", # Percentage change compared to same period in previous year
  "c[geo]" = paste(countries$geo, collapse = ","),
  "c[TIME_PERIOD]" = "ge:2000-01", # we start from 2000 bc is when we have data for all countries, to start from the begginig you can do "c[TIME_PERIOD]" = "ge:1953-01",
  compress = "false",
  format = "csvdata",
  formatVersion = "2.0",
  lang = "en",
  labels = "id"
)

# 5. Download data
response <- request(base_url) |>
  req_url_query(!!!query) |>
  req_retry(max_tries = 3) |>
  req_perform()

datos <- read_csv(
  resp_body_string(response),
  show_col_types = FALSE
)


# 6. Clean, transform data and include country IDs
IPI <- datos %>%
  left_join(countries, by = "geo") %>%
  transmute(
    id,
    country,
    date = TIME_PERIOD,
    ip = as.numeric(OBS_VALUE)
  ) %>%
  arrange(id, date)

# 7. View results
# print(IPI)
# View(IPI)

#8. Save the results
write_csv(IPI, "1_Eurostat_IP.csv")

# 9. Summary of data availability
latest_IPI <- IPI %>%
  filter(!is.na(ip)) %>%
  group_by(date) %>%
  summarise(
    n_countries = n_distinct(id),
    .groups = "drop"
  ) %>%
  summarise(
    variable = "Industrial Production",
    frequency = "Monthly",
    first_available = min(date),
    last_available = max(date),
    countries_updated = paste0(
      n_countries[date == max(date)], "/10"
    ),
    last_complete_period = if (any(n_countries == 10)) {
      max(date[n_countries == 10])
    } else {
      NA_character_
    }
  )

print(latest_IPI)
