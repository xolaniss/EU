library("tidyverse")
library("lubridate")
library("rebus")
library("xts")
library("here")
library("filesstrings")
library("readxl")

# Functions ---------------------------------------------------------------
CPI_zip_excel_download <- function(url, filename, filetype) {
  download.file(url, paste0(filename, ".zip"), method = "auto")
  outDir <- here()
  unzip(paste0(filename, ".zip"), exdir = outDir)
  file.remove(here(paste0(filename, ".zip")), recursive = TRUE)
  file.rename(here("Excel - CPI (COICOP) from Jan 2008.xls"), here(paste0(filename, filetype)))
}

CPI_split <- function(data) {
  data %>% mutate(H13 = str_replace_all(H13, "Rural Areas", "Rural areas")) %>%
    mutate(H13 = str_replace_all(H13, "North West", "North-West")) %>%
    mutate(H13 = str_replace_all(H13, "Total country", "South Africa")) %>%
    group_by(H13) %>%
    group_split(H13, .add = TRUE)
}

CPI_clean <- function(data) {
  data %>%   select(-c("H01",
                       "H02",
                       "H03",
                       "H06",
                       "H14",
                       "H18",
                       "H17",
                       "H25")) %>%
    pivot_longer(starts_with("M"), "Period", values_to = "Index") %>%
    mutate(Period = str_replace_all(Period, "MO", "")) %>%
    mutate(Period = parse_date_time(Period, "mY")) %>%
    relocate(H13, H04, Period, Index, H05) %>%
    rename(Category = H04, Notes = H05, Province = H13)
}
list_RDS_export <- function (export_list, name) {
  saveRDS(export_list, file = name)
}

# URL ---------------------------------------------------------------------
base_url <-
  "http://www.statssa.gov.za/timeseriesdata/Excel/P0141%20-%20CPI(COICOP)%20from%20Jan%202008%20"
suppl_url <- ".zip"
year <- year(Sys.Date())
publication_lag <-  month(1)
month <- month(Sys.Date()) - publication_lag
url <- paste0(base_url, "(", year, month, ")", suppl_url)

# Data Download -----------------------------------------------------------
#CPI_zip_excel_download(url, "SA_CPI", "xls")


# Data Import -------------------------------------------------------------
CPI <- read_excel("SA_CPI.xls", col_names = TRUE)


# Data Cleaning -----------------------------------------------------------
CPI_months <-
  CPI %>% select(starts_with("M")) %>% map_df(., as.numeric)
CPI_descriptions <-  CPI %>% select(-starts_with("M"))
CPI <- tbl_df(cbind(CPI_descriptions, CPI_months))

country_region_names <- sort(unique(CPI$H13)) %>%
  str_replace_all("Rural Areas", "Rural areas") %>%
  str_replace_all("North-West", "North West") %>%
  str_replace_all("Total country", "South Africa") %>%
  unique() %>% sort()

CPI_list_clean <-
  CPI %>% CPI_split() %>% set_names(country_region_names) %>% map(CPI_clean)

# RDS Export --------------------------------------------------------------
list_RDS_export(CPI_list_clean, here("SA_CPI.rds"))
