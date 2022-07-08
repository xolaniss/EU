library("tidyverse")
library("lubridate")
library("rebus")
library("xts")
library("here")
library("filesstrings")
library("readxl")
library("xts")

# Functions ---------------------------------------------------------------
GDP_zip_excel_download <- function(url, filename) {
  download.file(url, paste0(filename, ".zip"), method = "auto")
  outDir <- here()
  unzip(paste0(filename, ".zip"), exdir = outDir)
  move_file <- here("EXCEL", "Excel table from 1993.xlsx")
  file.move(move_file, outDir , overwrite = TRUE)
  file.remove("EXCEL", recursive = TRUE)
  file.remove(here(paste0(filename, ".zip")))
  file.rename(here("Excel table from 1993.xlsx"), here(paste0(filename, ".xlsx")))
}
GDP_clean_up <- function(data) {
  data %>%
    pivot_longer(starts_with("QR"), "Quarter") %>%
    mutate(Quarter = str_replace_all(Quarter, "QR", ""),
           Quarter = as.yearqtr(parse_date_time(Quarter, "qY"))) %>%
    mutate(across(matches("H"), ~ str_replace_all(., "\n", " "))) %>%
    select(-c(H01, H04, H02, H03, H25)) %>%
    rename(
      Value = value,
      Name = H05,
      Type = H15,
      Level = H16,
      Unit = H17,
      Period = Quarter
    ) %>%
    unite("Description",
          Type:Level,
          sep = ", ",
          remove = TRUE) %>%
    mutate(Description = str_replace_all(Description, "At ", ""))
}
GDP_split <- function(data) {
  data %>%
    group_by(Name) %>%
    group_split(Name, .add = TRUE)
}
list_RDS_export <- function (export_list, name) {
  saveRDS(export_list, file = name)
}

# Data Download -----------------------------------------------------------
base_url <-
  "http://www.statssa.gov.za/timeseriesdata/Excel/P0441%20Gross%20Domestic%20Product%20(Quarterly)%20"
suppl_url <- ".zip"
year <- year(Sys.Date())
publication_lag <-
  months(5) # current publication lag *may need to change depending on the year change issue*
quarter <- quarter(Sys.Date() - publication_lag)
year_quarter <- paste0(year, "Q", quarter)
url <- paste0(base_url, "(", year_quarter, ")", suppl_url)
GDP_zip_excel_download(url, "GDP") #downloading GDP data

# Data Import -------------------------------------------------------------
SA_GDP <- read_excel(here("GDP.xlsx"), col_names = TRUE)

# Data Cleaning -----------------------------------------------------------
SA_GDP_clean <-
  SA_GDP  %>% GDP_clean_up()
SA_GDP_clean_category_names <-
  c(sort(unique(SA_GDP_clean$Name)))
SA_GDP_list  <-
  SA_GDP_clean %>% GDP_split() %>% set_names(SA_GDP_clean_category_names)


# RDS Export --------------------------------------------------------------
list_RDS_export(SA_GDP_list, here("SA_GDP.rds"))
