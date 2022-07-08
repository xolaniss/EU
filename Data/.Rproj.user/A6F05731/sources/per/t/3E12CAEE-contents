library("lubridate")
library("tidyverse")
library("readxl")
library("naniar")
library("here")

# Functions ---------------------------------------------------------------
WEO_split <- function(database) {
  database %>%
    select(Country,
           Units,
           Scale,
           starts_with("19"),
           `Subject Descriptor`) %>%
    group_by(Country, .add = TRUE) %>%
    group_split(Country)
}
WEO_cleanup <- function (database) {
  database %>%
    pivot_longer(starts_with("19"), "Year") %>%
    replace_with_na_all(condition = ~ .x == "n/a") %>%
    mutate(
      Year = parse_date_time(Year, "Y"),
      value = str_replace_all(value, ",", ""),
      value =  as.numeric(value)
    ) %>%
    rename(Value = value, Description = `Subject Descriptor`)
}
list_RDS_export <- function (export_list, name) {
  saveRDS(export_list, file = name)
}

# URL ---------------------------------------------------------------------
year <- year(Sys.Date())
month <- month(Sys.Date())
edition <-
  case_when(month < 10 ~ "01", month  > 10 ~ "02")  #first or second edition
release <- "Oct2020" #abbreviated date
organisation_date_attach <- "WEO"
base_url <-
  "https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database"
suppl_url <- "all.ashx"

url <-
  paste0(base_url,
         "/",
         year,
         "/",
         edition,
         "/",
         organisation_date_attach,
         release,
         suppl_url)

# Data Download -----------------------------------------------------------
#download.file(url, "WEO.xls", method = "auto") #save data in excel as "xls" to allow for import

# Data Import -------------------------------------------------------------
WEO <- read_excel(here("WEO.xls"), col_names = TRUE)

# Data Cleanup ------------------------------------------------------------
WEO <-
  WEO %>% mutate(Country = str_replace_all(
    Country,
    c(
      "The " = "",
      "S„o TomÈ and PrÌncipe" = "Sao Tome and Principe",
      "Republic of Congo" = "Congo Republic",
      "CÙte d'Ivoire" = "Ivory Coast"
    )
  ))
country_names <- sort(unique(WEO$Country))
WEO_list_clean <-
  WEO %>%  WEO_split() %>% map(WEO_cleanup)
WEO_list_clean[[196]] <- NULL
WEO_list_clean <- WEO_list_clean %>% set_names(country_names)

# RDS Export --------------------------------------------------------------
list_RDS_export(WEO_list_clean, here("WEO_country.rds"))
