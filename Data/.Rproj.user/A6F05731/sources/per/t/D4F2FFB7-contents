library("tidyverse")
library("lubridate")
library("rebus")
library("xts")
library("here")
library("filesstrings")
library("readxl")


# Functions ---------------------------------------------------------------
retail_sales_zip_excel_download <-
  function(url, filename, filetype) {
    download.file(url, paste0(filename, ".zip"), method = "auto")
    outDir <- here()
    unzip(paste0(filename, ".zip"), exdir = outDir)
    move_file <- here("EXCEL", "Retail trade sales from 2002.xls")
    file.move(move_file, outDir , overwrite = TRUE)
    file.remove("EXCEL", recursive = TRUE)
    file.remove(here(paste0(filename, ".zip")))
    file.rename(here("Retail trade sales from 2002.xls"), here(paste0(filename, filetype)))
  }




retail_sales_clean <- function(data) {
  data %>%   select(-c("H01",
                       "H02",
                       "H03",
                       "H04",
                       "H25")) %>%
    pivot_longer(starts_with("M"), "Period", values_to = "Value") %>%
    mutate(Period = str_replace_all(Period, "MO", "")) %>%
    mutate(Period = parse_date_time(Period, "mY")) %>%
    replace_na(list(H05 = "Total")) %>%
    rename(
      Nominal = H15,
      Measure = H16,
      Scale = H17,
      Type = H05
    ) %>%
    group_by(Type) %>%
    group_split(Type)
}

list_RDS_export <- function (export_list, name) {
  saveRDS(export_list, file = name)
}
# URL ---------------------------------------------------------------------
base_url <-
  "http://www.statssa.gov.za/timeseriesdata/Excel/P6242.1%20Retail%20trade%20sales%20(New%20time%20series)%20from%20January%202002"
suppl_url <- ".zip"
publication_lag <-  month(2)
year <- year(Sys.Date())
month <- month(Sys.Date()) - publication_lag
url <- paste0(base_url, "_", year, month, suppl_url)

# Data Download -----------------------------------------------------------
#Retail_Sales <- retail_sales_zip_excel_download(url, "Retail_Sales", ".xls")



# Data Import -------------------------------------------------------------
Retail_Sales <- read_excel("Retail_Sales.xls", na = ".")



# Data Cleaning -----------------------------------------------------------
Retail_Sales_months <-
  Retail_Sales %>% select(starts_with("M")) %>% map_df(., as.numeric)
Retail_Sales_descriptions <-
  Retail_Sales %>% select(-starts_with("M"))
Retail_Sales <-
  tbl_df(cbind(Retail_Sales_descriptions, Retail_Sales_months)) %>%  replace_na(list(H05 = "Total"))

retailer_names <- sort(unique(Retail_Sales$H05))

Retail_sales_list_clean <-
  Retail_Sales %>% retail_sales_clean() %>% set_names(retailer_names)

# RDS Export --------------------------------------------------------------
list_RDS_export(Retail_sales_list_clean , here("SA_Retail_Sales.rds"))
