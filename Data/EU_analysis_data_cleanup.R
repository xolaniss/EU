#Preamble----
options("scipen" = 100, "digits" = "4")
#Clear Environment and Set WD
rm(list=ls())
setwd("/Users/xolanisibande/Desktop/EU_Analysis/Sweave/EU_2020_12/Data")
#Loading Packages
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(zoo)
library(ggplot2)
library(ggthemes)
library(stringr)
library(gridExtra)
library(knitr)
library(kableExtra)
library(magick)
library(stringi)
colors <- c ("#90bff4", "#71adf2", "#529bef", "#3389ec", "#1677e7", "#1367c8", "#1057a9", "#0d478a", "#0a376b", "#07274c" ) 
#Data Import----

#Sheet Import Function
Sheets_import<- function(path){
  data<- path %>%
    excel_sheets() %>%
    purrr::set_names() %>%
    map(read_excel,  col_names = FALSE, path=path)
}

# Importing Sheets
EU_tables <-Sheets_import("EU_Database.xlsx")

#Bloomberg Data ----

#Bloomberg GDP Data Cleaning Function
Bloomberg_clean <- function(data){
  df <- data %>%  
    select(-...1) %>%
    slice(2:n()) %>%
    subset(!is.na(...3)) %>%
    mutate_if(is.numeric, round, digits = 2) %>%
    mutate(...2 = str_replace(...2, "&", "and")) %>% 
    mutate(...2 = str_replace(...2, "CAPE VERDE", "Cape Verde")) %>% 
    mutate(...2 = str_replace(...2, "CHILE", "Chile")) %>% 
    mutate(...2 = str_replace(...2, "URUGUAY", "Uruguay")) %>% 
    mutate(...2 = str_replace(...2, "PERU", "Peru")) %>% 
    mutate(...2 = str_replace(...2, "SWITZERLAND", "Switzerland"))        
}
#Cleaning Bloomberg GDP Data
Bloomberg_tables <- list(EU_tables$Subsahara_Africa_GDP, EU_tables$Americas_GDP, EU_tables$AsiaOceania_GDP, EU_tables$Europe_GDP, EU_tables$Subsahara_Africa_CPI, EU_tables$Americas_CPI, EU_tables$AsiaOceania_CPI, EU_tables$Europe_CPI)
Bloomberg_tables[[3]][3] <- lapply(Bloomberg_tables[[3]][3], as.numeric) 
Bloomberg_tables[[7]][3] <- lapply(Bloomberg_tables[[7]][3], as.numeric) 
Bloomberg_tables_clean <- lapply(Bloomberg_tables, Bloomberg_clean)
names(Bloomberg_tables_clean) <- c("Subsahara_Africa_GDP", "Americas_GDP", "AsiaOceania_GDP", "Europe_GDP", "Subsahara_Africa_CPI", "Americas_CPI", "AsiaOceania_CPI", "Europe_CPI" )
Bloomberg_tables_clean$Americas_GDP$...2 <- str_replace(Bloomberg_tables_clean$Americas_GDP$...2, "United States of America", "United States")
Bloomberg_tables_clean$Americas_CPI$...2 <- str_replace(Bloomberg_tables_clean$Americas_CPI$...2, "United States of America", "United States")
#Date Conversion Functions---- 
Date_conversion_quarter <- function(data){
  df<- data %>%
    as.Date(origin = "1899-12-28") %>%
    as.yearqtr(format = "%Y-%m-%d")
}
Date_conversion_month <- function(data){
  df<- data %>%
    as.Date(origin = "1899-12-28") %>%
    as.yearmon(format = "%Y-%m-%d")
}

#South Africa Data ----

#South Africa Data Cleaning Function
South_Africa_clean <- function(data){
  df <-  data %>% 
    slice(8:n()) %>%
    lapply(as.numeric) %>%
    lapply(round, digits =2) %>%
    tbl_df()
}
#Cleaning South Africa Data
South_Africa_table <- list(EU_tables$South_Africa_GDP, EU_tables$South_Africa_CPI, EU_tables$South_Africa_BCI, EU_tables$South_Africa_Retail, EU_tables$South_Africa_CCI, EU_tables$South_Africa_Fuel_Prices, EU_tables$South_Africa_Vehicle_Sales, EU_tables$South_Africa_Commodity_Prices, EU_tables$South_Africa_current_account, EU_tables$South_Africa_exr, EU_tables$SA_investment)
South_Africa_table_clean <- lapply(South_Africa_table, South_Africa_clean)
names(South_Africa_table_clean) <- c("South_Africa_GDP", "South_Africa_CPI", "South_Africa_BCI", "South_Africa_Retail_Sales", "South_Africa_CCI", "South_Africa_Fuel_Prices", "South_Africa_Vehicle_Sales", "South_Africa_Commodity_Prices", "South_Africa_current_account", "South_Africa_exr", "South_Africa_inv")
#Date Conversion
South_Africa_names <- names(South_Africa_table_clean)
South_Africa_quarter_names <- South_Africa_names[c(1, 3, 5, 9, 10, 11)]
South_Africa_month_names <- South_Africa_names[c(2, 4, 6, 7, 8)]
for(i in South_Africa_quarter_names){
  South_Africa_table_clean[[i]][1] <- South_Africa_table_clean[[i]][1] %>% map(Date_conversion_quarter)
}
for(i in South_Africa_month_names){
  South_Africa_table_clean[[i]][1] <- South_Africa_table_clean[[i]][1] %>% map(Date_conversion_month)
}

#Provincial Data ----
#Provincial Data Function
Province_clean <- function(data){
  df<- data %>%
    slice(8:n()) %>%
    lapply(as.numeric) %>%
    lapply(round, digits =2) %>%
    tbl_df()
}
#Cleaning Provincial Data
Provincial_table <- list(EU_tables$Province_GDP, EU_tables$Province_CPI, EU_tables$Province_Unemployment, EU_tables$Province_CCI)
Provincial_table_clean <- lapply(Provincial_table, Province_clean)
names(Provincial_table_clean) <- c("Province_GDP", "Province_CPI", "Province_Unemployment", "Province_CCI")
#Date Conversion
provincial_names <- names(Provincial_table_clean)
provincial_quarter_names <- provincial_names[c(1, 3, 4)]
provincial_month_names <- provincial_names[c(2)]
for(i in provincial_quarter_names){
  Provincial_table_clean[[i]][1] <- Provincial_table_clean[[i]][1] %>% map(Date_conversion_quarter)
}
for(i in provincial_month_names){
  Provincial_table_clean[[i]][1] <- Provincial_table_clean[[i]][1] %>% map(Date_conversion_month)
}                     

#Labour Profile Data----
#South Africa Labour Profile Function
South_Africa_labour_clean <- function(data){
  df <- data %>% 
    slice(2:n()) %>%
    select(-c(...2, ...4:...22)) %>%
    mutate_if(is.numeric, round, digits = 2) %>%
    mutate(...1 = str_replace(...1, "Labour force characteristics:", "")) %>%
    mutate(...3 = str_replace(...3, "%", "Percent")) %>%
    mutate(...3 = str_replace(...3, " % ", " Percent ")) %>%
    mutate(...3 = str_replace(...3, "Thousands \\(Year on Year Percent Change\\)", "\\% Change \\(Year on Year\\)")) %>%
    mutate(...3 = str_replace(...3, "Percent \\(Year on Year Percent Change\\)", "\\% Change \\(Year on Year\\)"))
}
#Cleaning South Africa Labour Profile Data
South_Africa_labour_profile_clean <- South_Africa_labour_clean(EU_tables$South_Africa_Labour_Profile)
South_Africa_labour_profile_clean$...1[stri_duplicated(South_Africa_labour_profile_clean$...1)]<-""
#Province Labour Profile Function
Province_labour_clean <- function(data){
  df <- data %>% 
    slice(2:n()) %>%
    select(-c(...2, ...4:...22)) %>%
    mutate_if(is.numeric, round, digits = 2) %>%
    mutate(...1 = str_replace(...1, "Labour force characteristics by province and metro: ", "")) %>%
    mutate(...3 = str_replace(...3, "%", "Percent")) %>%
    mutate(...1 = str_replace(...1, "Population of working age \\(15–64 years\\): ", "")) %>%
    mutate(...1 = str_replace(...1, " Gauteng", "")) %>%
    mutate(...3 = str_replace(...3, " % ", " Percent "))  %>%
    mutate(...3 = str_replace(...3, "Thousands \\(Year on Year Percent Change\\)", "\\% Change \\(Year on Year\\)")) %>%
    mutate(...3 = str_replace(...3, "Percent \\(Year on Year Percent Change\\)", "\\% Change \\(Year on Year\\)"))
  
}
#Cleaning Province Labour Profile Data
Province_labour_profile_clean <- Province_labour_clean(EU_tables$Province_labour_profile)
Province_labour_profile_clean$...1[stri_duplicated(Province_labour_profile_clean$...1)]<-""
#Employment Data ----
#Employment Data Cleaning Function
Employment_clean <- function(data){
  df <- data %>% 
    slice(2:n()) %>%
    select(-c(...2, ...4:...22)) %>%
    mutate_if(is.numeric, round, digits = 2) %>%
    mutate(...1 = str_replace(...1, "Labour force characteristics:", "")) %>%
    mutate(...3 = str_replace(...3, "%", "Percent")) %>%
    mutate(...3 = str_replace(...3, " % ", " Percent ")) %>%
    mutate(...1 = str_replace(...1, "Employed by industry: Industry", "")) %>%
    mutate(...1 = str_replace(...1, "Employed by industry and province: Industry", "")) %>%
    mutate(...1 = str_replace(...1, " Gauteng", "")) %>%
    mutate(...3 = str_replace(...3, "Thousands \\(Period on Period Percent Change\\)", "\\% Change"))
} 

#Cleaning Employment Data
Employment_industry <- list (EU_tables$South_AfricaEmployment_Industry, EU_tables$Province_employment_industry)
Employment_industry_clean <- lapply(Employment_industry, Employment_clean)
names(Employment_industry_clean) <- c("South_Africa_Employment_Industry", "Province_employment_industry")

Employment_industry_clean$South_Africa_Employment_Industry$...1[stri_duplicated(Employment_industry_clean$South_Africa_Employment_Industry$...1)]<-""
Employment_industry_clean$Province_employment_industry$...1[stri_duplicated(Employment_industry_clean$Province_employment_industry$...1)]<-""


#Industry Growth Data ----
#Industry Growth Cleaning Function
Industry_growth_clean <- function(data){
  df <- data %>% 
    slice(2:n()) %>%
    select(-c(...2, ...4:...22)) %>%
    mutate_if(is.numeric, round, digits = 2) %>%
    mutate(...1 = str_replace(...1, "Industry value added and GDP: ", "")) %>%
    mutate(...1 = str_replace(...1, "&", "and")) %>%
    mutate(...1 = str_replace(...1, " & ", " and "))%>%
    mutate(...1 = str_replace(...1, "P7: Gauteng — GVA01: ", "")) %>%
    mutate(...1 = str_replace(...1, "P7: Gauteng — GVA02: ", "")) %>%
    mutate(...1 = str_replace(...1, "P7: Gauteng — GVA03: ", "")) %>%
    mutate(...1 = str_replace(...1, "P7: Gauteng — GVA04: ", "")) %>%
    mutate(...1 = str_replace(...1, "P7: Gauteng — GVA05: ", "")) %>%
    mutate(...1 = str_replace(...1, "P7: Gauteng — GVA06: ", "")) %>%
    mutate(...1 = str_replace(...1, "P7: Gauteng — GVA07: ", "")) %>%
    mutate(...1 = str_replace(...1, "P7: Gauteng — GVA08: ", "")) %>%
    mutate(...1 = str_replace(...1, "P7: Gauteng — GVA09: ", "")) %>%
    mutate(...1 = str_replace(...1, "P7: Gauteng — GVA10: ", "")) %>%
    mutate(...3 = str_replace(...3, "%", "Percent")) %>%
    mutate(...3 = str_replace(...3, " % ", " Percent ")) %>%
    mutate(...1 = str_replace(...1, ", seasonally adjusted and annualised", "")) %>%
    mutate(...3 = str_replace(...3, ", seasonally adjusted and annualised", "")) %>%
    mutate(...1 = str_replace(...1, "— R million at constant 2010 prices", "")) %>%
    mutate(...3 = str_replace(...3, "Constant 2010 prices:", "")) %>%
    mutate(...3 = str_replace(...3, " at constant 2010 prices", "")) %>%
    mutate(...3 = str_replace(...3, "Seasonally adjusted", "")) %>%
    mutate(...3 = str_replace(...3, "R million", "")) %>%
    mutate(...3 = str_replace(...3, "\\(\\) ", "")) %>%
    mutate(...3 = str_replace(...3, "\\(", "")) %>%
    mutate(...1 = str_replace(...1, " at basic prices", "")) %>%
    mutate(...1 = str_replace(...1, ": Gross value added \\(GVA\\)", "")) %>%
    mutate(...3 = str_replace(...3, "\\)", ""))  %>%
    mutate(...3 = str_replace(...3, "Period on Period Percent Change", "% Change")) %>%
   mutate(...3 = str_replace(...3, "Year on Year Percent Change", "% Change \\(Year on Year\\)"))
}

#Cleaning Industry Growth Data
Industry_Growth <- list(EU_tables$South_Africa_growth_industry, EU_tables$Province_growth_industry)
Industry_Growth_Clean <- lapply(Industry_Growth, Industry_growth_clean)
names(Industry_Growth_Clean) <- c("South_Africa_Industry_Growth", "Province_Industry_Growth")
Industry_Growth_Clean$South_Africa_Industry_Growth$...1[stri_duplicated(Industry_Growth_Clean$South_Africa_Industry_Growth$...1)]<-""
Industry_Growth_Clean$Province_Industry_Growth[2, 1] <- " "
Industry_Growth_Clean$Province_Industry_Growth$...1[stri_duplicated(Industry_Growth_Clean$South_Africa_Industry_Growth$...1)]<-""

#Renaming ----

  #Bloomberg tables
Bloom_start_date <- "2016-07-01"
Bloom_end_date <- "2019-07-01"

quarters <-  as.yearqtr(seq(as.Date(Bloom_start_date), as.Date(Bloom_end_date), by = "quarters"), format = "%YQ%q")
bloomberg_names <-  c(" ", as.character(quarters))

rename <- function(data){
 names(data) <- bloomberg_names
}

#Renaming Bloomberg GDP Data Columns
Bloomberg_tables_clean_rename <- lapply(Bloomberg_tables_clean, setNames, bloomberg_names)

 
#Renaming South Africa Data Columns'
SA_GDP_names <- str_c(c("Period", "R million" ,"Annualised Quarter on Quarter", "Quarter on Quarter", "Year on Year"), sep = "," )
SA_CPI_names <- str_c(c("Period", "All Items", "Food and alcohol beverages", "Electricity and Other Fuels", "Water and Other Services", "Transport") , sep = ",")
SA_BCI_names <- str_c (c("Period", "Index", "Quarter to Quarter % Change", "Year to Year % Change"), sep = ",")
SA_CCI_names <- str_c (c("Period", "Index", "Month to Month % Change", "Annualised Month to Month % Change", "Year to Year % Change"), sep = ",")
SA_Retail_names <- str_c(c("Period", "R million", "Month to Month % Change", "Annualised Month to Month % Change", "Year to Year % Change"), sep = ",")
SA_Vehicle_names <- str_c(c("Period", "Total Sales", "Month to Month % Change", "Year to Year % Change", "Export Sales", "Month to Month % Change", "Year to Year % Change"), sep = ",")
SA_Commodities_names <- str_c(c("Period", "Gold (Rand)", "Month to Month % Change", "Platinum (Rand)",  "Month to Month % Change", "Brent Crude (Rand)", "Month to Month % Change"), sep = ",")
SA_Fuel_names <- str_c(c("Period", "93 Octane Unleaded", "95 Octane Unleaded", "Diesel - 0.005% Sulphur", "Diesel - 0.05% Sulphur"), sep = ",")
SA_Exc_names <- str_c(c("Period", "SA Rand per Dollar", "Quarter to Quarter % Change", "SA Rand per Pound", "Quarter to Quarter % Change", "SA Rand per Euro", "Quarter to Quarter % Change", "SA Rand per Japanese Yen", "Quarter to Quarter % Change") , sep=",")
SA_CA_names <- str_c(c("Period","Current Account to GDP (%)", "Quarter to Quarter % Change", "Quarter to Quarter % Change Annualised", "Year to Year % Change"), sep = ",")
SA_inv_names <- str_c(c("Period", " General Government", "Public Corporations", "Private Business", "Total"))

South_Africa_table_clean_rename <- South_Africa_table_clean
names(South_Africa_table_clean_rename$South_Africa_GDP) <-SA_GDP_names
names(South_Africa_table_clean_rename$South_Africa_CPI) <- SA_CPI_names
names(South_Africa_table_clean_rename$South_Africa_BCI) <- SA_BCI_names
names(South_Africa_table_clean_rename$South_Africa_CCI) <- SA_CCI_names
names(South_Africa_table_clean_rename$South_Africa_Retail_Sales) <- SA_Retail_names
names(South_Africa_table_clean_rename$South_Africa_Vehicle_Sales) <- SA_Vehicle_names
names(South_Africa_table_clean_rename$South_Africa_Commodity_Prices) <- SA_Commodities_names
names(South_Africa_table_clean_rename$South_Africa_Fuel_Prices) <- SA_Fuel_names
names(South_Africa_table_clean_rename$South_Africa_exr) <- SA_Exc_names
names(South_Africa_table_clean_rename$South_Africa_current_account) <- SA_CA_names
names(South_Africa_table_clean_rename$South_Africa_inv) <- SA_inv_names

South_Africa_table_clean_rename$South_Africa_GDP$Period <- as.character(South_Africa_table_clean_rename$South_Africa_GDP$Period)
South_Africa_table_clean_rename$South_Africa_CPI$Period <- as.character(South_Africa_table_clean_rename$South_Africa_CPI$Period)
South_Africa_table_clean_rename$South_Africa_BCI$Period <- as.character( South_Africa_table_clean_rename$South_Africa_BCI$Period)
South_Africa_table_clean_rename$South_Africa_CCI$Period <- as.character(South_Africa_table_clean_rename$South_Africa_CCI$Period)
South_Africa_table_clean_rename$South_Africa_Fuel_Prices$Period <- as.character(South_Africa_table_clean_rename$South_Africa_Fuel_Prices$Period)
South_Africa_table_clean_rename$South_Africa_Vehicle_Sales$Period <- as.character(South_Africa_table_clean_rename$South_Africa_Vehicle_Sales$Period)
South_Africa_table_clean_rename$South_Africa_Commodity_Prices$Period <- as.character(South_Africa_table_clean_rename$South_Africa_Commodity_Prices$Period)
South_Africa_table_clean_rename$South_Africa_current_account$Period <- as.character(South_Africa_table_clean_rename$South_Africa_current_account$Period )
South_Africa_table_clean_rename$South_Africa_exr$Period <- as.character(South_Africa_table_clean_rename$South_Africa_exr$Period)
South_Africa_table_clean_rename$South_Africa_Retail_Sales$Period <- as.character(South_Africa_table_clean_rename$South_Africa_Retail_Sales$Period)

#Renaming Provincial Data Columns

Provincial_table_clean_rename <- Provincial_table_clean
Province_names_GDP <- str_c(c("Period", "Gauteng", "Western Cape", "Eastern Cape" , "Northen Cape", "Free State", "KwaZulu Natal", "North West", "Mpumalanga", "Limpopo" ), sep = ",")
Province_names_unemploy <- str_c(c("Period","Western Cape", "Eastern Cape" , "Northen Cape", "Free State", "KwaZulu Natal", "North West", "Gauteng","Mpumalanga", "Limpopo" ), sep = ",")
Province_names_CPI <- str_c(c("Period", "Free State", "Gauteng", "Eastern Cape", "KwaZulu Natal", "Limpopo", "Mpumalanga", "North West", "Northen Cape", "Western Cape" ), sep = ",")
names(Provincial_table_clean_rename$Province_GDP) <- Province_names_GDP
names(Provincial_table_clean_rename$Province_CPI) <- Province_names_CPI
names(Provincial_table_clean_rename$Province_Unemployment) <- Province_names_unemploy
names(Provincial_table_clean_rename$Province_CCI) <- SA_CCI_names 

Provincial_table_clean_rename$Province_GDP$Period <- as.character(Provincial_table_clean_rename$Province_GDP$Period)
Provincial_table_clean_rename$Province_Unemployment$Period <- as.character(Provincial_table_clean_rename$Province_Unemployment$Period)
Provincial_table_clean_rename$Province_CCI$Period <- as.character(Provincial_table_clean_rename$Province_CCI$Period)
Provincial_table_clean_rename$Province_CPI$Period <- as.character(Provincial_table_clean_rename$Province_CPI$Period)

#Renaming Labour Profile Data
industry_quarters <- as.yearqtr(seq(as.Date("2019-10-01"), as.Date("2020-07-01"), by = "quarters"), format = "%YQ%q")
Industry_names <- c("Sector", "Measure", as.character(industry_quarters))

Labour_profile_table_clean <- list(South_Africa_labour_profile_clean, Province_labour_profile_clean)
names(Labour_profile_table_clean) <- c("South_Africa_labour_profile", "Province_labour_profile")
Labour_profile_table_clean_rename <- lapply(Labour_profile_table_clean, setNames, Industry_names)

#Renaming Employment Data

Employment_industry_clean_rename <- lapply(Employment_industry_clean, setNames, Industry_names)

#Renaming Industry Growth Data

Industry_Growth_Clean_rename <- lapply(Industry_Growth_Clean,setNames, Industry_names)

#Exporting----

#List RDS Export Function
list_RDS_export <- function (export_list, name){
    saveRDS(export_list, file = name)
}

#Exporting Bloomberg Data to Excel
list_RDS_export(Bloomberg_tables_clean_rename, "Bloomberg.rds")
#Exporting South Africa Data to Excel
list_RDS_export(South_Africa_table_clean_rename, "South_africa.rds")
#Exporting Provincial Data to Excel
list_RDS_export(Provincial_table_clean_rename, "Provinces.rds")
#Exporting Labour Profile Data to Excel
list_RDS_export(Labour_profile_table_clean_rename, "Labour_profile.rds")
#Exporting Employment Data to Excel
list_RDS_export(Employment_industry_clean_rename, "Employment_industry.rds")
#Exporting Industry Growth Data to Excel
list_RDS_export(Industry_Growth_Clean_rename, "Industry.rds")








