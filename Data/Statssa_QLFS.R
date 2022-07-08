library("tidyverse")
library("lubridate")
library("rebus")
library("xts")
library("here")
library("filesstrings")
library("readxl")
library("xts")

# Functions ---------------------------------------------------------------
publication_lag <- today() - months(3)
quarter_names <-
  as.character(as.yearqtr(seq(
    as.Date("2008-01-01"), publication_lag, by = "quarter"
  ))) %>% prepend("Description")
table_names <- function(path) {
  data <- path %>%
    excel_sheets() %>%
    purrr::set_names() %>%
    map(read_excel,
        col_names = FALSE,
        path = path,
        n_max = 1)
  data <- do.call(rbind, data)
  pattern <-
    START %R%
    one_or_more(WRD) %R%
    SPC %R%
    ascii_digit() %R%
    optional(".") %R%
    optional(ascii_digit()) %R%
    optional(WRD) %R%
    optional(":") %R%
    SPC
  names <- str_replace_all(data$...1, pattern, "")
  
}
QLFS_import <- function(path) {
  data <- path %>%
    excel_sheets() %>%
    map(
      read_excel,
      col_names = FALSE ,
      path = path,
      skip = 1 ,
      col_types = c("text", rep("numeric", 51))
    )
}

QLFS_table_clean <- function(data, case_criteria, case_filter) {
  data %>% mutate (
    Category = case_when(!!!case_criteria),
    Category = na.locf(Category, na.rm = TRUE)
  ) %>% filter(!!!case_filter) %>% drop_na()
  
}

list_RDS_export <- function (export_list, name) {
  saveRDS(export_list, file = name)
}

# Data Download -----------------------------------------------------------
base_url <-
  "http://www.statssa.gov.za/publications/P0211/QLFS%20Trends%202008-"
suppl_url <- ".xlsx"
year <- year(Sys.Date())
publication_lag <-
  months(3) # current publication lag *may need to change depending on the year change issue*
quarter <- quarter(Sys.Date() - publication_lag)
year_quarter <- paste0(year, "Q", quarter)
url <- paste0(base_url, year_quarter, suppl_url)
download.file(url, "SA_Unemployment.xlsx", method = "auto")

# Data Cleaning -----------------------------------------------------------
names <- table_names(here("SA_Unemployment.xlsx"))
QLFS <-
  QLFS_import(here("SA_Unemployment.xlsx")) %>% set_names(names)
oldnames <- colnames(QLFS$`Population of working age (15-64 years)`)
publication_lag <- today() - months(3)
quarter_names <-
  as.character(seq(as.Date("2008-01-01"), publication_lag, by = "quarter")) %>% prepend("Description")
QLFS <-
  QLFS %>% map(rename_at, vars(oldnames), ~ quarter_names) %>% map( ~ pivot_longer(
    .x,
    starts_with("20"),
    names_to = "Period",
    values_to = "Value"
  )) %>%  map( ~ mutate(., Period = parse_date_time(Period, "Ymd"))) %>%  map(., drop_na, Description)

# Data Cleaning - Working age table -----------------------------------------
criteria_working_age <- rlang::exprs(
  .data$Description == "Both sexes" ~ "gender",
  .data$Description == "Population groups" ~ "population_group",
  .data$Description == "South Africa" ~ "province"
)
filter_working_age <- rlang::exprs(
  .data$Description != "Both sexes" &
    .data$Description !=  "Population groups" &
    .data$Description != "South Africa"
)

QLFS$`Population of working age (15-64 years)` <-
  QLFS_table_clean(
    QLFS$`Population of working age (15-64 years)`,
    criteria_working_age,
    filter_working_age
  )


# Data Cleaning - Characteristics by sex ----------------------------------
criteria_characteristics_sex_all <- rlang::exprs(
  .data$Description == "Both sexes" ~ "both_sexes",
  .data$Description == "Women" ~ "women",
  .data$Description == "Men" ~ "men"
)
filter_characteristics_sex_all <- rlang::exprs(
  .data$Description != "Both sexes" &
    .data$Description !=  "Women" &
    .data$Description != "Men"
)

QLFS$`Labour force characteristics by sex - All population groups` <-
  QLFS_table_clean(
    QLFS$`Labour force characteristics by sex - All population groups`,
    criteria_characteristics_sex_all,
    filter_characteristics_sex_all
  )


# Data Cleaning - Charactateristics by population group -------------------
criteria_characteristics_group <- rlang::exprs(
  .data$Description == "South Africa" ~ "all_groups",
  .data$Description == "Black/African" ~ "black/african",
  .data$Description == "Coloured" ~ "coloured",
  .data$Description == "Indian/Asian" ~ "indian/asian",
  .data$Description == "White" ~ "white"
)
filter_characteristics_group <- rlang::exprs(
  .data$Description != "South Africa" &
    .data$Description != "Black/African" &
    .data$Description != "Coloured" &
    .data$Description != "Indian/Asian"  &
    .data$Description != "White"
)
QLFS$`Labour force characteristics by population group` <-
  QLFS_table_clean(
    QLFS$`Labour force characteristics by population group`,
    criteria_characteristics_group,
    filter_characteristics_group
  )


# Data Cleaning - Characteristics by age group ----------------------------
criteria_characteristics_age_group <- rlang::exprs(
  .data$Description == "15-64 years" ~ "15-64 years",
  .data$Description == "15-24 years"  ~ "15-24 years" ,
  .data$Description == "25-34 years" ~ "25-34 years",
  .data$Description == "45-54 years" ~ "45-54 years",
  .data$Description == "55-64 years"  ~ "55-64 years"
)
filter_characteristics_age_group <- rlang::exprs(
  .data$Description !=   "15-64 years" &
    .data$Description != "15-24 years" &
    .data$Description != "25-34 years" &
    .data$Description != "45-54 years" &
    .data$Description != "55-64 years"
)
QLFS$`Labour force characteristics by age group` <-
  QLFS_table_clean(
    QLFS$`Labour force characteristics by age group`,
    criteria_characteristics_age_group,
    filter_characteristics_age_group
  )


# Data Cleaning - Characteristics by province and metro -------------------

criteria_characteristics_province_metro <- rlang::exprs(
  .data$Description == "South Africa" ~ "south_africa",
  .data$Description == "Western Cape"  ~ "western_cape",
  .data$Description == "Western Cape - Non metro"  ~ "western_cape_non metro",
  .data$Description == "Western Cape - City of cape Town" ~ "western_cape_city_of_cape_town",
  .data$Description ==  "Eastern Cape" ~ "eastern_cape",
  .data$Description ==  "Eastern Cape - Non Metro" ~ "eastern_cape_non Metro",
  .data$Description ==  "Eastern Cape - Nelson mandela Bay" ~ "eastern_cape_nelson_mandela_bay",
  .data$Description ==  "Northern Cape" ~ "northern_cape",
  .data$Description ==  "Free State" ~ "Free State",
  .data$Description ==  "Free State - Non Metro" ~ "free_state_non_metro",
  .data$Description ==  "Free State - Mangaung" ~ "free_state_mangaung",
  .data$Description ==  "KwaZulu Natal" ~ "kwazulu_natal",
  .data$Description ==  "KwaZulu-Natal - Non Metro" ~ "kwazulu_natal_non_metro",
  .data$Description ==  "KwaZulu-Natal - eThekwini" ~ "kwazulu_natal_eThekwini",
  .data$Description ==  "North West"   ~ "north_west",
  .data$Description ==  "Gauteng" ~ "gauteng",
  .data$Description ==  "Gauteng - Non Metro" ~ "gauteng_non_metro",
  .data$Description ==  "Gauteng - Ekurhuleni" ~ "gauteng_ekurhuleni",
  .data$Description ==  "Gauteng - City of Johannesburg" ~ "gauteng_city_of_johannesburg",
  .data$Description ==  "Gauteng - City of Tshwane" ~ "gauteng_city_of_tshwane",
  .data$Description ==  "Mpumalanga"  ~  "mpumalanga" ,
  .data$Description ==  "Limpopo" ~ "limpopo"
)

filter_characteristics_province_metro <- rlang::exprs(
  .data$Description != "South Africa" &
    .data$Description != "Western Cape" &
    .data$Description != "Western Cape - Non metro" &
    .data$Description != "Western Cape - City of cape Town" &
    .data$Description !=  "Eastern Cape" &
    .data$Description !=  "Eastern Cape - Non Metro" &
    .data$Description !=  "Eastern Cape - Nelson mandela Bay" &
    .data$Description !=  "Northern Cape" &
    .data$Description !=  "Free State" &
    .data$Description !=  "Free State - Non Metro" &
    .data$Description !=  "Free State - Mangaung" &
    .data$Description !=  "KwaZulu Natal" &
    .data$Description !=  "KwaZulu-Natal - Non Metro" &
    .data$Description !=  "KwaZulu-Natal - eThekwini" &
    .data$Description !=  "North West" &
    .data$Description !=  "Gauteng" &
    .data$Description !=  "Gauteng - Non Metro" &
    .data$Description !=  "Gauteng - Ekurhuleni" &
    .data$Description !=  "Gauteng - City of Johannesburg" &
    .data$Description !=  "Gauteng - City of Tshwane" &
    .data$Description !=  "Mpumalanga"  &
    .data$Description !=  "Limpopo"
)


QLFS$`Labour force characteristics by province and metro` <-
  QLFS_table_clean(
    QLFS$`Labour force characteristics by province and metro`,
    criteria_characteristics_province_metro,
    filter_characteristics_province_metro
  )


# Data Cleaning - Characteristics by sex expanded ----------------------------------

criteria_characteristics_sex_all_expanded <- rlang::exprs(
  .data$Description == "Both sexes" ~ "both_sexes",
  .data$Description == "Women" ~ "women",
  .data$Description == "Men" ~ "men"
)
filter_characteristics_sex_all_expanded <- rlang::exprs(
  .data$Description != "Both sexes" &
    .data$Description !=  "Women" &
    .data$Description != "Men"
)

QLFS$`Labour force characteristics by sex - Expanded definition of unemployment` <-
  QLFS_table_clean(
    QLFS$`Labour force characteristics by sex - Expanded definition of unemployment`,
    criteria_characteristics_sex_all_expanded,
    filter_characteristics_sex_all_expanded
  )

# Data Cleaning - Characteristics by population group expanded -------------------
criteria_characteristics_group_expanded <- rlang::exprs(
  .data$Description == "South Africa" ~ "all_groups",
  .data$Description == "Black/African" ~ "black/african",
  .data$Description == "Coloured" ~ "coloured",
  .data$Description == "Indian/Asian" ~ "indian/asian",
  .data$Description == "White" ~ "white"
)
filter_characteristics_group_expanded <- rlang::exprs(
  .data$Description != "South Africa" &
    .data$Description != "Black/African" &
    .data$Description != "Coloured" &
    .data$Description != "Indian/Asian"  &
    .data$Description != "White"
)
QLFS$`Labour force characteristics by population group - Expanded definition of unemployment` <-
  QLFS_table_clean(
    QLFS$`Labour force characteristics by population group - Expanded definition of unemployment`,
    criteria_characteristics_group_expanded,
    filter_characteristics_group_expanded
  )

# Data Cleaning - Characteristics by age group expanded ----------------------------
criteria_characteristics_age_group_expanded <- rlang::exprs(
  .data$Description == "15-64 years" ~ "15-64 years",
  .data$Description == "15-24 years"  ~ "15-24 years" ,
  .data$Description == "25-34 years" ~ "25-34 years",
  .data$Description == "45-54 years" ~ "45-54 years",
  .data$Description == "55-64 years"  ~ "55-64 years"
)
filter_characteristics_age_group_expanded <- rlang::exprs(
  .data$Description !=   "15-64 years" &
    .data$Description != "15-24 years" &
    .data$Description != "25-34 years" &
    .data$Description != "45-54 years" &
    .data$Description != "55-64 years"
)
QLFS$`Labour force characteristics by age group - Expanded definition of unemployment` <-
  QLFS_table_clean(
    QLFS$`Labour force characteristics by age group - Expanded definition of unemployment`,
    criteria_characteristics_age_group_expanded,
    filter_characteristics_age_group_expanded
  )

# Data Cleaning - Characteristics by province and metro expanded -------------------

criteria_characteristics_province_metro_expanded <- rlang::exprs(
  .data$Description == "South Africa" ~ "south_africa",
  .data$Description == "Western Cape"  ~ "western_cape",
  .data$Description == "Western Cape - Non metro"  ~ "western_cape_non metro",
  .data$Description == "Western Cape - City of cape Town" ~ "western_cape_city_of_cape_town",
  .data$Description ==  "Eastern Cape" ~ "eastern_cape",
  .data$Description ==  "Eastern Cape - Non Metro" ~ "eastern_cape_non Metro",
  .data$Description ==  "Eastern Cape - Nelson mandela Bay" ~ "eastern_cape_nelson_mandela_bay",
  .data$Description ==  "Northern Cape" ~ "northern_cape",
  .data$Description ==  "Free State" ~ "Free State",
  .data$Description ==  "Free State - Non Metro" ~ "free_state_non_metro",
  .data$Description ==  "Free State - Mangaung" ~ "free_state_mangaung",
  .data$Description ==  "KwaZulu Natal" ~ "kwazulu_natal",
  .data$Description ==  "KwaZulu-Natal - Non Metro" ~ "kwazulu_natal_non_metro",
  .data$Description ==  "KwaZulu-Natal - eThekwini" ~ "kwazulu_natal_eThekwini",
  .data$Description ==  "North West"   ~ "north_west",
  .data$Description ==  "Gauteng" ~ "gauteng",
  .data$Description ==  "Gauteng - Non Metro" ~ "gauteng_non_metro",
  .data$Description ==  "Gauteng - Ekurhuleni" ~ "gauteng_ekurhuleni",
  .data$Description ==  "Gauteng - City of Johannesburg" ~ "gauteng_city_of_johannesburg",
  .data$Description ==  "Gauteng - City of Tshwane" ~ "gauteng_city_of_tshwane",
  .data$Description ==  "Mpumalanga"  ~  "mpumalanga" ,
  .data$Description ==  "Limpopo" ~ "limpopo"
)

filter_characteristics_province_metro_expanded <- rlang::exprs(
  .data$Description != "South Africa" &
    .data$Description != "Western Cape" &
    .data$Description != "Western Cape - Non metro" &
    .data$Description != "Western Cape - City of cape Town" &
    .data$Description !=  "Eastern Cape" &
    .data$Description !=  "Eastern Cape - Non Metro" &
    .data$Description !=  "Eastern Cape - Nelson mandela Bay" &
    .data$Description !=  "Northern Cape" &
    .data$Description !=  "Free State" &
    .data$Description !=  "Free State - Non Metro" &
    .data$Description !=  "Free State - Mangaung" &
    .data$Description !=  "KwaZulu Natal" &
    .data$Description !=  "KwaZulu-Natal - Non Metro" &
    .data$Description !=  "KwaZulu-Natal - eThekwini" &
    .data$Description !=  "North West" &
    .data$Description !=  "Gauteng" &
    .data$Description !=  "Gauteng - Non Metro" &
    .data$Description !=  "Gauteng - Ekurhuleni" &
    .data$Description !=  "Gauteng - City of Johannesburg" &
    .data$Description !=  "Gauteng - City of Tshwane" &
    .data$Description !=  "Mpumalanga"  &
    .data$Description !=  "Limpopo"
)


QLFS$`Labour force characteristics by province and metro - Expanded definition of unemployment` <-
  QLFS_table_clean(
    QLFS$`Labour force characteristics by province and metro - Expanded definition of unemployment`,
    criteria_characteristics_province_metro,
    filter_characteristics_province_metro
  )

# Data Cleaning - Employed by industry and sex south africa ----------------------------------
criteria_characteristics_sex_all_industry <- rlang::exprs(
  .data$Description == "Both sexes" ~ "both_sexes",
  .data$Description == "Women" ~ "women",
  .data$Description == "Men" ~ "men"
)
filter_characteristics_sex_all_industry <- rlang::exprs(
  .data$Description != "Both sexes" &
    .data$Description !=  "Women" &
    .data$Description != "Men"
)

QLFS$`Employed by industry and sex - South Africa` <-
  QLFS_table_clean(
    QLFS$`Employed by industry and sex - South Africa`,
    criteria_characteristics_sex_all_industry,
    filter_characteristics_sex_all_industry
  )

# Data Cleaning - Employed by industry and province ----------------------------------

criteria_employed_industry_province <- rlang::exprs(
  .data$Description == "Agriculture"  ~ "agriculture",
  .data$Description ==  "Mining"    ~ "mining"  ,
  .data$Description ==  "Manufacturing" ~ "manufacturing",
  .data$Description ==  "Utilities" ~ "utilities"  ,
  .data$Description ==  "Construction" ~ "construction",
  .data$Description ==  "Trade" ~ "trade",
  .data$Description ==  "Transport" ~ "transport",
  .data$Description ==  "Finance" ~ "finance",
  .data$Description ==  "Private households" ~ "private_household"
)
filter_employed_industry_province <- rlang::exprs(
  .data$Description != "Agriculture"   &
    .data$Description !=  "Mining"       &
    .data$Description !=  "Manufacturing" &
    .data$Description !=  "Utilities"    &
    .data$Description !=  "Construction" &
    .data$Description !=  "Trade"        &
    .data$Description !=  "Transport"    &
    .data$Description !=  "Finance" &
    .data$Description !=  "Private households"
)

QLFS$`Employed by industry and province` <-
  QLFS_table_clean(
    QLFS$`Employed by industry and province`,
    criteria_employed_industry_province,
    filter_employed_industry_province
  )
# Data Cleaning - Employed by sector and industry south africa ----------------------------------
criteria_employed_sector_industry <- rlang::exprs(
  .data$Description == "Total employed"  ~ "total_employed",
  .data$Description ==  "Formal and informal sector (Non-agricultural)" ~ "formal_and_informal_non_agri"  ,
  .data$Description ==  "Formal sector (Non-agricultural)"  ~ "formal_sector_non_agri",
  .data$Description ==  "Informal sector (Non-agricultural)"  ~ "informal_sector_non_agri"  ,
  .data$Description ==  "Agriculture" ~ "agriculture",
  .data$Description ==  "Private households" ~ "private_household"
)
filter_employed_sector_industry <- rlang::exprs(
  .data$Description != "Total employed"  &
    .data$Description !=  "Formal and informal sector (Non-agricultural)"    &
    .data$Description !=  "Formal sector (Non-agricultural)" &
    .data$Description !=  "Informal sector (Non-agricultural)"    &
    .data$Description !=  "Agriculture" &
    .data$Description !=  "Private households"
)

QLFS$`Employed by sector and industry - South Africa` <-
  QLFS_table_clean(
    QLFS$`Employed by sector and industry - South Africa`,
    criteria_employed_sector_industry,
    filter_employed_sector_industry
  )

# Data Cleaning - Employed by province, metro and sector ----------------------------------

criteria_employed_province_metro_sector <- rlang::exprs(
  .data$Description == "South Africa" ~ "south_africa",
  .data$Description == "Western Cape"  ~ "western_cape",
  .data$Description == "Western Cape - Non metro"  ~ "western_cape_non metro",
  .data$Description == "Western Cape - City of cape Town" ~ "western_cape_city_of_cape_town",
  .data$Description ==  "Eastern Cape" ~ "eastern_cape",
  .data$Description ==  "Eastern Cape - Non Metro" ~ "eastern_cape_non Metro",
  .data$Description ==  "Eastern Cape - Nelson mandela Bay" ~ "eastern_cape_nelson_mandela_bay",
  .data$Description ==  "Northern Cape" ~ "northern_cape",
  .data$Description ==  "Free State" ~ "Free State",
  .data$Description ==  "Free State - Non Metro" ~ "free_state_non_metro",
  .data$Description ==  "Free State - Mangaung" ~ "free_state_mangaung",
  .data$Description ==  "KwaZulu Natal" ~ "kwazulu_natal",
  .data$Description ==  "KwaZulu-Natal - Non Metro" ~ "kwazulu_natal_non_metro",
  .data$Description ==  "KwaZulu-Natal - eThekwini" ~ "kwazulu_natal_eThekwini",
  .data$Description ==  "North West"   ~ "north_west",
  .data$Description ==  "Gauteng" ~ "gauteng",
  .data$Description ==  "Gauteng - Non Metro" ~ "gauteng_non_metro",
  .data$Description ==  "Gauteng - Ekurhuleni" ~ "gauteng_ekurhuleni",
  .data$Description ==  "Gauteng - City of Johannesburg" ~ "gauteng_city_of_johannesburg",
  .data$Description ==  "Gauteng - City of Tshwane" ~ "gauteng_city_of_tshwane",
  .data$Description ==  "Mpumalanga"  ~  "mpumalanga" ,
  .data$Description ==  "Limpopo" ~ "limpopo"
)

filter_employed_province_metro_sector <- rlang::exprs(
  .data$Description != "South Africa" &
    .data$Description != "Western Cape" &
    .data$Description != "Western Cape - Non metro" &
    .data$Description != "Western Cape - City of cape Town" &
    .data$Description !=  "Eastern Cape" &
    .data$Description !=  "Eastern Cape - Non Metro" &
    .data$Description !=  "Eastern Cape - Nelson mandela Bay" &
    .data$Description !=  "Northern Cape" &
    .data$Description !=  "Free State" &
    .data$Description !=  "Free State - Non Metro" &
    .data$Description !=  "Free State - Mangaung" &
    .data$Description !=  "KwaZulu Natal" &
    .data$Description !=  "KwaZulu-Natal - Non Metro" &
    .data$Description !=  "KwaZulu-Natal - eThekwini" &
    .data$Description !=  "North West" &
    .data$Description !=  "Gauteng" &
    .data$Description !=  "Gauteng - Non Metro" &
    .data$Description !=  "Gauteng - Ekurhuleni" &
    .data$Description !=  "Gauteng - City of Johannesburg" &
    .data$Description !=  "Gauteng - City of Tshwane" &
    .data$Description !=  "Mpumalanga"  &
    .data$Description !=  "Limpopo"
)

QLFS$`Employed by province, metro and sector` <-
  QLFS_table_clean(
    QLFS$`Employed by province, metro and sector`,
    criteria_employed_province_metro_sector,
    filter_employed_province_metro_sector
  )

# Data Cleaning - Employed by sex and occupation south africa ----------------------------------
criteria_employed_sex_occupation <- rlang::exprs(
  .data$Description == "Both sexes" ~ "both_sexes",
  .data$Description == "Women" ~ "women",
  .data$Description == "Men" ~ "men"
)
filter_employed_sex_occupation <- rlang::exprs(
  .data$Description != "Both sexes" &
    .data$Description !=  "Women" &
    .data$Description != "Men"
)

QLFS$`Employed by sex and occupation - South Africa` <-
  QLFS_table_clean(
    QLFS$`Employed by sex and occupation - South Africa`,
    criteria_employed_sex_occupation,
    filter_employed_sex_occupation
  )

# Data Cleaning - Employed by sex and status south africa ----------------------------------
criteria_employed_sex_status <- rlang::exprs(
  .data$Description == "Both sexes" ~ "both_sexes",
  .data$Description == "Women" ~ "women",
  .data$Description == "Men" ~ "men"
)
filter_employed_sex_status <- rlang::exprs(
  .data$Description != "Both sexes" &
    .data$Description !=  "Women" &
    .data$Description != "Men"
)

QLFS$`Employed by sex and status in employment - South Africa` <-
  QLFS_table_clean(
    QLFS$`Employed by sex and status in employment - South Africa`,
    criteria_employed_sex_status,
    filter_employed_sex_status
  )

# Data Cleaning - Employed by sex and working hours south africa ----------------------------------
criteria_employed_sex_working_hours <- rlang::exprs(
  .data$Description == "Both sexes" ~ "both_sexes",
  .data$Description == "Women" ~ "women",
  .data$Description == "Men" ~ "men"
)
filter_employed_sex_working_hours <- rlang::exprs(
  .data$Description != "Both sexes" &
    .data$Description !=  "Women" &
    .data$Description != "Men"
)

QLFS$`Employed by sex and usual hours of work - South Africa` <-
  QLFS_table_clean(
    QLFS$`Employed by sex and usual hours of work - South Africa`,
    criteria_employed_sex_working_hours,
    filter_employed_sex_working_hours
  )


# Data Cleaning - Characteristics of unemployed south africa --------------

criteria_characteristics_unemployed <- rlang::exprs(
  .data$Description == "Unemployed"  ~ "unemployed_type",
  .data$Description == "Unemployed" ~ "unemployed_term",
  .data$Description == "Long-term unemployment(%)"  ~ "long-term_unemployment(%)",
  .data$Description == "Previous occupation" ~ "Previous_occupation_worked_prior_5years",
  .data$Description == "Previous industry"  ~ "Previous_industry_worked_prior_5years"
)
filter_characteristics_unemployed <- rlang::exprs(
  .data$Description != "Unemployed" &
    .data$Description != "Unemployed" &
    .data$Description != "Long-term unemployment(%)" &
    .data$Description != "Previous occupation" &
    .data$Description != "Previous industry"
)

QLFS$`Characteristics of the unemployed - South Africa` <-
  QLFS_table_clean(
    QLFS$`Characteristics of the unemployed - South Africa`,
    criteria_characteristics_unemployed,
    filter_characteristics_unemployed
  )




# Data Cleaning - Characteristics of the not economically active  --------------------------------------------------------

criteria_characteristics_nea <- rlang::exprs(
  .data$Description == "Not economically active"  ~ "nea_type",
  .data$Description == "Inactivity rate by age (Both sexes)" ~ "inactivity_age_group_both_sexes",
  .data$Description == "Inactivity rate by age (Women)" ~ "inactivity_age_group_women",
  .data$Description == "Inactivity rate by age (Men)" ~ "inactivity_age_group_men"
)
filter_characteristics_nea <- rlang::exprs(
  .data$Description != "Not economically active" &
    .data$Description != "Inactivity rate by age (Both sexes)" &
    .data$Description != "Inactivity rate by age (Women)" &
    .data$Description != "Inactivity rate by age (Men)"
)

QLFS$`Characteristics of the not economically active - South Africa` <-
  QLFS_table_clean(
    QLFS$`Characteristics of the not economically active - South Africa`,
    criteria_characteristics_nea,
    filter_characteristics_nea
  )


# Data Cleaning - Socio-demographic characteristics south africa ------------------------
criteria_characteristics_socio <- rlang::exprs(
  .data$Description == "Age group of the employed" ~ "age_group_employed",
  .data$Description == "Age group of the unemployed" ~ "age_group_unemployed",
  .data$Description == "Age group of the not economically active"  ~ "age_group_not_economically_active",
  .data$Description == "Highest level of education of the employed" ~ "highest_level_education_employed",
  .data$Description == "Highest level of education of the unemployed"   ~ "highest_level_education_unemployed" ,
  .data$Description == "Highest level of education of the not economically active"  ~ "highest_level_education_not_economically_active" ,
  .data$Description == "Employed" ~ "employed",
  .data$Description == "Unemployed" ~ "unemployed",
  .data$Description == "Not economically active"  ~ "not_economically_active",
  .data$Description == "Current marital status of the employed"  ~ "current_marital_status_employed" ,
  .data$Description == "Current marital status of the unemployed"  ~ "current_marital_status_unemployed" ,
  .data$Description == "Current marital status of the not economically active" ~ "current_marital_status_not_economically_active"
)
filter_characteristics_socio <- rlang::exprs(
  .data$Description != "Age group of the employed" &
    .data$Description != "Age group of the unemployed" &
    .data$Description != "Age group of the not economically active"  &
    .data$Description != "Highest level of education of the employed" &
    .data$Description != "Highest level of education of the unemployed"   &
    .data$Description != "Highest level of education of the not economically active"  &
    .data$Description != "Employed" &
    .data$Description != "Unemployed" &
    .data$Description != "Not economically active" &
    .data$Description != "Current marital status of the employed"   &
    .data$Description != "Current marital status of the unemployed"   &
    .data$Description != "Current marital status of the not economically active"
)

QLFS$`Socio-demographic characteristics - South Africa` <-
  QLFS_table_clean(
    QLFS$`Socio-demographic characteristics - South Africa`,
    criteria_characteristics_socio,
    filter_characteristics_socio
  )



# Data Cleaning - Profile not in employment -------------------------------
criteria_profile_not_employed <- rlang::exprs(
  .data$Description == "Both sexes" ~ "gender",
  .data$Description == "Age group" ~ "age_group",
  .data$Description == "Population groups" ~ "population_group",
  .data$Description == "South Africa" ~ "province"
)
filter_profile_not_employed <- rlang::exprs(
  .data$Description != "Both sexes" &
    .data$Description != "Age group" &
    .data$Description != "Population groups"  &
    .data$Description != "South Africa"
)

QLFS$`Profile of those not in education and not in employment - South Africa` <-
  QLFS_table_clean(
    QLFS$`Profile of those not in education and not in employment - South Africa`,
    criteria_profile_not_employed,
    filter_profile_not_employed
  )







# Data Cleaning - Involvement in non-market activities and statsus by province ----------------------------------
criteria_involvement_non_market <- rlang::exprs(
  .data$Description == "South Africa" ~ "south_africa",
  .data$Description == "Western Cape"  ~ "western_cape",
  .data$Description ==  "Eastern Cape" ~ "eastern_cape",
  .data$Description ==  "Northern Cape" ~ "northern_cape",
  .data$Description ==  "Free State" ~ "Free State",
  .data$Description ==  "KwaZulu Natal" ~ "kwazulu_natal",
  .data$Description ==  "North West"   ~ "north_west",
  .data$Description ==  "Gauteng" ~ "gauteng",
  .data$Description ==  "Mpumalanga"  ~  "mpumalanga" ,
  .data$Description ==  "Limpopo" ~ "limpopo"
)
filter_involvement_non_market <- rlang::exprs(
  .data$Description != "South Africa" &
    .data$Description != "Western Cape" &
    .data$Description !=  "Eastern Cape" &
    .data$Description !=  "Northern Cape" &
    .data$Description !=  "Free State" &
    .data$Description !=  "KwaZulu Natal" &
    .data$Description !=  "North West" &
    .data$Description !=  "Gauteng" &
    .data$Description !=  "Mpumalanga"  &
    .data$Description !=  "Limpopo"
)

QLFS$`Involvement in non-market activities and labour market status by province` <-
  QLFS_table_clean(
    QLFS$`Involvement in non-market activities and labour market status by province`,
    criteria_involvement_non_market,
    filter_involvement_non_market
  )



# Data Cleaning - Tables not needed ---------------------------------------

QLFS$`Time-related underemployment - South Africa` <- NULL
QLFS$`Conditions of employment  by sex - South Africa` <- NULL
QLFS$`Conditions of employment by sex - South Africa` <- NULL
QLFS$`Conditions of employment by sex - South Africa` <- NULL

# RDS Export --------------------------------------------------------------
list_RDS_export(QLFS, here("QLFS_SA.rds"))
