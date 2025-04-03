###########################################################################
# N. of Job posting and University students' number
# 3rd. April 2025
# Yuzuru Utsunomiya, Ph. D.
# 
# 
###########################################################################
# 
# ----- read.library -----
# Add requisite libraries here if necessary.
# Whenever you run this code, run here all.
library(tidyverse)

# -----read.data -----
# read an original data
# NOTE
# We use base pipe (|>). Its movement differs from one (%>%) by tidiverse().
df <- 
  readxl::read_excel(
    "engineering_majors.xlsx",
    sheet = "scraping_results"
    ) 

# ----- refine.data -----
# refine the data
engineering_majors <- 
  df |>
  # select necessary variables
  dplyr::select(position, company, scale, wage, experience, educational) |> 
  # combine province obtained from cities' name
  dplyr::bind_cols(
    readxl::read_excel(
      "engineering_majors.xlsx",
      sheet = "province_en")
  ) |> 
  # remove duplicated rows
  dplyr::distinct(company, .keep_all = TRUE) |> 
  dplyr::select(-position, -city) |> 
  # change types
  dplyr::mutate(across(where(is.character), factor)) |> 
  # provide new names for the original data
  data.table::setnames(c("company","size","wage","experience","education","province")) |> 
  dplyr::mutate(across(where(is.character), factor)) |> 
  # replace experience for convenience. 
  dplyr::mutate(
    experience = dplyr::case_when(
      experience == "1年以内" ~ "1_year_or_less",
      experience == "1-3年" ~ "1-3_years",
      experience == "3-5年" ~ "3-5_years",
      experience == "5-10年" ~ "5-10_years",
      experience == "10年以上" ~ "10_years_and_over",
      experience == "在校/应届" ~ "at_school",
      experience == "经验不限" ~ "no_requirement",
      TRUE ~ "hoge"
    ),
    # replace size for convenience. 
    size = dplyr::case_when(
      size == "0-20人" ~ "0-19_persons",
      size == "20-99人" ~ "20-99_persons",
      size == "100-499人" ~ "100-499_persons",
      size == "500-999人" ~ "500-999_persons",
      size == "1000-9999人" ~ "1000-9999_persons",
      size == "10000人以上" ~ "10000_persons_and_over",
      TRUE ~ "hoge"
    ),
    # replace educational attainment for convenience. 
    education = dplyr::case_when(
      education == "初中及以下" ~ "primary",
      education == "高中" ~ "secondary",
      education == "中专/中技" ~ "vocational_school",
      education == "大专" ~ "technical_college",
      education == "本科" ~ "bachelor",
      education == "硕士" ~ "master",
      education == "博士" ~ "doctor",
      education == "学历不限" ~ "no_requirement",
      TRUE ~ "hoge"
    )
  ) |> 
  # remove unnecessary strings
  dplyr::mutate(
    # K
    wage = stringr::str_remove_all(wage, "(?<=\\d-\\d{2})K"),
    # Yuan
    wage = stringr::str_remove_all(wage, "(?<=\\d-\\d{2})元"),
    # forward slash
    wage = stringr::str_remove_all(wage, "/")
  ) |> 
  # make a new variable indicating wage calculation unit
  dplyr::mutate(
    unit = dplyr::case_when(
      stringr::str_detect(wage, "天") ~ "day",
      stringr::str_detect(wage, "周") ~ "week",
      stringr::str_detect(wage, "月") ~ "month",
      stringr::str_detect(wage, "薪") ~ "year",
      TRUE ~ "month"
    )
  ) |> 
  # split wage amount range into lower wage and and upper wage 
  dplyr::mutate(
    # Extract raw numbers
    lower_raw = as.numeric(str_extract(wage, "^\\d+")),
    upper_raw = as.numeric(str_extract(wage, "(?<=-)\\d+")),
    bonus = as.numeric(str_extract(wage, "(?<=·)\\d+")),
    # Normalize if numbers > 1000
    lower = dplyr::if_else(lower_raw > 1000, lower_raw / 1000, lower_raw),
    upper = dplyr::if_else(upper_raw > 1000, upper_raw / 1000, upper_raw),
    id = row_number(),  # Add ID column
  ) |> 
  dplyr::select(-lower_raw, -upper_raw, -company) |> 
  dplyr::select(id, size, wage, experience, education, province, unit, bonus, lower, upper	) |> 
  dplyr::mutate(across(where(is.character), factor)) 
  
# save the results
readr::write_excel_csv(engineering_majors, "engineering_majors_refined.csv")
# end
