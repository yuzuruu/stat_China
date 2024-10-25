########################################################################
# Draw maps of the N. of student in Chinese University
# 26th. October, 2024
# by Yuzuru Utsunomiya, Ph. D.
########################################################################
#
# NOTE
# When use of a certain function is not clear, we have some options.
# Option 1 Ask R
# Example
# If you would like to know how to use ggplot2::ggplot() function,
# ask R with "?".
#
# ?ggplot2::ggplot
#
# Option 2 Ak Google
# 
# -----read.library -----
library(tidyverse)
library(sf)
library(khroma)
library(ggrepel)
#
# ----- read.data -----
university_list_china <- 
  # read the data of the N. of student
  readxl::read_excel("List_of_Univ_Cn.xlsx") %>% 
  # cleaning the data
  # The data inludes unnecessary strings and line breaks.
  # They might be a cause of malfunction and we need to remove the.
  dplyr::mutate(
    # remove Chinese characters
    # the [\u4e00-\u9fff] demonstrates range of the Chinese character
    # at UTF-8 system.
    university = stringr::str_remove_all(university, "[\u4e00-\u9fff]"),
    # remove Japanese characters
    university = stringr::str_remove_all(university, "[\u30a0-\u30ff\u3040-\u309f]"),
    # replace line breaks into 
    university = stringr::str_replace_all(university, "[\r\n]" , "")
  ) %>% 
  # Fill missing values using the tidyr::fill() function to make a tidy dataset
  # The data should have completed by the factors below. 
  # For data analysis, we do not need to follow MSExcel-table style.
  tidyr::fill(university) %>% 
  tidyr::fill(province) %>% 
  tidyr::fill(major) %>% 
  # Unify some provinces' name in accordance with one employed by the shapefiles.
  # To confirm the provinces' name, we can check by reading and opening the 
  # shapefiles.
  # Or else,we need to open .dbf file using other apps such as LibreOffice. 
  dplyr::mutate(
    # convert data type from character to factor
    # The dplyr::across function finds meeting conditions. In this case,
    # the function convert data type if a variable is character.
    dplyr::across(
      where(is.character),
      factor
      ),
    province = stringr::str_replace_all(province, "Shanxi", "Shaanxi") %>% factor(),
    province = stringr::str_replace_all(province, "Mongolia", "Nei Mongol") %>% factor()
  ) 
# read shapefiles of China
Province_China <- 
  sf::read_sf(
    "gadm41_CHN_shp/gadm41_CHN_1.shp"
  ) %>% 
  dplyr::tibble() %>% 
  dplyr::mutate(
    dplyr::across(where(is.character),factor)
  )
#
# ----- draw.map -----
# make a summary table of the data
N_student_china <- 
  # read the data
  university_list_china %>% 
  # make a group by target factor(s)
  # We can make the group by multiple factors such as major and degree
  # dplyr:group_by(province, major, degree)
  dplyr::group_by(province) %>% 
  # make a summary table
  # Not only sample size and mean but also some other descriptive statistics
  # such as median and sd can be placed.
  dplyr::summarise(
    n_student = sum(number),
    Mean = mean(number)
  ) %>% 
  # remove observations with NA
  tidyr::drop_na() %>% 
  ungroup()
# draw a map
map_N_student_china <- 
  # Combine the university data with Shapefiles to draw a 
  # Chropleth map. We use the statistics for filling colors
  # by the statistics.
  Province_China %>% 
  # Pick up the target data
  # For the filter, check usage of logical operators.
  dplyr::filter(
    NAME_1 %in% levels(N_student_china$province)
    ) %>% 
  # combine the data
  dplyr::left_join(
    N_student_china, 
    by = c("NAME_1" = "province")
    ) %>% 
  tidyr::drop_na() %>% 
  # a magic word
  sf::st_as_sf() %>% 
  # add variables of coordinate of centroid of province.
  dplyr::mutate(
    # calculate the centroid first
    centroid = st_centroid(geometry),
    # longitude
    x = st_coordinates(centroid)[,1],
    # latitude
    y = st_coordinates(centroid)[,2],
  ) %>% 
  # draw the map
  # In detail of the ggplot2::ggplot(), check the following page.
  # https://r-graphics.org/
  ggplot2::ggplot(
    aes(
      geometry = geometry,
      fill = n_student
      )
  ) +
  geom_sf() +
  ggrepel::geom_text_repel(
    aes(
      x = x,
      y = y,
      label = NAME_1
    )
  ) +
  khroma::scale_fill_smoothrainbow() +
  theme_void() +
  theme(
    legend.position = "bottom"
  )
# save in pdf format
ggsave(
  "map_N_student_china.pdf",
  plot = map_N_student_china,
  height = 200,
  witdh = 200,
  units = "mm"
)
