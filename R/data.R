library(tidyverse)
library(readODS)

# data URL
cs_stats_url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/940305/Statistical_tables_-_Civil_Service_Statistics_2020_V2.ods"

# download file
# download.file(cs_stats_url, "data/cs_stats_2020.ods")

# get list of sheets
sheets <- readODS::list_ods_sheets("data/cs_stats_2020.ods")

# get list of tables
tables <- readODS::read_ods("data/cs_stats_2020.ods") %>%
  janitor::row_to_names(9) %>%
  janitor::clean_names() %>%
  drop_na() %>%
  filter(table != "Table") %>%
  mutate(ods_label = paste("Table", table, sep = "_"))

# find tables with salary info
tables %>% filter(str_detect(title, "salary"))

# get raw data
salary_dept_raw <- readODS::read_ods("data/cs_stats_2020.ods", sheet = "Table_25")
salary_region_raw <- readODS::read_ods("data/cs_stats_2020.ods", sheet = "Table_26")

# process departmental data
salary_dept <- salary_dept_raw[8:196,2:11] %>%
  janitor::remove_empty() %>%
  set_names(c("dept", "scs", "g6g7", "seoheo", "eo", "aoaa", "total")) %>%
  mutate(empty = rowSums(is.na(.))) %>%
  filter(empty == 0) %>%
  mutate(across(-dept, as.numeric)) %>%
  select(-empty) %>%
  pivot_longer(-dept, names_to = "grade", values_to = "annual_salary") %>%
  mutate(hourly_rate = annual_salary / (((52 * 5) - 30 - 1 - 8) * 7.4),
         hourly_rate = janitor::round_half_up(hourly_rate, 2),
         grade_label = case_when(
           grade == "scs" ~ "SCS",
           grade == "g6g7" ~ "Grade 6/Grade 7",
           grade == "seoheo" ~ "SEO/HEO",
           grade == "eo"~ "EO",
           grade == "aoaa" ~ "AO/AA",
           grade == "total" ~ "Unknown"
         ))

# get region data
salary_region <- salary_region_raw[8:26, 2:11] %>%
  janitor::remove_empty() %>%
  set_names(c("region", "scs", "g6g7", "seoheo", "eo", "aoaa", "total")) %>%
  filter(region != "Not reported") %>%
  mutate(across(-region, as.numeric)) %>%
  pivot_longer(cols = -region, names_to = "grade")

# get overall totals from region data
salary_region_totals <- salary_region %>%
  filter(region == "All employees") %>%
  rename(overall = value) %>%
  select(-region)

# convert region data to ratio
salary_region_ratio <- salary_region %>%
  left_join(salary_region_totals, by = "grade") %>%
  mutate(ratio = value/overall) %>%
  select(region, grade, ratio)

# write data
write_csv(salary_dept, "data/salary_dept.csv")
write_csv(salary_region, "data/salary_region.csv")
write_csv(salary_region_ratio, "data/salary_region_ratio.csv")

