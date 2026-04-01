# raw data wrangle of moult photos scores, ect.
library(tidyverse)
library(here)
library(readxl)
library(standardize)

# import data with all columns as character (so that no auto-formatting is done by R)
dat <-
  bind_rows(read.csv(here("data/Dataset_Molt_Final - modified 7.11.2024-1.csv"), colClasses = "character"),
            read.csv(here("data/Aug_Sep_scored_final-1.csv"), colClasses = "character")) %>% 
  mutate(Rings_comb_simp = ifelse(str_detect(Rings_comb, "_F"), str_sub(Rings_comb, 1, 4), Rings_comb)) %>%
  rename_with(tolower) # convert all column headers to lowercase

# import sex, migratory status, and age at banding information
dat_sexes <-
  read.csv(here("data/sex_status_banding.csv"), colClasses = "character") %>% 
  mutate(Rings_comb_simp = ifelse(str_detect(Rings_comb, "_F"), str_sub(Rings_comb, 1, 4), Rings_comb)) %>%
  rename_with(tolower) %>% # convert all column headers to lowercase
  distinct() %>% 
  select(rings_comb_simp, sex, migratory_status, banding_date, age_at_banding)

dat_sexes %>% 
  # filter(!(rings_comb_simp %in% dat$rings_comb_simp))
  filter(rings_comb_simp == "RROW")

dat_sexes %>% 
  filter(!(rings_comb_simp %in% dat$rings_comb_simp))

# check values of Date column for mistakes...looks good
unique(dat$date) %>% as.Date(., format = "%d/%m/%Y") %>% sort()

# check values of score column for mistakes...one row to fix
unique(dat$score)

# one row contains no data for the score (row 767)...needs to be fixed
dat %>% 
  filter(score == "")

dat %>% 
  filter(score == "0")

dat %>% 
  filter(molt == "0" & !is.na(score))

dat %>% 
  filter(molt == "1" & score == "0")

dat %>% 
  filter(rings_comb_simp == "RGBG")

dat <- 
  dat %>% 
  mutate(score = ifelse(sourcefile == "E:/Kaikoura_photos/Ailsa/2021to2022/February_2022/2022_02_11 JA Dots and Spoonbills/IMG_3590.JPG", 1, score)) %>% 
  mutate(score = ifelse(sourcefile == "E:/Kaikoura_photos/Ailsa/2021to2022/April_2022/2022_04_08 Dob Ob Jimmies/IMG_9681.JPG", 2, score)) %>% 
  mutate(rings_comb = ifelse(sourcefile == "E:/Kaikoura_photos/Ailsa/2021to2022/November_2021/2021_11_25 Dot ob new nests S A and A begin/IMG_2879.JPG", "RBBO", rings_comb)) %>% 
  mutate(rings_comb_simp = ifelse(sourcefile == "E:/Kaikoura_photos/Ailsa/2021to2022/November_2021/2021_11_25 Dot ob new nests S A and A begin/IMG_2879.JPG", "RBBO", rings_comb_simp)) %>% 
  mutate(rings_comb = ifelse(sourcefile == "E:/Dotterel Observations/2022/October/2022_10_16 RRBB/IMG_4487.JPG", "RWBG", rings_comb)) %>% 
  mutate(rings_comb_simp = ifelse(sourcefile == "E:/Dotterel Observations/2022/October/2022_10_16 RRBB/IMG_4487.JPG", "RWBG", rings_comb_simp)) %>% 
  mutate(rings_comb = ifelse(sourcefile == "E:/Dotterel Observations/2023/September/2023_09_22 Dot Ob Nests/IMG_6783.JPG", "RGBR", rings_comb)) %>% 
  mutate(rings_comb_simp = ifelse(sourcefile == "E:/Dotterel Observations/2023/September/2023_09_22 Dot Ob Nests/IMG_6783.JPG", "RGBR", rings_comb_simp)) %>% 
  filter(sourcefile != "E:/Dotterel Observations/2024/February/2024_02_12 Dot ob SB/IMG_6744.JPG") %>% 
  filter(sourcefile != "E:/Dotterel Observations/2024/February/2024_02_12 Dot ob SB/IMG_6584.JPG") %>% 
  filter(sourcefile != "E:/Dotterel Observations/2023/January/23_07_01 Dot ob JA/IMG_9467.JPG") %>% 
  filter(sourcefile != "E:/Kaikoura_photos/Ailsa/2021to2022/March_2022/2022_03_07 Dot Ob Point to JA AM/IMG_5920.JPG") %>% 
  filter(sourcefile != "E:/Dotterel Observations/2023/April/2023_04_14 Dot ob with Barb/IMG_0404.JPG")

# check values of rings_comb column for mistakes...need to remove the non-unique combos (rows with XB and XW)
unique(dat$rings_comb) %>% sort()

setdiff(dat$rings_comb_simp, dat_sexes$rings_comb_simp)
setdiff(dat_sexes$rings_comb_simp, dat$rings_comb_simp)

dat %>% 
  filter(rings_comb %in% c("XB", "XW"))

dat %>% 
  filter(rings_comb %in% c("RBBO")) %>% 
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>% 
  arrange(date)

# remove observations of non-unique combos from the data
dat <-
  dat %>% 
  filter(!grepl("XB", rings_comb) & !grepl("XW", rings_comb))

# checkout the sex-type data
# number of birds with sex-type data
dat_sexes %>% pull(rings_comb_simp) %>% unique() %>% length()

# identify duplicates...none, good
dat_sexes[which(duplicated(dat_sexes)), ]

# assess all birds with migratory status data
dat_sexes %>% filter(migratory_status %in% c("R", "M"))

# mutate the Date column into a date variable
dat <-
  dat %>% 
  mutate(date = paste(substring(dat$date, first = 7, last = 10), 
                      substring(date, first = 4, last = 5),
                      substring(date, first = 1, last = 2),
                      sep = "-") %>% as.Date()) %>% 
  # remove the scores of 0 and NA
  filter(score != 0, !is.na(score)) %>% 
  # specify the season as the first calender year
  mutate(season = ifelse(month(date) < 7, year(date) - 1, year(date))) %>% 
  # change to Julian date shifted for the Southern Hemisphere (1 = July 1)
  mutate(date_J = as.numeric(format(date + 181, "%j"))) %>% 
  # join the sexes provided by Ailsa (note many-to-many is fine)
  left_join(., dat_sexes, by = "rings_comb_simp", relationship = "many-to-many") %>%
  # add a ranking variable to sort the facets of the sampling distribution plots
  group_by(rings_comb) %>% mutate(n_photos = n(), 
                                  n_scores = n_distinct(score)) %>% 
  arrange(desc(n_scores)) %>%
  mutate(
    banding_date = paste0("20", substring(banding_date, first = 7, last = 8), "-",
                          substring(banding_date, first = 4, last = 5), "-",
                          substring(banding_date, first = 1, last = 2)) %>% as.Date(., format = "%Y-%m-%d"),
    season_ringed = ifelse(age_at_banding  == "P" , 
                           ifelse(month(banding_date) < 7, year(banding_date) - 1, year(banding_date)),
                           NA),
    age = ifelse(age_at_banding == "P", as.numeric(season - season_ringed), NA),
    hatch_md = as.numeric(format(banding_date + 181, "%j"))) %>% 
  ungroup()

# check that each combo has one sex-type
more_than_one_sex <- 
  dat %>% 
  group_by(rings_comb) %>% 
  summarise(n_sexes = n_distinct(sex)) %>% 
  filter(n_sexes != 1)

# birds seen in winter months in Kaikoura
read.csv(here("Metadata_pictures_Mar_Jul.csv"), colClasses = "character") %>%
  select(ModifyDate, Rings_comb) %>%
  filter(Rings_comb != "") %>%
  mutate(date = str_sub(ModifyDate, 1, 10) %>% as.Date(., format = "%Y:%m:%d")) %>%
  mutate(month = month(date)) %>%
  filter(month %in% c(4, 5, 6)) %>%
  select(Rings_comb, month) %>% distinct() %>% arrange(Rings_comb) %>% 
  filter(Rings_comb %in% (dat %>% filter(migratory_status == "") %>% pull(rings_comb_simp) %>% unique() %>% sort()))

dat_chk <- dat_breeding %>% 
  # select(rings_comb_simp, season, score, date_J) %>% 
  mutate(
    score = as.numeric(score),
    season = as.factor(season)
  ) %>% 
  arrange(rings_comb, season, date_J)

dat_chk <- dat_chk %>% 
  group_by(rings_comb, season) %>% 
  mutate(
    score_prev = lag(score),
    score_next = lead(score),
    delta_prev = score - score_prev,
    is_increase = !is.na(delta_prev) & delta_prev > 0
  ) %>% 
  ungroup()

# How many increasing steps?
dat_chk %>% 
  summarise(
    n_total = n(),
    n_increases = sum(is_increase, na.rm = TRUE),
    prop_increases = mean(is_increase, na.rm = TRUE)
  )

dat_chk %>% 
  group_by(rings_comb_simp, season) %>% 
  summarise(
    n_obs = n(),
    n_increases = sum(is_increase, na.rm = TRUE),
    any_increase = n_increases > 0,
    .groups = "drop"
  ) %>% 
  summarise(
    n_ind_season = n(),
    n_with_increase = sum(any_increase),
    prop_with_increase = mean(any_increase)
  )

dat_chk <- dat_chk %>% 
  group_by(rings_comb, season) %>% 
  mutate(
    is_local_peak = 
      !is.na(score_prev) &
      !is.na(score_next) &
      score > score_prev &
      score > score_next
  ) %>% 
  ungroup()

dat_chk %>% 
  summarise(
    n_local_peaks = sum(is_local_peak, na.rm = TRUE),
    prop_local_peaks = mean(is_local_peak, na.rm = TRUE)
  )

dat_clean <- dat_chk %>% 
  mutate(
    score_clean = if_else(
      is_local_peak,
      pmax(score_prev, score_next, na.rm = TRUE),
      score
    )
  )

dat_breeding <- dat_clean

error_cases <- dat_chk %>% 
  filter(is_local_peak) %>% 
  distinct(rings_comb, season)

set.seed(123)
example_ids <- error_cases %>% 
  slice_sample(n = 3)

plot_dat <- dat_clean %>% 
  semi_join(error_cases, by = c("rings_comb", "season"))

ggplot(plot_dat, aes(x = date_J)) +
  geom_line(aes(y = score), colour = "grey60", linewidth = 0.8) +
  geom_point(aes(y = score), colour = "grey60", size = 2) +
  geom_line(aes(y = score_clean), colour = "black", linewidth = 1) +
  geom_point(aes(y = score_clean), colour = "black", size = 2) +
  geom_point(
    data = plot_dat %>% filter(is_local_peak),
    aes(y = score),
    colour = "red",
    size = 3
  ) +
  facet_wrap(~ rings_comb + season) +
  labs(
    x = "Julian date",
    y = "Molt score",
    title = "Raw vs. corrected molt score trajectories",
    subtitle = "Red points indicate corrected observer errors"
  ) +
  theme_bw()

### breeding data wrangle ----
# explore datasets provided by Ted
## 2021 season
# import and consolidate key columns
breeding_data_2021 <- 
  #read_excel("/Users/leberhart/ownCloud/kemp_projects/bdot/moult/data/Kaikoura_Dotterel_2021_Feb2022_upd_Nov22_Locations.xlsx", 
  read_excel("data/Kaikoura_Dotterel_2021_Feb2022_upd_Nov22_Locations.xlsx",
             sheet = "Kaikoura_NestVisit2021_1", 
             col_types = "text") %>%
  mutate(visit_date_ = as.POSIXct(as.numeric(`Visit Date`) * 86400, origin = "1899-12-30", tz = "UTC")) %>% 
  mutate(visit_date_nz = with_tz(visit_date_, tzone = "Pacific/Auckland")) %>% 
  mutate(date = as.Date(visit_date_nz)) %>% 
  select(OBJECTID, NestID, date, `Nest Status`, `Egg count`, `Number Hatched`, Notes, `Number Fledged`, `Number of chicks seen`) %>% 
  rename(nest_fate = `Nest Status`,
         eggs = `Egg count`,
         hatched = `Number Hatched`,
         fledged = `Number Fledged`,
         chicks = `Number of chicks seen`)

# Define a function to extract the desired patterns and ensure enough columns
extract_patterns <- function(text) {
  # Replace vertical bar '|' with an empty string
  text <- gsub("\\|", "", text)
  
  # Extract all 4 or 5-character long texts that start with R or r
  matches_r <- str_extract_all(text, "\\b[Rr]\\w{3,4}\\b")[[1]]
  
  # Extract UB, UN, UBF, UBM as standalone patterns
  matches_ub_un <- str_extract_all(text, "\\b[Uu][BbNnFfMm]\\b")[[1]]
  
  # Combine matches
  matches <- c(matches_r, matches_ub_un)
  
  # Ensure all matches are capitalized
  matches <- toupper(matches)
  
  # Ensure there are exactly two columns, filling with NA if necessary
  result <- c(matches, rep(NA, 2 - length(matches)))
  
  # Return the result as a named vector
  return(setNames(result, c("parent1", "parent2")))
}

# Apply the function to the text column and store the results in new columns
extracted_parents <- t(sapply(breeding_data_2021$NestID, extract_patterns))
extracted_parents %>% as.data.frame() %>% 
  rownames_to_column(var = "rowname") %>% 
  mutate(id = str_extract(rowname, "^[^.]+")) %>% 
  filter(parent1 %in% c("RROB", "RROW", "RBWO") | parent2 %in% c("RROB", "RROW", "RBWO")) %>% 
  select(-rowname) %>% 
  distinct()

breeding_data_2021 <-
  bind_cols(breeding_data_2021, extracted_parents) %>% 
  select(-parent2) %>% 
  rename(parent = parent1) %>% 
  bind_rows(bind_cols(breeding_data_2021, extracted_parents) %>% 
              select(-parent1) %>% 
              rename(parent = parent2)) %>% 
  filter(!is.na(parent)) %>% 
  # filter(parent %in% (dat %>% filter(season == 2021) %>% pull(rings_comb_simp) %>% unique())) %>%
  filter(nest_fate == "Occupied" | as.numeric(chicks) > 0 | as.numeric(chicks) == -1) %>%
  mutate(nest_id = sub("^([^[:space:]]+).*", "\\1", NestID)) %>%
  group_by(parent) %>% 
  mutate(
    first_date_breeding = date[which.min(date)],
    last_date_breeding = date[which.max(date)],
    first_nest_id = nest_id[which.min(date)],
    last_nest_id = nest_id[which.max(date)]) %>%
  # arrange(desc(date)) %>%
  # slice(1) %>%
  ungroup() %>%
  # arrange(desc(date)) %>%
  group_by(nest_id) %>% 
  mutate(
    first_date_nest = date[which.min(date)],
    last_date_nest = date[which.max(date)]) %>%
  filter(first_date_nest == date | last_date_nest == date) %>% 
  # rename(last_date_breeding = date) %>% 
  select(parent, first_date_breeding, last_date_breeding, first_nest_id, last_nest_id, first_date_nest, last_date_nest, nest_id, OBJECTID) %>%
  group_by(parent) %>% 
  mutate(last_nest_first_obs = first_date_nest[which.max(first_date_nest)]) %>% 
  ungroup()

# import and consolidate key columns
breeding_data_2021_ <-
  #  read_excel("/Users/leberhart/ownCloud/kemp_projects/bdot/moult/data/Kaikoura_Dotterel_2021_Feb2022_upd_Nov22_Locations.xlsx", 
  read_excel("data/Kaikoura_Dotterel_2021_Feb2022_upd_Nov22_Locations.xlsx",
             sheet = "Kaikoura_DotterelNest2021_0", 
             col_types = "text") %>%
  mutate(hatch_date_ = as.POSIXct(as.numeric(`Date Hatched`) * 86400, origin = "1899-12-30", tz = "UTC"),
         fail_date_ = as.POSIXct(as.numeric(`Date Failed`) * 86400, origin = "1899-12-30", tz = "UTC"),
         found_date_ = as.POSIXct(as.numeric(`Date Found`) * 86400, origin = "1899-12-30", tz = "UTC")) %>% 
  mutate(hatch_date_nz = with_tz(hatch_date_, tzone = "Pacific/Auckland"),
         fail_date_nz = with_tz(fail_date_, tzone = "Pacific/Auckland"),
         found_date_nz = with_tz(found_date_, tzone = "Pacific/Auckland")) %>% 
  mutate(hatch_date = as.Date(hatch_date_nz),
         fail_date = as.Date(fail_date_nz),
         found_date = as.Date(found_date_nz)) %>%
  mutate(nest_id = sub("^([^[:space:]]+).*", "\\1", `Nest ID`)) %>% 
  select(OBJECTID, nest_id, found_date, hatch_date, fail_date) %>% 
  mutate(time_bw_found_hatch = hatch_date - found_date)  %>% 
  mutate(lay_date = as.Date(ifelse(time_bw_found_hatch < 25, hatch_date - 28, found_date))) %>% 
  arrange(time_bw_found_hatch)

breeding_2021_final <- 
  left_join(breeding_data_2021, breeding_data_2021_, by = "nest_id") %>%
  mutate(date_check = ifelse((!is.na(found_date) & is.na(fail_date) & is.na(hatch_date)) | 
                               ((!is.na(hatch_date) & found_date < hatch_date)) | 
                               ((!is.na(fail_date) & !is.na(hatch_date)) & hatch_date < fail_date) |
                               ((!is.na(fail_date) & found_date < fail_date)), 1, 0)) %>% 
  group_by(parent) %>% 
  mutate(last_date_breeding_ = max(found_date, hatch_date, fail_date, na.rm = TRUE)) %>% 
  mutate(first_date_breeding_ = min(found_date, hatch_date, fail_date, lay_date, na.rm = TRUE)) %>% 
  mutate(days_diff = last_date_breeding_ - last_date_breeding) %>% 
  mutate(days_diff = first_date_breeding_ - first_date_breeding) %>% 
  arrange(desc(days_diff)) %>% 
  mutate(last_date_breeding_final = as.Date(ifelse(days_diff > 0, last_date_breeding + ceiling(days_diff/2), last_date_breeding)),
         first_date_breeding_final = as.Date(ifelse(days_diff > 0, first_date_breeding + ceiling(days_diff/2), first_date_breeding))) %>% 
  # arrange(desc(last_date_breeding_final)) %>% 
  # specify the season as the first calender year
  mutate(season = ifelse(month(last_date_breeding_final) < 7, year(last_date_breeding_final) - 1, year(last_date_breeding_final))) %>% 
  distinct() %>% 
  mutate(first_date_nest = ifelse(!is.na(lay_date) & lay_date < first_date_nest, lay_date, first_date_nest)) %>% 
  group_by(parent) %>% 
  mutate(last_nest_first_obs = as.Date(first_date_nest[which.max(first_date_nest)])) %>% 
  select(parent, first_date_breeding_final, last_date_breeding_final, last_nest_first_obs) %>% 
  mutate(parent = ifelse(parent == "RROW", "RROW_female", parent)) %>% 
  mutate(parent = ifelse(parent == "RROB", "RROB_female", parent))

###########

## 2022 season

# import and consolidate key columns
breeding_data_2022 <- 
  #read_excel("/Users/leberhart/ownCloud/kemp_projects/bdot/moult/data/Kaikoura_Dotterel_Jan23_upd.xlsx", 
  read_excel("data/Kaikoura_Dotterel_Jan23_upd.xlsx",
             sheet = "Kaikoura_NestVisit_1", 
             col_types = "text") %>%
  mutate(visit_date_ = as.POSIXct(as.numeric(`Visit Date`) * 86400, origin = "1899-12-30", tz = "UTC")) %>% 
  mutate(visit_date_nz = with_tz(visit_date_, tzone = "Pacific/Auckland")) %>% 
  mutate(date = as.Date(visit_date_nz)) %>% 
  select(OBJECTID, NestID, date, `Nest Status`, `Egg count`, `Number Hatched`, Notes, `Number Fledged`, `Number of chicks seen`) %>% 
  rename(nest_fate = `Nest Status`,
         eggs = `Egg count`,
         hatched = `Number Hatched`,
         fledged = `Number Fledged`,
         chicks = `Number of chicks seen`)


# Apply the function to the text column and store the results in new columns
extracted_parents2 <- t(sapply(breeding_data_2022$NestID, extract_patterns))
extracted_parents2 %>% as.data.frame() %>% 
  rownames_to_column(var = "rowname") %>% 
  mutate(id = str_extract(rowname, "^[^.]+")) %>% 
  filter(parent1 %in% c("RROB", "RROW", "RBWO") | parent2 %in% c("RROB", "RROW", "RBWO")) %>% 
  select(-rowname) %>% 
  distinct()

breeding_data_2022 <-
  bind_cols(breeding_data_2022, extracted_parents2) %>% 
  select(-parent2) %>% 
  rename(parent = parent1) %>% 
  bind_rows(bind_cols(breeding_data_2022, extracted_parents2) %>% 
              select(-parent1) %>% 
              rename(parent = parent2)) %>% 
  filter(!is.na(parent)) %>% 
  # filter(parent %in% (dat %>% filter(season == 2022) %>% pull(rings_comb) %>% unique())) %>%
  filter(nest_fate == "Occupied" | as.numeric(chicks) > 0 | as.numeric(chicks) == -1) %>%
  mutate(nest_id = sub("^([^[:space:]]+).*", "\\1", NestID)) %>%
  group_by(parent) %>% 
  mutate(
    first_date_breeding = date[which.min(date)],
    last_date_breeding = date[which.max(date)],
    first_nest_id = nest_id[which.min(date)],
    last_nest_id = nest_id[which.max(date)]) %>%
  # arrange(desc(date)) %>%
  # slice(1) %>%
  ungroup() %>%
  # arrange(desc(date)) %>%
  group_by(nest_id) %>% 
  mutate(
    first_date_nest = date[which.min(date)],
    last_date_nest = date[which.max(date)]) %>%
  filter(first_date_nest == date | last_date_nest == date) %>% 
  # rename(last_date_breeding = date) %>% 
  select(parent, first_date_breeding, last_date_breeding, first_nest_id, last_nest_id, first_date_nest, last_date_nest, nest_id, OBJECTID) %>%
  group_by(parent) %>% 
  mutate(last_nest_first_obs = first_date_nest[which.max(first_date_nest)]) %>% 
  ungroup()

# import and consolidate key columns
breeding_data_2022_ <-
  #read_excel("/Users/leberhart/ownCloud/kemp_projects/bdot/moult/data/Kaikoura_Dotterel_Jan23_upd.xlsx", 
  read_excel("data/Kaikoura_Dotterel_Jan23_upd.xlsx",          
             sheet = "Kaikoura_DotterelNest_0", 
             col_types = "text") %>%
  mutate(hatch_date_ = as.POSIXct(as.numeric(`Date Hatched`) * 86400, origin = "1899-12-30", tz = "UTC"),
         fail_date_ = as.POSIXct(as.numeric(`Date Failed`) * 86400, origin = "1899-12-30", tz = "UTC"),
         found_date_ = as.POSIXct(as.numeric(`Date Found`) * 86400, origin = "1899-12-30", tz = "UTC")) %>% 
  mutate(hatch_date_nz = with_tz(hatch_date_, tzone = "Pacific/Auckland"),
         fail_date_nz = with_tz(fail_date_, tzone = "Pacific/Auckland"),
         found_date_nz = with_tz(found_date_, tzone = "Pacific/Auckland")) %>% 
  mutate(hatch_date = as.Date(hatch_date_nz),
         fail_date = as.Date(fail_date_nz),
         found_date = as.Date(found_date_nz)) %>%
  mutate(nest_id = sub("^([^[:space:]]+).*", "\\1", `Nest ID`)) %>% 
  select(OBJECTID, nest_id, found_date, hatch_date, fail_date) %>% 
  mutate(time_bw_found_hatch = hatch_date - found_date)  %>% 
  mutate(lay_date = as.Date(ifelse(time_bw_found_hatch < 25, hatch_date - 28, found_date))) %>% 
  arrange(time_bw_found_hatch)

breeding_data_2022_ %>% 
  filter(nest_id == "N53")

dat %>% 
  filter(str_detect(rings_comb_simp, "RROW")) %>% 
  filter(season == 2022)

breeding_2022_final <- left_join(breeding_data_2022, breeding_data_2022_, by = "nest_id") %>%
  mutate(date_check = ifelse((!is.na(found_date) & is.na(fail_date) & is.na(hatch_date)) | 
                               ((!is.na(hatch_date) & found_date < hatch_date)) | 
                               ((!is.na(fail_date) & !is.na(hatch_date)) & hatch_date < fail_date) |
                               ((!is.na(fail_date) & found_date < fail_date)), 1, 0)) %>% 
  group_by(parent) %>% 
  mutate(last_date_breeding_ = max(found_date, hatch_date, fail_date, na.rm = TRUE)) %>% 
  mutate(first_date_breeding_ = min(found_date, hatch_date, fail_date, lay_date, na.rm = TRUE)) %>% 
  mutate(days_diff = last_date_breeding_ - last_date_breeding) %>% 
  mutate(days_diff = first_date_breeding_ - first_date_breeding) %>% 
  arrange(desc(days_diff)) %>% 
  mutate(last_date_breeding_final = as.Date(ifelse(days_diff > 0, last_date_breeding + ceiling(days_diff/2), last_date_breeding)),
         first_date_breeding_final = as.Date(ifelse(days_diff > 0, first_date_breeding + ceiling(days_diff/2), first_date_breeding))) %>% 
  # arrange(desc(last_date_breeding_final)) %>% 
  # specify the season as the first calender year
  mutate(season = ifelse(month(last_date_breeding_final) < 7, year(last_date_breeding_final) - 1, year(last_date_breeding_final))) %>% 
  distinct() %>% 
  mutate(first_date_nest = ifelse(!is.na(lay_date) & lay_date < first_date_nest, lay_date, first_date_nest)) %>% 
  group_by(parent) %>% 
  mutate(last_nest_first_obs = as.Date(first_date_nest[which.max(first_date_nest)])) %>% 
  select(parent, first_date_breeding_final, last_date_breeding_final, last_nest_first_obs) %>% 
  mutate(parent = ifelse(parent == "RROB", "RROB_male", parent)) %>% 
  mutate(parent = ifelse(parent == "RROW", "RROW_male", parent))

###########

## 2023 season

# import and consolidate key columns
breeding_data_2023 <- 
  read_excel("data/Kaikoura_Dotterel_Feb2024_upd.xlsx",          
             sheet = "Kaikoura_NestVisit_1", 
             col_types = "text") %>%
  mutate(visit_date_ = as.POSIXct(as.numeric(`Visit Date`) * 86400, origin = "1899-12-30", tz = "UTC")) %>% 
  mutate(visit_date_nz = with_tz(visit_date_, tzone = "Pacific/Auckland")) %>% 
  mutate(date = as.Date(visit_date_nz)) %>% 
  select(OBJECTID, NestID, date, `Nest Status`, `Egg count`, `Number Hatched`, Notes, `Number Fledged`, `Number of chicks seen`) %>% 
  rename(nest_fate = `Nest Status`,
         eggs = `Egg count`,
         hatched = `Number Hatched`,
         fledged = `Number Fledged`,
         chicks = `Number of chicks seen`)


# Apply the function to the text column and store the results in new columns
extracted_parents3 <- t(sapply(breeding_data_2023$NestID, extract_patterns))
extracted_parents3 %>% as.data.frame() %>% 
  rownames_to_column(var = "rowname") %>% 
  mutate(id = str_extract(rowname, "^[^.]+")) %>% 
  filter(parent1 %in% c("RROB", "RROW", "RBWO") | parent2 %in% c("RROB", "RROW", "RBWO")) %>% 
  select(-rowname) %>% 
  distinct()

breeding_data_2023 <-
  bind_cols(breeding_data_2023, extracted_parents3) %>% 
  select(-parent2) %>% 
  rename(parent = parent1) %>% 
  bind_rows(bind_cols(breeding_data_2023, extracted_parents3) %>% 
              select(-parent1) %>% 
              rename(parent = parent2)) %>% 
  filter(!is.na(parent)) %>% 
  # filter(parent %in% (dat %>% filter(season == 2023) %>% pull(rings_comb) %>% unique())) %>%
  filter(nest_fate == "Occupied" | as.numeric(chicks) > 0 | as.numeric(chicks) == -1) %>%
  mutate(nest_id = sub("^([^[:space:]]+).*", "\\1", NestID)) %>%
  group_by(parent) %>% 
  mutate(
    first_date_breeding = date[which.min(date)],
    last_date_breeding = date[which.max(date)],
    first_nest_id = nest_id[which.min(date)],
    last_nest_id = nest_id[which.max(date)]) %>%
  # arrange(desc(date)) %>%
  # slice(1) %>%
  ungroup() %>%
  # arrange(desc(date)) %>%
  group_by(nest_id) %>% 
  mutate(
    first_date_nest = date[which.min(date)],
    last_date_nest = date[which.max(date)]) %>%
  filter(first_date_nest == date | last_date_nest == date) %>% 
  # rename(last_date_breeding = date) %>% 
  select(parent, first_date_breeding, last_date_breeding, first_nest_id, last_nest_id, first_date_nest, last_date_nest, nest_id, OBJECTID) %>%
  group_by(parent) %>% 
  mutate(last_nest_first_obs = first_date_nest[which.max(first_date_nest)]) %>% 
  ungroup()

# import and consolidate key columns
breeding_data_2023_ <-
  #read_excel("/Users/leberhart/ownCloud/kemp_projects/bdot/moult/data/Kaikoura_Dotterel_Feb2024_upd.xlsx",
  read_excel("data/Kaikoura_Dotterel_Feb2024_upd.xlsx",         
             sheet = "Kaikoura_DotterelNest_0", 
             col_types = "text") %>%
  mutate(hatch_date_ = as.POSIXct(as.numeric(`Date Hatched`) * 86400, origin = "1899-12-30", tz = "UTC"),
         fail_date_ = as.POSIXct(as.numeric(`Date Failed`) * 86400, origin = "1899-12-30", tz = "UTC"),
         found_date_ = as.POSIXct(as.numeric(`Date Found`) * 86400, origin = "1899-12-30", tz = "UTC")) %>% 
  mutate(hatch_date_nz = with_tz(hatch_date_, tzone = "Pacific/Auckland"),
         fail_date_nz = with_tz(fail_date_, tzone = "Pacific/Auckland"),
         found_date_nz = with_tz(found_date_, tzone = "Pacific/Auckland")) %>% 
  mutate(hatch_date = as.Date(hatch_date_nz),
         fail_date = as.Date(fail_date_nz),
         found_date = as.Date(found_date_nz)) %>%
  mutate(nest_id = sub("^([^[:space:]]+).*", "\\1", `Nest ID`)) %>% 
  select(OBJECTID, nest_id, found_date, hatch_date, fail_date) %>% 
  mutate(time_bw_found_hatch = hatch_date - found_date)  %>% 
  mutate(lay_date = as.Date(ifelse(time_bw_found_hatch < 25, hatch_date - 28, found_date))) %>% 
  arrange(time_bw_found_hatch)

breeding_2023_final <- left_join(breeding_data_2023, breeding_data_2023_, by = "nest_id") %>%
  mutate(date_check = ifelse((!is.na(found_date) & is.na(fail_date) & is.na(hatch_date)) | 
                               ((!is.na(hatch_date) & found_date < hatch_date)) | 
                               ((!is.na(fail_date) & !is.na(hatch_date)) & hatch_date < fail_date) |
                               ((!is.na(fail_date) & found_date < fail_date)), 1, 0)) %>% 
  group_by(parent) %>% 
  mutate(last_date_breeding_ = max(found_date, hatch_date, fail_date, na.rm = TRUE)) %>% 
  mutate(first_date_breeding_ = min(found_date, hatch_date, fail_date, lay_date, na.rm = TRUE)) %>% 
  mutate(days_diff = last_date_breeding_ - last_date_breeding) %>% 
  mutate(days_diff = first_date_breeding_ - first_date_breeding) %>% 
  arrange(desc(days_diff)) %>% 
  mutate(last_date_breeding_final = as.Date(ifelse(days_diff > 0, last_date_breeding + ceiling(days_diff/2), last_date_breeding)),
         first_date_breeding_final = as.Date(ifelse(days_diff > 0, first_date_breeding + ceiling(days_diff/2), first_date_breeding))) %>% 
  # arrange(desc(last_date_breeding_final)) %>% 
  # specify the season as the first calender year
  mutate(season = ifelse(month(last_date_breeding_final) < 7, year(last_date_breeding_final) - 1, year(last_date_breeding_final))) %>% 
  distinct() %>% 
  mutate(first_date_nest = ifelse(!is.na(lay_date) & lay_date < first_date_nest, lay_date, first_date_nest)) %>% 
  group_by(parent) %>% 
  mutate(last_nest_first_obs = as.Date(first_date_nest[which.max(first_date_nest)])) %>% 
  select(parent, first_date_breeding_final, last_date_breeding_final, last_nest_first_obs) %>% 
  mutate(parent = ifelse(parent == "RROB", "RROB_male", parent))

# combine breeding data from the 3 seasons into one table

breeding_data_all <- 
  bind_rows(breeding_2021_final, breeding_2022_final, breeding_2023_final) %>% 
  rename(rings_comb = parent) %>% 
  distinct() %>% 
  # change to Julian date shifted for the Southern Hemisphere (1 = July 1)
  mutate(last_breeding_date_J = as.numeric(format(last_date_breeding_final + 181, "%j")),
         first_breeding_date_J = as.numeric(format(first_date_breeding_final + 181, "%j")),
         last_nest_first_obs_J = as.numeric(format(last_nest_first_obs + 181, "%j"))) %>% 
  ungroup() %>% 
  mutate(season = ifelse(month(first_date_breeding_final) < 7, year(first_date_breeding_final) - 1, year(first_date_breeding_final))) %>% 
  # standardize the last breeding date by year
  mutate(last_breeding_date_J_s = scale_by(last_breeding_date_J ~ season, ., scale = 0),
         first_breeding_date_J_s = scale_by(first_breeding_date_J ~ season, ., scale = 0),
         last_nest_first_obs_J_s  = scale_by(last_nest_first_obs_J ~ season, ., scale = 0),
         last_breeding_date_J_s1 = scale_by(last_breeding_date_J ~ season, ., scale = 1),
         first_breeding_date_J_s1 = scale_by(first_breeding_date_J ~ season, ., scale = 1),
         last_nest_first_obs_J_s1  = scale_by(last_nest_first_obs_J ~ season, ., scale = 1)) %>% 
  # make the scaled date variable numeric class
  mutate(last_breeding_date_J_snum = as.numeric(last_breeding_date_J_s),
         first_breeding_date_J_snum = as.numeric(first_breeding_date_J_s),
         last_nest_first_obs_J_snum = as.numeric(last_nest_first_obs_J_s),
         last_breeding_date_J_s1num = as.numeric(last_breeding_date_J_s1),
         first_breeding_date_J_s1num = as.numeric(first_breeding_date_J_s1),
         last_nest_first_obs_J_s1num = as.numeric(last_nest_first_obs_J_s1)) %>% 
  mutate(season = factor(season, levels = c("2021", "2022", "2023"))) %>% 
  filter(rings_comb %in% (dat %>% pull(rings_comb_simp) %>% unique()))

### photo data and breeding data merge ----
dat_breeding <- 
  dat %>% 
  mutate(season = as.factor(season)) %>% 
  left_join(breeding_data_all, #%>% rename(rings_comb_simp = rings_comb), 
            by = c("rings_comb", "season")) %>%
  # remove outlier data
  filter(date_J < 300) %>%
  # select(-rings_comb) %>% 
  # rename(rings_comb = rings_comb_simp) %>%
  # classify score and sex
  mutate(
    score = as.numeric(score),
    sex = as.factor(sex),
    season = factor(season, levels = c("2021", "2022", "2023")),
    rings_comb = as.factor(rings_comb),
    migratory_status = factor(migratory_status, levels = c("M", "R")),
    age_class = factor(ifelse(age <= 1, "FY", "AFY"), levels = c("FY", "AFY")),
    across(
      c(
        first_date_breeding_final,
        last_date_breeding_final,
        last_nest_first_obs,
        last_breeding_date_J_snum,
        first_breeding_date_J_snum,
        last_nest_first_obs_J_snum
      ),
      as.numeric
    ))

saveRDS(dat_breeding,
        file = here("data/moult_breeding_KK_data.rds"))

saveRDS(breeding_data_all,
        file = here("data/breeding_KK_data.rds"))

saveRDS(bind_rows(breeding_data_2021_, breeding_data_2022_, breeding_data_2023_),
        file = here("data/nests_KK_data.rds"))


dat_chk <- dat_breeding %>% 
  # select(rings_comb_simp, season, score, date_J) %>% 
  mutate(
    score = as.numeric(score),
    season = as.factor(season)
  ) %>% 
  arrange(rings_comb_simp, season, date_J)

dat_chk <- dat_chk %>% 
  group_by(rings_comb_simp, season) %>% 
  mutate(
    score_prev = lag(score),
    score_next = lead(score),
    delta_prev = score - score_prev,
    is_increase = !is.na(delta_prev) & delta_prev > 0
  ) %>% 
  ungroup()

# How many increasing steps?
dat_chk %>% 
  summarise(
    n_total = n(),
    n_increases = sum(is_increase, na.rm = TRUE),
    prop_increases = mean(is_increase, na.rm = TRUE)
  )

dat_chk %>% 
  group_by(rings_comb_simp, season) %>% 
  summarise(
    n_obs = n(),
    n_increases = sum(is_increase, na.rm = TRUE),
    any_increase = n_increases > 0,
    .groups = "drop"
  ) %>% 
  summarise(
    n_ind_season = n(),
    n_with_increase = sum(any_increase),
    prop_with_increase = mean(any_increase)
  )

dat_chk <- dat_chk %>% 
  group_by(rings_comb_simp, season) %>% 
  mutate(
    is_local_peak = 
      !is.na(score_prev) &
      !is.na(score_next) &
      score > score_prev &
      score > score_next
  ) %>% 
  ungroup()

dat_chk %>% 
  summarise(
    n_local_peaks = sum(is_local_peak, na.rm = TRUE),
    prop_local_peaks = mean(is_local_peak, na.rm = TRUE)
  )

dat_clean <- dat_chk %>% 
  mutate(
    score_clean = if_else(
      is_local_peak,
      pmax(score_prev, score_next, na.rm = TRUE),
      score
    )
  )

dat_breeding <- dat_clean

error_cases <- dat_chk %>% 
  filter(is_local_peak) %>% 
  distinct(rings_comb_simp, season)

set.seed(123)
example_ids <- error_cases %>% 
  slice_sample(n = 3)

plot_dat <- dat_clean %>% 
  semi_join(error_cases, by = c("rings_comb_simp", "season"))

ggplot(plot_dat, aes(x = date_J)) +
  geom_line(aes(y = score), colour = "grey60", linewidth = 0.8) +
  geom_point(aes(y = score), colour = "grey60", size = 2) +
  geom_line(aes(y = score_clean), colour = "black", linewidth = 1) +
  geom_point(aes(y = score_clean), colour = "black", size = 2) +
  geom_point(
    data = plot_dat %>% filter(is_local_peak),
    aes(y = score),
    colour = "red",
    size = 3
  ) +
  facet_wrap(~ rings_comb_simp + season) +
  labs(
    x = "Julian date",
    y = "Molt score",
    title = "Raw vs. corrected molt score trajectories",
    subtitle = "Red points indicate corrected observer errors"
  ) +
  theme_bw()