library(tidyverse)
library(readxl)
library(glue)


# Scotland ----------------------------------------------------------------

sexual_age_scot <- read_xlsx("data/raw-data/Scotland/sexual_age_scot.xlsx", skip = 11) |>
  select(-1) |>
  rename(Sex = "...2", Age = "...3") |>
  fill(Sex, .direction = "down") |>
  filter(row_number() != 1) |>
  drop_na() |>
  pivot_longer(-c(Sex, Age), names_to = "response", values_to = "n") |>
  rename(sex = Sex, age = Age) |>
  mutate(
    response = str_trim(response),
    response = if_else(response == "All people aged 16 and over", "Total", response)
  ) |>
  mutate(sex = if_else(sex == "All people aged 16 and over", "All", sex))
write_csv(sexual_age_scot, "data/sexual_age_scot.csv")

genmod_age_scot <- read_xlsx("data/raw-data/Scotland/genmod_age_scot.xlsx", skip = 11) |>
  select(-1) |>
  rename(Age = "...2") |>
  filter(row_number() != 1) |>
  drop_na() |>
  pivot_longer(-Age, names_to = "response", values_to = "n") |>
  rename(age = Age) |>
  mutate(
    response = str_trim(response),
    response = if_else(response == "All people aged 16 and over", "Total", response)
  )
write_csv(genmod_age_scot, "data/genmod_age_scot.csv")


# Northern Ireland --------------------------------------------------------

sexual_age_ni_raw1 <- read_xlsx("data/raw-data/Northern Ireland/sexual_age_ni.xlsx",
  sheet = 2, skip = 8
) |>
  filter(row_number() <= 12) |>
  pivot_longer(-c(Geography, `Geography code`),
    values_to = "count"
  ) |>
  separate_wider_delim(
    cols = name,
    delim = ":", names = c("age", "sexual_orientation"),
    too_few = "align_start"
  ) |>
  mutate(
    sexual_orientation = str_trim(sexual_orientation),
    sexual_orientation = if_else(is.na(sexual_orientation), "All", sexual_orientation)
  ) |>
  mutate(age = str_replace(age, "All usual residents", "Usual residents"))

sexual_age_ni_raw2 <- read_xlsx("data/raw-data/Northern Ireland/sexual_age_ni.xlsx",
  sheet = 2, skip = 23
) |>
  drop_na() |>
  pivot_longer(-c(Geography, `Geography code`),
    values_to = "percentage"
  ) |>
  separate_wider_delim(
    cols = name,
    delim = ":", names = c("age", "sexual_orientation"),
    too_few = "align_start"
  ) |>
  mutate(
    sexual_orientation = str_trim(sexual_orientation),
    sexual_orientation = if_else(is.na(sexual_orientation), "All", sexual_orientation)
  ) |>
  mutate(age = str_replace(age, "All usual residents", "Usual residents")) |>
  filter(percentage <= 1)

sexual_age_ni <- sexual_age_ni_raw1 |>
  left_join(sexual_age_ni_raw2, by = c("Geography", "Geography code", "age", "sexual_orientation")) |>
  mutate(percentage = 100 * percentage) |>
  rename(
    area_code = `Geography code`,
    area_name = Geography,
  )
write_csv(sexual_age_ni, "data/sexual_age_ni.csv")


# England & Wales ---------------------------------------------------------

sexual_genmod_ew <- read_csv("data/raw-data/England and Wales/sexual_genmod.csv") |>
  select(
    area_code = `Lower tier local authorities Code`,
    area_name = `Lower tier local authorities`,
    gender_identity = `Gender identity (7 categories)`,
    sexual_orientation = `Sexual orientation (4 categories)`,
    n = Observation
  )
write_csv(sexual_genmod_ew, "data/sexual_genmod_ew.csv")

# Population data
population_ew <- read_xlsx("data/spatial-data/lsoa_population.xlsx", skip = 2) |>
  select(
    area_code = `LA code`,
    area_name = `LA name`,
    population = `Usual resident population, 2021`
  )
write_csv(population_ew, "data/population_ew.csv")


## Additional data --------------------------------------------------------

process_ew_data <- function(fpath, type = "Sexual orientation") {
  variable_name <- fpath |>
    str_remove("sexual_") |>
    str_remove("genmod_") |>
    str_remove(".csv")

  raw_var_name <- variable_name |>
    str_replace_all("_", " ") |>
    str_to_sentence()

  gen_sex_name <- type |>
    str_to_lower() |>
    str_replace_all(" ", "_")

  full_fpath <- glue("data/raw-data/England and Wales/{type}/{fpath}")
  data_ew <- read_csv(full_fpath, show_col_types = FALSE) |>
    rename(
      area_code = `Lower tier local authorities Code`,
      area_name = `Lower tier local authorities`,
      n = Observation
    ) |>
    select(-ends_with("Code", ignore.case = FALSE)) |>
    select(area_code, area_name,
           {{gen_sex_name}} := starts_with(type),
           {{variable_name}} := starts_with(raw_var_name),
           n) |>
    left_join(population_ew, by = c("area_code", "area_name")) |>
    mutate(percentage = 100 * n / population)
  write_csv(data_ew, glue("data/additional-ew/{fpath}"))
}

# Sexual orientation
orientation_files <- list.files("data/raw-data/England and Wales/Sexual orientation")
orientation_drop_files <- c("sexual_age_sex.csv", "sexual_detailed geo.xlsx", "sexual_further_char.xlsx")
orientation_files <- orientation_files[!orientation_files %in% orientation_drop_files]
for (i in 1:length(orientation_files)) {
  process_ew_data(orientation_files[i])
}

# Gender modality
gender_files <- list.files("data/raw-data/England and Wales/Gender modality")
for (i in 1:length(gender_files)) {
  process_ew_data(gender_files[i], type = "Gender modality")
}

# Tricky files
sexual_detailed_geo_ew <- read_xlsx(paste0("data/raw-data/England and Wales/Sexual orientation/", orientation_drop_files[2]), sheet = 2, skip = 1) |>
  select(
    area_code = `England and Wales code`,
    area_name = `England and Wales`,
    sexual_orientation = `Sexual orientation (9 categories)`,
    n = Observation,
    percentage = starts_with("Percentage")
  ) |>
  mutate(area_level = "England and Wales", .after = 2)
sexual_detailed_geo_country <- read_xlsx(paste0("data/raw-data/England and Wales/Sexual orientation/", orientation_drop_files[2]), sheet = 3, skip = 1) |>
  select(
    area_code = `Countries code`,
    area_name = Countries,
    sexual_orientation = `Sexual orientation (9 categories)`,
    n = Observation,
    percentage = starts_with("Percentage")
  ) |>
  mutate(area_level = "Country", .after = 2)
sexual_detailed_geo_region <- read_xlsx(paste0("data/raw-data/England and Wales/Sexual orientation/", orientation_drop_files[2]), sheet = 4, skip = 1) |>
  select(
    area_code = `Regions code`,
    area_name = Regions,
    sexual_orientation = `Sexual orientation (9 categories)`,
    n = Observation,
    percentage = starts_with("Percentage")
  ) |>
  mutate(area_level = "Region", .after = 2)
sexual_detailed_geo_utla <- read_xlsx(paste0("data/raw-data/England and Wales/Sexual orientation/", orientation_drop_files[2]), sheet = 5, skip = 1) |>
  select(
    area_code = `Upper tier local authorities code`,
    area_name = `Upper tier local authorities`,
    sexual_orientation = `Sexual orientation (9 categories)`,
    n = Observation,
    percentage = starts_with("Percentage")
  ) |>
  mutate(area_level = "Upper Tier Local Authority", .after = 2) |>
  mutate(n = as.numeric(n),
         percentage = as.numeric(percentage))
sexual_detailed_geo_ltla <- read_xlsx(paste0("data/raw-data/England and Wales/Sexual orientation/", orientation_drop_files[2]), sheet = 6, skip = 1) |>
  select(
    area_code = `Lower tier local authorities code`,
    area_name = `Lower tier local authorities`,
    sexual_orientation = `Sexual orientation (9 categories)`,
    n = Observation,
    percentage = starts_with("Percentage")
  ) |>
  mutate(area_level = "Lower Tier Local Authority", .after = 2) |>
  mutate(n = as.numeric(n),
         percentage = as.numeric(percentage))
sexual_detailed_geo_nhs <- read_xlsx(paste0("data/raw-data/England and Wales/Sexual orientation/", orientation_drop_files[2]), sheet = 7, skip = 1) |>
  select(
    area_code = `NHS England regions code`,
    area_name = `NHS England regions`,
    sexual_orientation = `Sexual orientation (9 categories)`,
    n = Observation,
    percentage = starts_with("Percentage")
  ) |>
  mutate(area_level = "NHS England Region", .after = 2) |>
  mutate(n = as.numeric(n),
         percentage = as.numeric(percentage))

sexual_detailed_geo_icb <- read_xlsx(paste0("data/raw-data/England and Wales/Sexual orientation/", orientation_drop_files[2]), sheet = 8, skip = 1) |>
  select(
    area_code = `Integrated care boards code`,
    area_name = `Integrated care boards`,
    sexual_orientation = `Sexual orientation (9 categories)`,
    n = Observation,
    percentage = starts_with("Percentage")
  ) |>
  mutate(area_level = "Integrated Care Board", .after = 2) |>
  mutate(n = as.numeric(n),
         percentage = as.numeric(percentage))

sexual_detailed_geo_sicb <- read_xlsx(paste0("data/raw-data/England and Wales/Sexual orientation/", orientation_drop_files[2]), sheet = 9, skip = 1) |>
  select(
    area_code = `Sub integrated care board locations code`,
    area_name = `Sub integrated care board locations`,
    sexual_orientation = `Sexual orientation (9 categories)`,
    n = Observation,
    percentage = starts_with("Percentage")
  ) |>
  mutate(area_level = "Sub-integrated Care Board Location", .after = 2) |>
  mutate(n = as.numeric(n),
         percentage = as.numeric(percentage))

sexual_detailed_geo_local <- read_xlsx(paste0("data/raw-data/England and Wales/Sexual orientation/", orientation_drop_files[2]), sheet = 10, skip = 1) |>
  select(
    area_code = `Local health boards code`,
    area_name = `Local health boards`,
    sexual_orientation = `Sexual orientation (9 categories)`,
    n = Observation,
    percentage = starts_with("Percentage")
  ) |>
  mutate(area_level = "Local Health Board", .after = 2) |>
  mutate(n = as.numeric(n),
         percentage = as.numeric(percentage))

sexual_detailed_geo <- bind_rows(
  sexual_detailed_geo_ew, sexual_detailed_geo_country,
  sexual_detailed_geo_region, sexual_detailed_geo_utla,
  sexual_detailed_geo_ltla, sexual_detailed_geo_nhs,
  sexual_detailed_geo_icb, sexual_detailed_geo_sicb,
  sexual_detailed_geo_local
)
write_csv(sexual_detailed_geo, "data/additional-ew/sexual_detailed_geo.csv")

# LTLA by age and sex
sexual_age_sex <- read_csv(paste0("data/raw-data/England and Wales/Sexual orientation/", orientation_drop_files[1])) |>
  select(
    area_code = `Lower tier local authorities Code`,
    area_name = `Lower tier local authorities`,
    sexual_orientation = `Sexual orientation (4 categories)`,
    sex = `Sex (2 categories)`,
    age = `Age (D) (8 categories)`,
    n = Observation,
    percentage = starts_with("Percentage")
  ) |>
  mutate(area_level = "Lower Tier Local Authority", .after = 2)
write_csv(sexual_age_sex, "data/additional-ew/sexual_age_sex.csv")

# Other files that can be cleaned but aren't cleaned here due to time limitatipns
sexual_further_char_1 <- read_xlsx(paste0("data/raw-data/England and Wales/Sexual orientation/", orientation_drop_files[3]), sheet = 5, skip = 4)



