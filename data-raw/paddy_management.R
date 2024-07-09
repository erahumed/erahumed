library(dplyr)

# Data from https://doi.org/10.1016/j.scitotenv.2023.163018
# This comes from the personal account of local farmers, that told the authors
# of the above reference during which days the irrigated and drained the paddies
# in the April-September period.
# The first entry of both binary vectors correspond to the sowing day (04-20).
farmer_account <- readr::read_csv("data-raw/raw/paddy_management_farmer_account.csv")

paddy_management <-  # Prepare result data-frame
  tidyr::expand_grid(mm = 1:12, dd = 1:31) |>
  filter(case_when(  # Only actual calendar days (leap years cause no trouble)
    mm == 2               ~ dd <= 29,
    mm %in% c(4,6,9,11)   ~ dd <= 30,
    TRUE                  ~ TRUE
    )) |>
  mutate(sowing = mm == 4 & dd == 20) |>
  left_join(  # This brings in "irrigation" and "draining" logical columns
    farmer_account,
    by = c("mm", "dd")
    ) |>
  arrange(mm, dd) |>
  mutate(
    # Create the variable 'height_cm'. We assume that it takes two days for the
    # paddies to empty and refill.

    height_cm = case_when(
      irrigation & !lag(irrigation, default = FALSE) & !draining      ~ 5,
      irrigation & !draining                                          ~ 10,
      !irrigation & draining & lag(draining) & !lag(irrigation)       ~ 0,
      !irrigation & draining & lag(irrigation)                        ~ 5,
      !irrigation & !draining                                         ~ 0,
      irrigation & draining                                           ~ 10,
      TRUE                                                            ~ 0
      )) |>
  mutate(across(c(irrigation, draining), \(x) ifelse(is.na(x), FALSE, x))) |>
  tidyr::crossing(tibble(tancat = c(TRUE, FALSE))) |>
  mutate(
    # Corrections to irrigation/draining scheduling due to winter drowning

    irrigation = case_when(
      !tancat                                    ~ irrigation,
      mm == 11 & dd %in% 1:2                     ~ TRUE,
      mm %in% c(11, 12) | (mm == 1 & dd < 15)    ~ TRUE,
      mm == 1 & dd %in% 15:16                    ~ FALSE,
      TRUE                                       ~ irrigation
    ),

    draining = case_when(
      !tancat                                    ~ draining,
      mm == 11 & dd %in% 1:2                     ~ FALSE,
      mm %in% c(11, 12) | (mm == 1 & dd < 15)    ~ TRUE,
      mm == 1 & dd %in% 15:16                    ~ TRUE,
      TRUE                                       ~ draining
    ),

    height_cm = case_when(
      !tancat                                    ~ height_cm,
      mm == 11 & dd == 1 | mm == 1 & dd == 15    ~ 10,
      mm %in% c(11, 12) | (mm == 1 & dd < 15)    ~ 20,
      TRUE                                       ~ height_cm
    )
  ) |>
  tidyr::crossing(tibble(variety = c("J.Sendra", "Bomba", "Clearfield"))) |>
  mutate(
  # Corrections for rice variety: for 'Clearfield' there's one less emptying
    .p = variety == "Clearfield" & mm == 4 & dd %in% 24:28,
    irrigation = ifelse(.p, TRUE, irrigation),
    draining = ifelse(.p, TRUE, draining),
    height_cm = ifelse(.p, 10, height_cm)
  ) |>
  select(mm, dd, tancat, variety, sowing, irrigation, draining, height_cm) |>
  arrange(mm, dd, tancat, variety)

usethis::use_data(paddy_management, overwrite = TRUE)
