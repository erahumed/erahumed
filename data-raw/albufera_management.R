library(dplyr)

# Data from https://doi.org/10.1016/j.scitotenv.2023.163018
# This comes from the personal account of local farmers, that told the authors
# of the above reference during which days the irrigated and drained the paddies
# in the April-September period.
# The first entry of both binary vectors correspond to the sowing day (04-20).
farmer_account <- readr::read_csv("data-raw/raw/paddy_management_farmer_account.csv")

albufera_management <-  # Prepare result data-frame
  tidyr::expand_grid(mm = 1:12, dd = 1:31) |>
  filter(case_when(  # Only actual calendar days (leap years cause no trouble)
    mm == 2               ~ dd <= 29,
    mm %in% c(4,6,9,11)   ~ dd <= 30,
    TRUE                  ~ TRUE
    )) |>
  mutate(sowing = mm == 4 & dd == 20, seed_day = 1:n() - which(sowing)) |>
  left_join(  # This brings in "ideal_irrigation" and "ideal_draining" logical columns
    farmer_account,
    by = c("mm", "dd")
    ) |>
  arrange(mm, dd) |>
  mutate(
    # Create the variable 'ideal_height_eod_cm'. We assume that it takes two days for
    # the paddies to empty and refill.

    ideal_height_eod_cm = case_when(
      ideal_irrigation ~ ifelse(ideal_draining | lag(ideal_irrigation), 10, 5),
      ideal_draining   ~ ifelse(lag(ideal_irrigation), 5, 0),
      TRUE       ~ 0
    )

    ) |>
  mutate(across(c(ideal_irrigation, ideal_draining), \(x) ifelse(is.na(x), FALSE, x))) |>
  tidyr::crossing(tibble(tancat = c(TRUE, FALSE))) |>
  mutate(
    # Corrections to irrigation/draining scheduling due to winter drowning

    ideal_irrigation = case_when(
      !tancat                                    ~ ideal_irrigation,
      mm == 11 & dd %in% 1:2                     ~ TRUE,
      mm %in% c(11, 12) | (mm == 1 & dd < 15)    ~ TRUE,
      mm == 1 & dd %in% 15:16                    ~ FALSE,
      TRUE                                       ~ ideal_irrigation
    ),

    ideal_draining = case_when(
      !tancat                                    ~ ideal_draining,
      mm == 11 & dd %in% 1:2                     ~ FALSE,
      mm %in% c(11, 12) | (mm == 1 & dd < 15)    ~ TRUE,
      mm == 1 & dd %in% 15:16                    ~ TRUE,
      TRUE                                       ~ ideal_draining
    ),

    ideal_height_eod_cm = case_when(
      !tancat                                    ~ ideal_height_eod_cm,
      mm == 11 & dd == 1 | mm == 1 & dd == 15    ~ 10,
      mm %in% c(11, 12) | (mm == 1 & dd < 15)    ~ 20,
      TRUE                                       ~ ideal_height_eod_cm
    )
  ) |>
  tidyr::crossing(tibble(variety = c("J.Sendra", "Bomba", "Clearfield"))) |>
  mutate(
  # Corrections for rice variety: for 'Clearfield' there's one less emptying
    .p = variety == "Clearfield" & mm == 4 & dd %in% 24:28,
    ideal_irrigation = ifelse(.p, TRUE, ideal_irrigation),
    ideal_draining = ifelse(.p, TRUE, ideal_draining),
    ideal_height_eod_cm = ifelse(.p, 10, ideal_height_eod_cm)
  ) |>
  select(mm, dd, tancat, variety, sowing, seed_day, ideal_irrigation, ideal_draining, ideal_height_eod_cm) |>
  arrange(mm, dd, tancat, variety)

