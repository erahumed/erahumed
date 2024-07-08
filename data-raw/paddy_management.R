library(dplyr)

# TODO:
# 0. What data do we need at the end of the day? Is it necessary to have one row
#    per day  since 2010?
# 1. Review the logic corresponding to yday (are leap years treated correctly?)
# 2. Where do the binary vectors (and the logic used to process them) come from?
# 3. Can the overall logic be made more transparent?
# 4. What do we need from this dataset?

# Why?
dates <-
  # Really necessary?
  tibble(date = seq.Date(from = as.Date("2010-01-01"),
                         to  = as.Date("2020-12-31"),
                         by = "day")
         ) |>
  mutate(yday = as.numeric(format(date, "%j")))

# TODO: check that there are no errors related with leap years
yday <- c(110:251) # 04-20 to 09-08 (or 04-19 to 09-07 in leap years)

seeding <- numeric(length(yday))
seeding[1] <- 1
Tcrop <- seq_along(seeding) - 1

# TODO: explain this
irri_binary <- c(0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0,
                 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
                 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
drain_binary <- c(0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
                  0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)
irri_binary[(length(irri_binary) + 1):length(yday)] <- NA  # V: matching lengths
drain_binary[(length(drain_binary) + 1):length(yday)] <- NA

paddy_management <- tibble(irri_binary, drain_binary, seeding, Tcrop, yday) |>

  right_join(dates, by = "yday") |>
  tidyr::crossing(tibble(tancat = c("0", "1"))) |>
  tidyr::crossing(tibble(mngmt = c("J.Sendra", "Bomba", "Clearfield"))) |>

  mutate( # TODO: double check
    across(c(irri_binary, drain_binary, seeding, Tcrop),
           \(x) ifelse(is.na(x), 0, x))
    ) |>

  mutate( # TODO: Explain this logic
    height_cm = case_when(
      irri_binary == 1 & lag(irri_binary, default = 0) == 0 & drain_binary == 0 ~ 5,
      irri_binary == 1 & drain_binary == 0 ~ 10,
      irri_binary == 0 & drain_binary == 1 & lag(drain_binary) == 1 & lag(irri_binary) == 0 ~ 0,
      irri_binary == 0 & drain_binary == 1 & lag(irri_binary) == 1 ~ 5,
      irri_binary == 0 & drain_binary == 0 ~ 0,
      irri_binary == 1 & drain_binary == 1 ~ 10,
      TRUE ~ 0
      )) |>

  mutate(
    # TODO: Explain this logic. In particular, is it ok that this mutate block
    # comes after the previous one?
    .mm = as.numeric(format(date, "%m")),
    .dd = as.numeric(format(date, "%d")),
    .mmdd = format(date, "%m-%d"),
    .t = tancat == "1",

    irri_binary = case_when(
      !.t                                        ~ irri_binary,
      .mmdd %in% c("11-01", "11-02")             ~ 1,
      .mm %in% c(11, 12) | (.mm == 1 & .dd < 15) ~ 1,
      .mmdd %in% c("01-15", "01-16")             ~ 0,
      TRUE                                       ~ irri_binary
    ),

    drain_binary = case_when(
      !.t                                        ~ drain_binary,
      .mmdd %in% c("11-01", "11-02")             ~ 0,
      .mm %in% c(11, 12) | (.mm == 1 & .dd < 15) ~ 1,
      .mmdd %in% c("01-15", "01-16")             ~ 1,
      TRUE                                       ~ drain_binary
    ),

    height_cm = case_when(
      !.t                                        ~ height_cm,
      .mmdd %in% c("11-01", "01-15")             ~ 10,
      .mm %in% c(11, 12) | (.mm == 1 & .dd < 15) ~ 20,
      TRUE                                       ~ height_cm
    )
  ) |>

  mutate( # TODO: Explain this logic. In particular, same as above
    .p = mngmt == "Clearfield" & Tcrop %in% c(4, 5, 6, 7, 8),
    irri_binary = ifelse(.p, 1, irri_binary),
    drain_binary = ifelse(.p, 1, drain_binary),
    height_cm = ifelse(.p, 10, height_cm)
  ) |>

  select(-starts_with(".")) |>

  arrange(date, mngmt, tancat)

usethis::use_data(paddy_management, overwrite = TRUE)
