# Rice Field Management System

Functions to define and initialize rice field management systems
(RFMSs).

## Usage

``` r
jsendra()

bomba()

clearfield()

new_rfms(
  sowing_yday = 113,
  harvesting_yday = 251,
  perellona_end_yday = 15,
  perellona_start_yday = 306,
  flow_height_cm = 10,
  perellona_height_cm = 20,
  display_name = "New Management System"
)
```

## Arguments

- sowing_yday:

  `[integer(1)]`  
  Day of the year marking the start of the sowing season (1–366,
  assuming a leap year).

- harvesting_yday:

  `[integer(1)]`  
  Day of the year marking the end of the sowing season (1–366, assuming
  a leap year).

- perellona_end_yday:

  `[integer(1)]`  
  Day of the year marking the end of the *Perellona* flooding period
  (before sowing).

- perellona_start_yday:

  `[integer(1)]`  
  Day of the year marking the beginning of the *Perellona* flooding
  period (after harvest).

- flow_height_cm:

  `[numeric(1)]`  
  Target water level (in cm) during the regular days of the sowing
  season, excluding emptying and transition days.

- perellona_height_cm:

  `[numeric(1)]`  
  Target water level (in cm) during the *Perellona* flooding period.

- display_name:

  `[character(1)]`  
  Name of the management system to be displayed in output plots and
  summaries.

## Value

An object of class `erahumed_rfms`.

## Details

These functions are used to define and initialize rice field management
systems. Specifically:

- `new_rfms()` creates an empty management system, with no scheduled
  chemical applications.

- `jsendra()`, `bomba()`, and `clearfield()` provide predefined systems
  inspired by the *J. Sendra*, *Bomba*, and *Clearfield* rice varieties,
  respectively.

Additional chemical applications can be scheduled using the helper
function
[`schedule_application()`](https://erahumed.github.io/erahumed/reference/schedule_application.md).
The default values of the arguments of `new_rfms()` coincide with those
used internally by `jsendra()`, `bomba()`, and `clearfield()`.

For a detailed explanation of RFMS concepts, configuration, and built-in
presets, see the [RFMS section of the user
manual](https://erahumed.github.io/erahumed-book/chapters/rfms.html).

## See also

[schedule_application](https://erahumed.github.io/erahumed/reference/schedule_application.md)
