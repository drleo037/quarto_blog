# load libraries -----
library(tidyverse)  # this blog uses the tidyverse
library(lubridate)  # I'm sure lubridate has been added to the tidyverse, old habits die hard
library(httr)       # we will use this to collect data from the internet
source(here::here("../shiny/openRivers/functions.R")) #  the functions what I wrote

# this is a good read:
# https://david-charles.medium.com/thames-water-sewage-discharges-january-to-march-2023-1e6a2f7d2b0a
#
# gathering rainfall data from
# https://www.metoffice.gov.uk/hadobs/hadukp/data/daily/HadSEEP_daily_totals.txt

met_office_rain <- read.csv(file = url("https://www.metoffice.gov.uk/hadobs/hadukp/data/daily/HadSEEP_daily_totals.txt"), header = T, skip = 4) %>%
  as_tibble() %>%
  separate(col = 1, into = c("date", "rain_mo_mm"), sep = "[[:space:]]+") %>%
  mutate(date = lubridate::ymd(date)) %>%
  mutate(rain_mo_mm = parse_number(rain_mo_mm))

# collect thames event data -----
CLIENT_ID <- keyring::key_get(service = "thames_api", "CLIENT_ID")
CLIENT_SECRET <- keyring::key_get(service = "thames_api", "CLIENT_SECRET")
#| modify this URL as desired to access the different end points.
# CODE
# get everything by walking back in time without pre-defining an EDM list
if(TRUE) {
  tw_edm_df <- readRDS(file = here::here("tw_edm_df.Rds"))
} else {
  tw_edm_df <- list(
      collected_dt = with_tz(as_datetime(Sys.time()), tzone = "UTC"),
      events = load_edm_historic_data(client_id = CLIENT_ID,
                                               client_secret = CLIENT_SECRET,
                                               after_dt = as.Date("2019-03-31"),
                                               one_shot = FALSE) 
    )
  saveRDS(object = tw_edm_df, file = here::here("tw_edm_df.Rds"))
}
#| r use-case 1: which sites have the most discharge events in a given period ----
chatter_limit <- 10

most_frequently_discharging_edms <- tw_edm_df$events %>%
  count(location_name, alert_type, sort = T) %>%
  pivot_wider(names_from = alert_type, values_from = n) %>%
  rowwise() %>% 
  mutate(total_events = sum(c_across(where(is.numeric)), na.rm = T)) %>%
  ungroup() %>% 
  arrange(desc(total_events)) %>%
  mutate(rank = row_number(), .before = 1) %>%
  head (chatter_limit)

most_frequently_discharging_edms %>%
  DT::datatable(filter = 'top')


# use-case 2:  Reporting the total duration of discharges by site -----
#| first I write a function
#| (as I'm going to be doing the same pairing TWICE)
#| one for the start - stop events for each EDM
#| and again for the offline start / stop events
edm_ss_events <- pair_edm_events(tw_edm_df$events, start = "Start", stop = "Stop")
edm_ss_events <- pair_edm_events(tw_edm_df$events, start = "Start", stop = "Stop", now_dt = tw_edm_df$collected_dt)

edm_ss_events_offline <- pair_edm_events(tw_edm_df$events, start = "Offline start", stop = "Offline stop") 

top_ten_edms_by_discharge_duration <- edm_ss_events %>%
  group_by(location_name) %>%
  summarise(n = n(), total_hrs = sum(event_hrs), max_hrs = max(event_hrs)) %>%
  arrange(desc(total_hrs)) %>%
  head(10)

top_ten_edms_by_discharge_duration

# events still running:
edm_ss_events %>%
  filter(is.na(alert_type.stop))

# r plot paired events (fence & fence-posts) -----
event_timeline <- edm_ss_events %>%
  # I could filter by the top 10 here, but I want to rank them according to length too
  inner_join(top_ten_edms_by_discharge_duration) %>%
  arrange(desc(total_hrs)) %>%
  mutate(location_name = as_factor(location_name)) %>%
  mutate(location_name = fct_reorder(location_name, total_hrs)) %>%
  # some havent ended yet, so make the end "NOW!" to they're plotted
  mutate(date_time.stop = ifelse(is.na(date_time.stop), as.POSIXct(collected_dt, origin = "UTC"), date_time.stop)) %>%
  mutate(date_time.stop = as.POSIXct(date_time.stop, origin = origin)) %>%
  # mutate(start_dt = date_time.start - lubridate::days(1)) %>%
  # mutate(end_dt = date_time.stop + lubridate::days(1)) %>%
  ggplot() +
  coord_flip() +
  # geom_segment( aes(x=location_name,
  #                   xend=location_name,
  #                   y=start_dt,
  #                   yend=end_dt),
  #               linewidth = 8,
  #               lineend = "butt",
  #               color="black") +
  geom_segment( aes(x=location_name,
                    xend=location_name,
                    y=date_time.start,
                    yend=date_time.stop,
                    color=location_name),
                linewidth = 7) +
  geom_hline(yintercept = tw_edm_df$collected_dt) +
  # geom_point( aes(x=location_name, y=date_time.start), shape = 20, color="green", size=7 ) +
  # geom_point( aes(x=location_name, y=date_time.stop), shape = 15, color="red", size=3 ) +
  theme(
    legend.position = "none",
  ) +
  labs(title = "Top 10 longest discharging EMDs since 2022-04-01",
       subtitle = "Sites have been ranked in descending total discharge duration",
       caption = "Data from Thames Water {api_root}/data/STE/v1/DischargeAlerts",
       tag = "", 
       x = "EDM location", y = "date")


plotly::ggplotly(event_timeline)


# {r access some EA data from duckDB using dbplyr -----
library(duckdb) #  implicitly loads required package DBI
library(dplyr)
library(dbplyr)
duck_path <- "C:/Users/leoki/DATA/EA/duckDB/duckDB_EA"
# establish a connection to the duckDB database...
# make sure no processes have it open at present
if(exists("con")) {
  dbDisconnect(con, shutdown=TRUE)
}
con <- dbConnect(duckdb(duck_path))


#| get handles to the tables into the database
#| dont collect the data back though, we're going to be making the most of
#|  1) duckDB's efficiency and...
#|  2) dbplyr's mapping from dplyr syntax to SQL
#| to write nice code that returns results from bigish data very quickly
#|  
ea_mmt_tbl <- tbl(con, "ea_measurements") # the time-series data
ea_stnmmt_tbl <- tbl(con, "ea_station_measurements") # context for the time-series

# I will collect ea_stnmmt_tbl as it has coordinates in 
# and maybe I'll spatially join the EA data to the TW data later
ea_stnmmt <- collect(ea_stnmmt_tbl)

# gather rainfail data let duckdb do the heavy lifting and then collect the results ----
tictoc::tic()
ea_daily_readings_rain <- ea_mmt_tbl %>%
  #  select(-measure) %>%
  inner_join(
    ea_stnmmt_tbl %>% 
      filter((lat > 51.3) & (lat < 51.6)) %>% #  a corridor a bit like the Thames
      select(measure, river_name, parameter_name, qualifier, unit_name) %>%
      filter(unit_name == "mm") %>%
      filter(qualifier == "Tipping Bucket Raingauge") %>%
      filter(parameter_name == "Rainfall")
  ) %>%
  mutate(date = as.Date(dateTime)) %>%
  filter(!is.nan(value)) %>%
  group_by(date, measure, parameter_name, qualifier, unit_name) %>%
  summarise(value = sum(value, na.rm = T), n = n()) %>% # show_query()
  ungroup() %>%
  collect()
tictoc::toc()
# 16.89 sec elapsed

# gather stage data let duckdb do the heavy lifting and then collect the results ----
tictoc::tic()
ea_daily_readings_stage <- ea_mmt_tbl %>%
  #  select(-measure) %>%
  inner_join(
    ea_stnmmt_tbl %>%
      filter((lat > 51.3) & (lat < 51.6)) %>% #  a corridor a bit like the Thames
      select(measure, river_name, parameter_name, qualifier, unit_name) %>%
      #      filter(river_name == "River Thames") # %>% head(5) #  only get data for 10 places along the thames
      filter(qualifier == "Stage") %>%
      filter(unit_name == "mASD")
  ) %>%
  mutate(date = as.Date(dateTime)) %>%
  group_by(date, measure, parameter_name, qualifier, unit_name) %>%
  filter(!is.nan(value)) %>%
  summarise(value = mean(value, na.rm = T)) %>%
  ungroup() %>%
  collect()
tictoc::toc()
# 11.92 seconds (14.89 sec elapsed on battery)

# we're done, it's good practice to close the connection as follows ----
# note: DBI::dbDisconnect(con) seems to leave some stale handles without shutdown
dbDisconnect(con, shutdown=TRUE)

# agrees well with #
# https://www.weatheronline.co.uk/weather/maps/city?LANG=en&WMO=03772&ART=PRE&CONT=ukuk&R=0&LEVEL=150&REGION=0003&LAND=__&NOREGION=0&MOD=&TMX=&TMN=&SON=&PRE=&MONAT=&OFFS=&SORT=
ea_daily_readings_rain %>%
  rename(rain_ea_mm = value) %>%
  filter(n==96) %>%
  group_by(date) %>%
  summarise(rain_ea_mm = mean(rain_ea_mm)) %>%
  filter(date > ymd("2023-04-11")) %>%
  filter(date < ymd("2023-05-10")) %>%
  ggplot() +
  geom_col(aes(x = date, y = rain_ea_mm), fill = "lightblue", colour = "blue") +
  ylim(0, 10) +
  labs(title = "Average daily rainfall in the Thames corridor",
       subtitle = glue::glue("Using data from {length(unique(ea_daily_readings_rain$measure))} measures"),
       caption = "EA data from: https://environment.data.gov.uk/flood-monitoring/archive",
       x = "Date", y = "Rainfall (mm)")
#
ea_daily_readings_rain %>%
  rename(rain_ea_mm = value) %>%
  filter(n==96) %>%
  filter(date > ymd("2023-04-11")) %>%
  filter(date < ymd("2023-05-10")) %>%
  mutate(measure = basename(measure)) %>%
  ggplot() +
  geom_line(aes(x = date, y = rain_ea_mm, colour = as.factor(measure)),
             alpha= 0.1,
            show.legend = FALSE) +
  geom_point(aes(x = date, y = rain_ea_mm, colour = as.factor(measure)),
             alpha= 0.1,
             #            shape = 15,
             size = 2,
             stroke = 0,
             show.legend = FALSE) +
  ylim(0, 15) +
  labs(title = "Plotting individual measures helps show the variability in rainfall across the region",
       subtitle = "e.g. Just before the 24th April rainfall was patchy, after it was everywhere",
       caption = "EA data from: https://environment.data.gov.uk/flood-monitoring/archive",
       x = "Date", y = "Rainfall (mm)")


# sensecheck plot
p <- ea_daily_readings_stage %>%
  mutate(measure = basename(measure)) %>%
  group_by(date) %>%
  summarise(stage_level_m = median(value, na.rm = T)) %>%
  inner_join(
    ea_daily_readings_rain %>%
      mutate(measure = basename(measure)) %>%
      group_by(date) %>%
      summarise(rain_ea_mm = median(value, na.rm = T))
  ) %>%
  pivot_longer(c(stage_level_m, rain_ea_mm), names_to = "measurand", values_to = "reading") %>%
  ggplot() +
  geom_line(aes(x = date, y = reading, colour = measurand)) +
  facet_wrap( ~ measurand, ncol = 1, scales = "free") +
  labs(title = "Historic rainfall and river levels", subtitle = "EA data filtered and summarised", caption = "data from EA historic archives", x = "date", y = "")

p
plotly::ggplotly(p)

edm_timeseries <- seq.Date(from = ymd("2022-04-01"), to = ymd("2023-05-10"), by = 1) %>%
  enframe(name = NULL, value = "date") %>%
  cross_join(edm_ss_events) %>%
  # create a column for whether each event is active on the date
  mutate(active = date >= as.Date(date_time.start) & date < as.Date(date_time.stop)) %>%
  filter(active) %>%
  transmute(date, location_name, active, event_start = as.Date(date_time.start), event_stop = as.Date(date_time.stop)) 

edm_timeseries_offline <- seq.Date(from = ymd("2022-04-01"), to = ymd("2023-05-10"), by = 1) %>%
  enframe(name = NULL, value = "date") %>%
  cross_join(edm_ss_events_offline) %>%
  # create a column for whether each event is active on the date
  mutate(active = date >= as.Date(date_time.start) & date < as.Date(date_time.stop)) %>%
  filter(active) %>%
  transmute(date, location_name, active, event_start = as.Date(date_time.start), event_stop = as.Date(date_time.stop)) 

edm_timeseries %>%
  # summarize to get the total number of active events for each date
  group_by(date) %>%
  summarize(discharging_edm_count = n()) %>%
  ggplot() +
  geom_line(aes(x = date, y = discharging_edm_count))

edm_timeseries_offline %>%
  # summarize to get the total number of active events for each date
  group_by(date) %>%
  summarize(discharging_edm_count = n()) %>%
  ggplot() +
  geom_line(aes(x = date, y = discharging_edm_count))

#
ea_tw_ts <- ea_daily_readings_stage %>%
  mutate(measure = basename(measure)) %>%
  group_by(date) %>%
  summarise(stage_level_m = median(value, na.rm = T)) %>%
  inner_join(
    ea_daily_readings_rain %>%
      mutate(measure = basename(measure)) %>%
      group_by(date) %>%
      summarise(rain_ea_mm = median(value, na.rm = T))
  )  %>%
  inner_join(
    met_office_rain
  ) %>%
  inner_join(
    edm_timeseries %>%
      # summarize to get the total number of active events for each date
      group_by(date) %>%
      summarize(discharging_edm_count = n()) 
  ) %>%
  inner_join(
    edm_timeseries_offline %>%
      # summarize to get the total number of active events for each date
      group_by(date) %>%
      summarize(discharging_edm_OL_count = n()) 
  )

# sensecheck plot
ea_tw_ts %>%
  pivot_longer(cols = c(stage_level_m, rain_ea_mm, discharging_edm_count, discharging_edm_OL_count, rain_mo_mm),
               names_to = "measurand",
               values_to = "reading") %>%
  ggplot() +
  geom_line(aes(x = date, y = reading, colour = measurand)) +
  facet_wrap( ~ measurand, ncol = 1, scales = "free") +
  labs(title = "Historic rainfall and river levels", subtitle = "EA data filtered and summarised", caption = "data from EA historic archives", x = "date", y = "")

GGally::ggpairs(ea_tw_ts)
ea_tw_ts %>%
  filter(date >= ymd("2023-01-01")) %>%
  GGally::ggpairs()

# sensecheck plot
ea_tw_ts %>%
  filter(date >= ymd("2023-01-01")) %>%
  pivot_longer(cols = c(stage_level_m, rain_ea_mm, rain_mo_mm, discharging_edm_count, discharging_edm_OL_count),
               names_to = "measurand",
               values_to = "reading") %>%
  ggplot() +
  geom_line(aes(x = date, y = reading, colour = measurand)) +
  facet_wrap( ~ measurand, ncol = 1, scales = "free") +
  labs(title = "Historic rainfall and river levels", subtitle = "EA data filtered and summarised", caption = "data from EA historic archives", x = "date", y = "")


ea_tw_ts %>%
#  filter(date >= ymd("2023-01-01")) %>%
  ggplot() +
  geom_line(aes(x = date, y = rain_mo_mm), colour = "blue") +
  geom_line(aes(x = date, y = rain_ea_mm), colour = "green")

p <- ea_tw_ts %>%
  filter(date >= ymd("2023-01-01")) %>%
  ggplot() +
  geom_point(aes(x = stage_level_m, y = discharging_edm_count),
             colour = "blue", alpha = 0.3)  +
  labs(title = "The count of EDMs discharging is highly related to river levels",
       subtitle = "Each point represents one day",
       caption = "EA data from flood-monitoring archive, TW data from their API Endpoint 1: DTE - Alert Stream",
       x = "EA River gauge levels", y = "Count of TW EDMs discharging")

ggExtra::ggMarginal(p, type = "histogram")

ea_tw_ts %>%
  filter(date >= ymd("2023-01-01")) %>%
  arrange(date) %>%
  mutate(tma = zoo::rollmean(rain_ea_mm, k = 3, fill = NA, align = "right")) %>%
#  filter(date >= ymd("2023-01-01")) %>%
  pivot_longer(cols = c(stage_level_m, discharging_edm_count, rain_ea_mm, tma),
               names_to = "measurand",
               values_to = "reading") %>%
  ggplot() +
  geom_line(aes(x = date, y = reading, colour = measurand)) +
  facet_wrap( ~ measurand, ncol = 1, scales = "free") +
  labs(title = "Historic rainfall and river levels", subtitle = "EA data filtered and summarised", caption = "data from EA historic archives", x = "date", y = "")


ea_tw_ts %>%
  filter(date >= ymd("2023-01-01")) %>%
  arrange(date) %>%
  mutate(tma = zoo::rollmean(rain_ea_mm, k = 3, fill = NA, align = "right")) %>%
  ggplot() +
  geom_point(aes(x = tma, y = discharging_edm_count), # stage_level_m),
             colour = "blue", alpha = 0.3)

# make a noise that this step has been completed
beepr::beep()
# buy a bit of time because beepr can cause crashes
Sys.sleep(5)
