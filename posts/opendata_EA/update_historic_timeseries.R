# 2023-05-14  Leo ran this manually to check
# and YES it is sourcable 
# NOTE if the EA doesnt have a file in it's archive (like 2023-04-04)
# the first stage will fail and crash and the second part wont even get run

library(tidyverse)
csv_path <- "C:/Users/leoki/DATA/EA/archive/"

all_archived_data <- list.files(csv_path, pattern = "^reading") %>%
  enframe(name = NULL, value = "filename") %>%
  mutate(archive_date = str_extract(filename, pattern = "[0-9]*-[0-9][0-9]-[0-9][0-9]")) %>%
  mutate(archive_date = lubridate::ymd(archive_date)) %>%
  arrange(desc(archive_date))

from_date <- max(all_archived_data$archive_date) + days(1)
to_date <- as.Date(Sys.Date()) - days(2) # -2 because EA is about 2 days lagging with archives
as.integer(to_date - from_date) * 50 / 1000
by_date <- 1


#r download the CSVs containing historic archives of EA measurements, warning=FALSE}

#| this code "Map"s a list of dates onto download.file.
#| "Map can be thought of as a loop.  it's the same as:
#| for (this_date in (list of dates) {
#|   create URL of csv from "https://envi[...]archive/readings-" & this_date
#|   download the file from the URL into the local directory defined by csv_path
#| }
#| NOTE: this will fail unglamorously if a CSV isnt present e.g. 2023-04-04
if(from_date < to_date) {
  Map(function(i) {
    f = str_c("https://environment.data.gov.uk/flood-monitoring/archive/readings-", i, ".csv")
    download.file(f, destfile = str_c(csv_path, basename(f)))
  }, seq.Date(from = from_date,
              to = to_date,
              by = by_date)
  )
  # this took 1hr 12min to get all 352 56Gb csvs
}

# push them into duckDB...
library(duckdb) #  implicitly loads required package DBI
duck_path <- "C:/Users/leoki/DATA/EA/duckDB/duckDB_EA"
# establish a connection to the duckDB database...
if(exists("con")) {
  dbDisconnect(con, shutdown=TRUE)
}
con <- dbConnect(duckdb(duck_path))

# {r access some data using dbplyr, warning=FALSE, message=FALSE}
# library(DBI)
library(dplyr)
library(dbplyr)

# get handles to the tables into the database

ea_mmt_tbl <- tbl(con, "ea_measurements") # the timeseries data
ea_stnmmt_tbl <- tbl(con, "ea_station_measurements") # context for the timeseries

ea_stnmmt_tbl %>%
  filter((lat > 51.3) & (lat < 51.6)) %>% #  a corridor a bit like the Thames
  select(measure, river_name, parameter_name, qualifier, unit_name) %>%
  filter(unit_name == "mm") %>%
  filter(qualifier == "Tipping Bucket Raingauge") %>%
  filter(parameter_name == "Rainfall") %>%
  count()

message(glue::glue("There are {ea_mmt_tbl %>% count() %>% collect() %>% as.integer()} measurements"))

all_dates_in_db <- ea_mmt_tbl %>%
  mutate(dt = as.Date(dateTime)) %>%
  count(dt) %>%
  arrange(desc(dt)) %>%
  collect()



# I'm appending here, so I dont want to recreate the table
if(F) {
  # only create this once
  dbExecute(con, "CREATE TABLE ea_measurements(
      dateTime TIMESTAMP,
      measure  VARCHAR(100),
      value DOUBLE);")
}

#| This code:
#|   1) gets a list of CSVs that can be loaded (as per previous code chunk)
#|   2) establishes a connection to a (new) duckDB database
#|   3) 
#| 1) get a list of files to import....
files <- dir(path = csv_path, pattern = "csv$", full.names = T)
#| NOTE this is written to only push the files collected in this itteraation
#| I should probably convert them to parquet
#| and just connect to the folder holding the parquet files?
all_files = dir(path = csv_path, pattern = "csv$", full.names = T) %>%
  as_tibble() %>% rename(filename = value) %>%
  mutate(archive_date = str_extract(filename, pattern = "[0-9]*-[0-9][0-9]-[0-9][0-9]")) %>%
  mutate(archive_date = lubridate::ymd(archive_date)) %>%
  arrange(desc(archive_date)) %>%
  # filter to exclude any dates that are already in the database
  filter(archive_date > max(all_dates_in_db$dt) )
files <- all_files$filename


message(glue::glue("There are {length(files)} new files to ingest"))
if(length(files) > 0) {
  message(glue::glue("Earliest new file date: {min(all_files$dt)}"))
  message(glue::glue("Latest new file date: {max(all_files$dt)}"))
  # if you wanted to manually pick-up after gaps caused the GET process to fail
  # manually adjust the start-date because the previous code will not have set
  # if correctly
  # from_date <- lubridate::ymd("2023-03-19")
  Sys.setenv(DUCKDB_NO_THREADS = 8)
  # create a new table called "ea_measurements"
  # and finally iterate over all the files,  copying them into the table
  # 50 files (50GMb each) tool
  #    user  system elapsed 
  #   18.76    3.54   27.72
  #   1.30    0.32    2.19  # for 5 56Mb files
  print(system.time({
    for(f in files) {
      q = sprintf("COPY ea_measurements FROM '%s' ( IGNORE_ERRORS TRUE );", f)
      dbExecute(con, q)
    }
  }))
} else {
  message("NO NEW FILE APPENDED TO DUCKDB")
}

ea_mmt_tbl %>%
  summarise(last_dt = max(dateTime, na.rm = T)) %>%
  collect()

if(exists("con")) {
  dbDisconnect(con, shutdown=TRUE)
}

