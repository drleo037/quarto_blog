# 2023-05-14  Leo ran this manually to check
# and YES it is sourcable 
# NOTE if the EA doesnt have a file in it's archive (like 2023-04-04)
# the first stage will fail and crash and the second part wont even get run

library(tidyverse)
csv_path <- "C:/Users/leoki/DATA/EA/archive/"


# push the CSVs into duckDB...
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

# I'm appending here, so I dont want to recreate the table
if(F) {
  dbExecute(con, "DROP TABLE ea_measurements")
  # only create this once
  dbExecute(con, "CREATE TABLE ea_measurements(
      dateTime TIMESTAMP,
      measure  VARCHAR(100),
      value DOUBLE);")
}


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



# This code:
#   1) gets a list of CSVs that can be loaded (as per previous code chunk)
#   2) establishes a connection to a (new) duckDB database
#   3) 
# 1) get a list of files to import....
files <- dir(path = csv_path, pattern = "csv$", full.names = T)
# NOTE this is written to only push the files collected in this iteration
# I should probably convert them to parquet
# and just connect to the folder holding the parquet files?

all_files = dir(path = csv_path, pattern = "csv$", full.names = T) %>%
  as_tibble() %>% rename(filename = value) %>%
  mutate(archive_date = str_extract(filename, pattern = "[0-9]*-[0-9][0-9]-[0-9][0-9]")) %>%
  mutate(archive_date = lubridate::ymd(archive_date)) %>%
  arrange(desc(archive_date))

most_recent_date = max(all_dates_in_db$dt)
if(most_recent_date == -Inf) {
  most_recent_date = min(all_files$archive_date)
}


new_files <- all_files %>%
  # filter to exclude any dates that are already in the database
  filter(archive_date > most_recent_date) %>%
  arrange((archive_date))
new_files

filenames <- new_files$filename

if(F) {
  f <- 'C:/Users/leoki/DATA/EA/archive/readings-2023-01-19.csv'
  q = sprintf("COPY ea_measurements FROM '%s' ( IGNORE_ERRORS TRUE);", f)
  dbExecute(con, q)
  dat = vroom::vroom(f, delim = ",")
  problems(dat)
}

message(glue::glue("There are {length(files)} new files to ingest"))
if(length(filenames) > 0) {
  message(glue::glue("Earliest new file date: {min(all_files$archive_date)}"))
  message(glue::glue("Latest new file date: {max(all_files$archive_date)}"))
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
    for(f in filenames) {
      # sprintf("COPY ea_measurements FROM '%s' ( ignore_errors TRUE,  DELIMITER ',', HEADER);", f)
      #   INSERT INTO ea_measurements SELECT * FROM read_csv(['flights1.csv', 'flights2.csv'], union_by_name = true, filename = true);
      q = sprintf("INSERT INTO ea_measurements SELECT * FROM read_csv('%s', columns = {'dateTime': 'DATETIME', 'measure': 'VARCHAR', 'value': 'double'}, AUTO_DETECT=TRUE, IGNORE_ERRORS = true);", f)
      q = sprintf("INSERT INTO ea_measurements SELECT * FROM read_csv('%s', columns = {'dateTime': 'DATETIME', 'measure': 'VARCHAR', 'value': 'double'}, IGNORE_ERRORS = true, parallel=false);", f)
      print(q)
     dbExecute(con, q)
    }
    # "C:/Users/leoki/DATA/EA/archive/readings-2023-01-02.csv"
    # line 64307 is malformed
  }))
} else {
  message("NO NEW FILE APPENDED TO DUCKDB")
}

ea_mmt_tbl %>%
  summarise(last_dt = max(dateTime, na.rm = T)) %>%
  collect()

ea_mmt_tbl %>%
  mutate(date = as.Date(dateTime)) %>%
  group_by(date) %>%
  summarise(n_samples = count()) %>%
  collect() 


if(exists("con")) {
  dbDisconnect(con, shutdown=TRUE)
}

