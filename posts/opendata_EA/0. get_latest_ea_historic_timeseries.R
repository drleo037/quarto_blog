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

if (nrow(all_archived_data) > 0) {
  from_date <- max(all_archived_data$archive_date) + days(1)
} else {
  from_date <- lubridate::ymd("2023-01-28")
  from_date <- lubridate::ymd("2023-04-05")
  from_date <- lubridate::ymd("2023-05-13")
  from_date <- lubridate::ymd("2024-01-16")
  from_date <- as.Date(Sys.Date()) - days(14) # first time called there will be no csvs.  go back 2 weeks
}
to_date <- as.Date(Sys.Date()) - days(2) # -2 because EA is about 2 days lagging with archives
as.integer(to_date - from_date) * 50 / 1000
by_date <- 1


#r download the CSVs containing historic archives of EA measurements, warning=FALSE}

# this code "Map"s a list of dates onto download.file.
# "Map can be thought of as a loop.  it's the same as:
# for (this_date in (list of dates) {
#   create URL of csv from "https://envi[...]archive/readings-" & this_date
#   download the file from the URL into the local directory defined by csv_path
# }
# NOTE: this will fail unglamorously if a CSV isnt present e.g. 2023-04-04
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
# https://environment.data.gov.uk/flood-monitoring/archive/readings-2024-02-17.csv