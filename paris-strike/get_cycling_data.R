suppressMessages(library(dplyr, quietly = T))
# source https://parisdata.opendatasoft.com/explore/dataset/comptage-velo-donnees-compteurs/information/?disjunctive.id&disjunctive.name&sort=date&dataChart=eyJxdWVyaWVzIjpbeyJjaGFydHMiOlt7InR5cGUiOiJhcmVhc3BsaW5lIiwiZnVuYyI6IkFWRyIsInlBeGlzIjoiY291bnRzIiwic2NpZW50aWZpY0Rpc3BsYXkiOnRydWUsImNvbG9yIjoiI0ZGQ0QwMCJ9XSwieEF4aXMiOiJkYXRlIiwibWF4cG9pbnRzIjoiIiwidGltZXNjYWxlIjoiaG91ciIsInNvcnQiOiIiLCJjb25maWciOnsiZGF0YXNldCI6ImNvbXB0YWdlLXZlbG8tZG9ubmVlcy1jb21wdGV1cnMiLCJvcHRpb25zIjp7ImRpc2p1bmN0aXZlLmlkIjp0cnVlLCJkaXNqdW5jdGl2ZS5uYW1lIjp0cnVlLCJzb3J0IjoiZGF0ZSIsInEudGltZXJhbmdlLmRhdGUiOiJkYXRlOlsyMDE5LTEyLTExVDIzOjAwOjAwWiBUTyAyMDE5LTEyLTE5VDIyOjU5OjU5Wl0ifX19XSwiZGlzcGxheUxlZ2VuZCI6dHJ1ZSwiYWxpZ25Nb250aCI6dHJ1ZSwidGltZXNjYWxlIjoiIn0%3D
# read data with csv2 since sep is ';'
cycling_raw <- readr::read_csv2(file = "paris-strike/data/comptage-velo-donnees-compteurs.csv", 
                            col_types = "dcdTDcc")
head(cycling_raw)

cycling <- cycling_raw %>% 
  dplyr::rename(
    "location_id" = "Identifiant du point de comptage",
    "location_name" = "Nom du point de comptage",
    "count_hourly" = "Comptage horaire",
    "timestamp" = "Date et heure de comptage",
    "counter_installation_date" = "Date d'installation du point de comptage",
    "url_location_photo" = "Lien vers photo du point de comptage",
    "location_coordinates" = "Coordonnées géographiques"
  ) %>%
  dplyr::filter(
    # ignore NA hourly_count
    !is.na(count_hourly),
    count_hourly > 0
  ) %>%
  dplyr::mutate(
    date = lubridate::date(timestamp),
    wday = lubridate::wday(date, week_start = 1),
    is_weekend = if_else(wday >= 6 & wday <= 7, 1, 0),
    location_latitude = sub(pattern = ',.+', '', location_coordinates),
    location_latitude = as.numeric(location_latitude),
    location_longitude = sub(pattern = '.+,', '', location_coordinates),
    location_longitude = as.numeric(location_longitude)
  ) %>%
  dplyr::select(-location_coordinates, -url_location_photo)
head(cycling)
remove(cycling_raw)

cycling_daily <- cycling %>%
  dplyr::group_by(
    location_id, 
    location_name, 
    counter_installation_date, 
    date, 
    wday,
    is_weekend,
    location_latitude, 
    location_longitude
    ) %>%
  dplyr::summarise(count_daily = sum(count_hourly)) %>%
  dplyr::ungroup()
