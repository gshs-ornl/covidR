#' @title       retrieve the timzone lookup table from wikipedia
#' @description this function is used to create the dataset to lookup
#' @return      a data.table ready for insertion into a database or other use
retrieve_timzone_lut <- function() {
  wiki_url <- 'https://en.wikipedia.org/wiki/List_of_tz_database_time_zones'
  page <- get_page(wiki_url)
  table <- rvest::html_table(page, trim = TRUE, fill = TRUE)[[1]]
  names(table) <- snakecase::to_snake_case(names(table))
  wiki_data <- data.table::as.data.table(table)
  wiki_data[, offset := paste('UTC', utc_dst_offset_hh_mm)]
  td_url <- 'https://www.timeanddate.com/time/zones/'
  page <- get_page(td_url)
  table <- rvest::html_table(page, trim = TRUE, fill = TRUE)[[1]]
  names(table) <- c('abb', 'name', 'location', 'offset')
  td_data <- data.table::as.data.table(table)
  td_data[, `:=`( name = gsub('(\\t|\\n)*', '', name))]
  wts_url <- 'https://www.worldtimeserver.com/time-zones/'
  page <- get_page(wts_url)
  table <- rvest::html_table(page, trim = TRUE, fill = TRUE)
  table <- data.table::rbindlist(table)[, -3]
  names(table) <- c('abb', 'name', 'offset')
  wts_data <- table
  lut <- merge(td_data, wts_data, by = 'abb', all = TRUE)
  names(lut) <- c('abb', 'td_name', 'location', 'td_offset', 'wts_name',
                  'wts_offset')
  tzlut <- merge(lut, wiki_data, by.x = 'wts_offset', by.y = 'offset')
  tzd_country <- data.table::fread('data-raw/country.csv', header = FALSE,
                                   col.names = c('country_code',
                                                 'country_name'))
  timezone <- data.table::fread('data-raw/timezone.csv', header = FALSE,
                                col.names = c('zone_id', 'abbreviation',
                                              'time_start', 'gmt_offset',
                                              'dst'))[, c(-3, -4)]
  zone <- data.table::fread('data-raw/zone.csv', header = FALSE,
                            col.names = c('zone_id', 'country_code',
                                          'zone_name'))
  tzdt <- unique(merge(timezone, zone, by = 'zone_id'))
  tzdt <- unique(merge(tzdt, tzd_country, by = 'country_code'))
  return(tzlut)
}