lockdown_kaggle <- readr::read_csv('lockdown_us.csv')

lockdown_dc <- tibble::tibble(state = 'District of Columbia', dates = 'April 04 - May 29', order = 'Stay at Home')

lockdown_url <- 'https://ballotpedia.org/Status_of_lockdown_and_stay-at-home_orders_in_response_to_the_coronavirus_(COVID-19)_pandemic,_2020'

lockdown_html <- xml2::read_html(lockdown_url)

lockdown_tables <- lockdown_html%>%rvest::html_table(fill = TRUE)

lockdown_table <- lockdown_tables[[3]][,c(-3)]
names(lockdown_table) <- lockdown_table[1,]
names(lockdown_table) <- c('state','dates','order')
lockdown_table <- lockdown_table[-1,]
lockdown_table$dates <- gsub('[\\[\\(](.*?)$','',lockdown_table$dates)
lockdown_table <- dplyr::bind_rows(lockdown_table,lockdown_dc)
lockdown_table$start <- as.Date(gsub('- (.*?)$','',lockdown_table$dates),format = '%b %d')
lockdown_table$end <- as.Date(gsub('^(.*?)- ','',lockdown_table$dates),format = '%b %d')

gh_lockdown <- readr::read_csv('https://raw.githubusercontent.com/COVID19StatePolicy/SocialDistancing/master/data/USstatesCov19distancingpolicy.csv')

l <- 14

plot_dat <- dat %>%
  dplyr::mutate(state.abb=ifelse(is.na(state.abb),'DC',state.abb))%>%
  dplyr::select(date, state.region,state.abb, state,cases_new, deaths_new_c, cases_new_c, deaths_new_c) %>%
  dplyr::group_by(state.abb) %>%
  dplyr::mutate(cases = lag(cases_new_c, l),
                n = dplyr::row_number(), nn = dplyr::n()) %>%
  dplyr::filter(n <= (nn - (l+2))) %>%
  dplyr::filter(!is.na(deaths_new_c))

plot_dat <- plot_dat%>%dplyr::left_join(lockdown_table%>%dplyr::select(-dates),by='state')%>%
  dplyr::mutate(lockdown = as.numeric(dplyr::between(date,start,end)))

# create equidistant sequence of dates to use as labels
lab_dates <- pretty(plot_dat$date)

plot_dat%>%
  dplyr::filter(state.region %in% c("West")) %>%
  ggplot(aes(x = cases, y = deaths_new_c)) +
  geom_path(aes(colour = lockdown),na.rm = TRUE) +
  facet_wrap(~state.abb, scales = "free") +
  labs(
    x = glue::glue('New Cases 7 Day Rolling Sum\n Lag {l} days'),
    y = 'New Deaths 7 Day Rolling Sum',
    colour = 'Lockdown'
    ) +
  viridis::scale_colour_viridis(breaks = c(1,0),labels = c('yes','no'),option = 'A',end = 0.75)


plot_dat <- world_roll%>%
  dplyr::mutate(date = as.Date(date)) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(cases = lag(cases_new_c, l),
                n = dplyr::row_number(), nn = dplyr::n()) %>%
  dplyr::filter(n <= (nn - (l+2))) %>%
  dplyr::filter(!is.na(deaths_new_c))

# create equidistant sequence of dates to use as labels
lab_dates <- pretty(plot_dat$date)

plot_dat%>%
  dplyr::filter(grepl('America',region)) %>%
  ggplot(aes(x = cases, y = deaths_new_c)) +
  geom_path(aes(colour = as.numeric(date)),na.rm = TRUE) +
  facet_wrap(~country, scales = "free") +
  labs(
    x = glue::glue('New Cases 7 Day Rolling Sum\n Lag {l} days'),
    y = 'New Deaths 7 Day Rolling Sum',
    colour = 'Date'
  ) +
  viridis::scale_colour_viridis(breaks = as.numeric(lab_dates), labels = lab_dates,direction = -1)
