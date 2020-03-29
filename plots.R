library(dplyr)
library(ggplot2)
library(patchwork)

county <- readr::read_csv('us-counties.csv')%>%
  dplyr::left_join(tibble::tibble(
    state.abb = state.abb,
    state = state.name,state.region = state.region),
    by = "state")%>%
  dplyr::mutate(
    state.region = as.character(state.region),
    state.region = ifelse(grepl('^District',state),'South',state.region),
    state.abb = ifelse(is.na(state.region),state,state.abb),
    state.region = ifelse(is.na(state.region),'Territory',state.region),
  )

state <- readr::read_csv('us-states.csv')%>%
  dplyr::left_join(tibble::tibble(
    state.abb = state.abb,
    state = state.name,state.region = state.region),
    by = "state")%>%
  dplyr::mutate(
    state.region = as.character(state.region),
    state.region = ifelse(grepl('^District',state),'South',state.region),
    state.abb = ifelse(is.na(state.region),state,state.abb),
    state.region = ifelse(is.na(state.region),'Territory',state.region),
    )

dat_state <- state%>%
  dplyr::arrange(state.abb,date)%>%
  dplyr::group_by(state.abb)%>%
  dplyr::mutate_at(vars(cases,deaths),list(c=cumsum))%>%
  dplyr::mutate(w = strftime(date,'%U'))%>%
  dplyr::group_by(w)%>%
  dplyr::filter(date==max(date))%>%
  dplyr::ungroup()

dat_region <- dat_state%>%
  dplyr::group_by(state.region,w,date)%>%
  dplyr::summarise_at(vars(cases,cases_c),list(sum))%>%
  dplyr::group_by(w)%>%
  dplyr::filter(date==max(date))%>%
  dplyr::ungroup()

county_plot <- function(data,which_state){

  dat_county <- data%>%
    dplyr::filter(state.abb==which_state)%>%
    dplyr::arrange(county,date)%>%
    dplyr::group_by(county)%>%
    dplyr::mutate_at(vars(cases,deaths),list(c=cumsum))%>%
    dplyr::mutate(w = strftime(date,'%U'))%>%
    dplyr::group_by(w)%>%
    dplyr::filter(date==max(date))%>%
    dplyr::ungroup()

  county_labs <- dat_county%>%
    dplyr::filter(cases_c>max(cases_c)/10)%>%
    dplyr::group_by(county)%>%
    slice(n())

  dat_county%>%
    ggplot(aes(x=cases_c,y=cases)) +
    geom_abline(intercept=0,slope=1)+
    geom_path(alpha=0.5,aes(group=county)) +
    stat_smooth(geom='line',se = FALSE,colour='red') +
    geom_point(data = county_labs) +
    ggrepel::geom_label_repel(
      aes(label = county),
      data = county_labs,nudge_x = 10,segment.color = 'grey80',size = 2) +
    facet_wrap(~state) +
    scale_y_log10()+
    scale_x_log10() +
    labs(
      y = 'New Weekly Cases',
      x = 'Total Cases'
    )
}

state_labs <-   dat_state%>%
  dplyr::group_by(state.region,state.abb)%>%
  dplyr::filter(cases_c>max(cases_c)/10)%>%
  slice(n())

p_state <- dat_state%>%
  ggplot(aes(x=cases_c,y=cases)) +
  geom_abline(intercept=0,slope=1)+
  geom_path(alpha=0.25,aes(group=state.abb)) +
  stat_smooth(geom='line',se = FALSE,alpha= 0.5,colour='red') +
  geom_point(data = state_labs) +
  ggrepel::geom_label_repel(
    aes(label = state.abb),
    data = state_labs,nudge_x = 10,segment.color = 'grey80',size = 2) +
  facet_wrap(~state.region) +
  scale_y_log10()+
  scale_x_log10() +
  labs(
    title = 'State Level',
    y = 'New Weekly Cases',
    x = 'Total Cases'
  )

region_labs <-   dat_region%>%
  dplyr::filter(cases_c>max(cases_c)/10)%>%
  dplyr::group_by(state.region)%>%
  slice(n())

p_region <- dat_region%>%
  ggplot(aes(x=cases_c,y=cases,group=state.region)) +
  geom_abline(intercept=0,slope=1)+
  geom_path() +
  geom_point(data = region_labs) +
  ggrepel::geom_label_repel(
    aes(label = state.region),
    data = region_labs,nudge_x = 10,segment.color = 'grey80',size = 2) +
  scale_y_log10()+
  scale_x_log10() +
  labs(
    title = 'Region Level',
    y = 'New Weekly Cases',
    x = 'Total Cases'
  ) +
  theme(legend.position = 'top')

p_county <- purrr:::map(c('NY','CA','LA','FL'),county_plot,data = county)%>%purrr::reduce(`+`)

design <- 'AAB\nAAB\nAAB\nAAC'

p_all <- wrap_plots(p_county,p_state,p_region,design = design)

ggsave('layout.png',p_all,width = 10,height = 10)
