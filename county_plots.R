system('git fetch upstream')
system("git merge upstream/master -m 'merge upstream'")
library(magrittr)
library(dplyr)
library(ggplot2)
library(urbnmapr)
source('read_data.R')
source('foos.R')
facet_by <- 'state'
path_by <- 'county'
metric <- 'cases'
mf <- c('04-20','05-20')
h_fl <- c('Alachua','Columbia','Levy','Marion','Putnam','Bradford','Clay','Suwannee','Gilchrist','Union')

h_ct <- 'Hartford'
h_ma <- 'Middlesex'
h_il <- 'Cook'
h_fl <- 'Alachua'
highlight <- c(h_fl)

#highlight <- c('MA','CT','FL')

dat <- county_input%>%
  dplyr::filter(date>=as.Date('2020-03-01'))%>%
  # dplyr::group_by(date)%>%
  # dplyr::summarise_at(dplyr::vars(cases, deaths, cases_new, deaths_new),list(sum))%>%
  dplyr::mutate(
    country = 'US',
    month = strftime(date,'%m-%y')
  )%>%
  dplyr::filter(!state.region%in%'Territory')%>%
  foo_roll(by = c(facet_by,path_by))


dat_labs <- dat%>%
  foo_labs(by = c(facet_by,path_by,'month'),
           scale = 100,
           highlight = highlight,
           highlight_var = 'county',metric = metric)

# ps <- purrr::map(mf,p,dat = dat,metric = metric,facet_by = facet_by,highlight = highlight,dat_labs = dat_labs,highlight_var = 'state.abb')
# ps[[2]]
# purrr::reduce(ps,`|`) + plot_layout(widths = c(1,2))

dat2 <- dat%>%
  # dplyr::group_by(county)%>%
  # dplyr::filter(date>=max(date)-28)%>%
  # dplyr::ungroup()%>%
  dplyr::filter(month%in%c('05-20','06-20'))%>%
  dplyr::select(date,state.region,fips,state.abb,county,!!rlang::sym(metric),!!rlang::sym(glue::glue('{metric}_new_c')))

dat3 <- dat2%>%
  dplyr::group_by(county)%>%
  dplyr::slice(1)%>%
  dplyr::ungroup()%>%
  dplyr::rename_at(dplyr::vars(dplyr::starts_with(metric)),
                   .funs = function(x){sprintf('%s1',x)})%>%
  dplyr::select(-date)

#dplyr::vars(cases,cases1,cases_new_c,cases_new_c1)

dat4 <- dat2%>%
  dplyr::left_join(dat3)%>%
  dplyr::mutate_at(dplyr::vars(!!rlang::sym(metric),
                               !!rlang::sym(glue::glue('{metric}1')),
                               !!rlang::sym(glue::glue('{metric}_new_c')),
                               !!rlang::sym(glue::glue('{metric}_new_c1'))),
                   list(function(x) x+1))%>%
  dplyr::mutate(
    !!rlang::sym(metric) := !!rlang::sym(metric)/!!rlang::sym(glue::glue('{metric}1')),
    !!rlang::sym(glue::glue('{metric}_new_c')) := !!rlang::sym(glue::glue('{metric}_new_c'))/!!rlang::sym(glue::glue('{metric}_new_c1')),
  )%>%
  dplyr::filter(state.abb%in%c('WA'))%>%
  dplyr::filter(!county%in%'Unknown')%>%
  # dplyr::left_join(flc_df,by='county')%>%
  identity()

dat_labs2 <- dat4%>%
  dplyr::group_by(state.abb,county)%>%
  slice(n())%>%
  dplyr::ungroup()

dat4%>%
  ggplot(aes(
    x = !!rlang::sym(glue::glue('{metric}')),
    y = !!rlang::sym(glue::glue('{metric}_new_c')))) +
  geom_ribbon(data = dat4%>%
                dplyr::filter(!!rlang::sym(glue::glue('{metric}'))>0)
              ,aes(x = !!rlang::sym(glue::glue('{metric}')),ymax = 1,ymin = 0.05),alpha = 0.3,fill = 'red') +
  geom_abline(intercept=0,slope=1,linetype = 2) +
  geom_path(na.rm = TRUE,aes(group = !!rlang::sym(path_by)),alpha = 0.25) +
  geom_path(data = dat4%>%dplyr::filter(county%in%highlight),
            na.rm = TRUE,aes(group = !!rlang::sym(path_by)),
            colour = 'red') +
  geom_point(data = dat_labs2,na.rm = TRUE) +
  ggrepel::geom_label_repel(aes(label = !!rlang::sym(path_by)),
                            data = dat_labs2,
                            segment.color = 'grey90',
                            #nudge_x = 2,
                            size = 2,
                            na.rm = TRUE) +
  facet_wrap(~state.abb) +
  scale_y_log10()+
  scale_x_log10() +
  labs(
    y = glue::glue('New {capfirst(metric)} Rate of Growth\n({attr(dat,"window")} Day Rolling Sum)'),
    x = glue::glue('Total {capfirst(metric)} Rate of Growth')
  ) +
  theme_minimal()

region <- c('Northeast')
state_id <- c('AZ')

fips <- state_input%>%
  dplyr::filter(state.abb%in%state_id)%>%
  # dplyr::filter(state.region%in%region)%>%
  dplyr::select(state,state.region,fips)%>%
  dplyr::distinct()%>%
  pull(fips)

dat2%>%
  left_join(counties, by = c('fips'="county_fips"))%>%
  left_join(countydata%>%dplyr::select(county_fips,hhpop), by = c('fips'="county_fips"))%>%
  dplyr::filter(state.abb%in%state_id)%>%
  # dplyr::filter(state.region%in%region)%>%
  dplyr::filter(date%in%(max(date) - seq(0,84,14)))%>%
  # dplyr::filter(date%in%(max(date)))%>%
  dplyr::mutate(
    cases_new_c = ifelse(cases_new_c<=0,1,cases_new_c),
    cases_new_c_pop = 1000*cases_new_c/hhpop,
    cases_new_c_log = log(cases_new_c_pop)
    ) -> dat_map

fips_mean <- counties%>%
  dplyr::group_by(county_fips,county_name)%>%
  dplyr::summarise_at(dplyr::vars(long,lat),list(mean))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(
    group = 1,
    county_name = gsub(' County','',county_name)
  )

dat_map%>%
  ggplot(mapping = aes(long, lat, group = group,fill = cases_new_c_log)) +
  geom_polygon(color = NA) +
  geom_polygon(data = states%>%
                 dplyr::filter(state_fips%in%fips),
               mapping = aes(long, lat, group = group),
               fill = NA, color = "#ffffff") +
  ggrepel::geom_label_repel(aes(label = county_name),
             size = 2,
             fill = NA,
             data = fips_mean%>%
               dplyr::filter(county_fips%in%dat_map$fips)%>%
               dplyr::mutate(
                 fips = substr(county_fips,4,5),
                 cases_new_c_log = NA_real_
                 ),
             ) +
  viridis::scale_fill_viridis(limits = c(-3,3)) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  facet_wrap(~date) +
  labs(fill = 'New Cases per 1000 (7 Day Cumulative)') +
  theme(legend.position = 'top')
