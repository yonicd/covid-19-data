system('git fetch upstream')
system("git merge upstream/master -m 'merge upstream'")

library(magrittr)
library(dplyr)
library(ggplot2)

source('read_data.R')
source('foos.R')

facet_by <- 'state.region'
path_by <- 'state.abb'
metric <- 'deaths'
mf <- c('06-20','07-20')
highlight <- c('MA','CT','FL')

dat <- state_input%>%
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
           highlight_var = 'state.abb',metric = metric)

dat2 <- dat%>%
  # dplyr::group_by(state.abb)%>%
  # dplyr::filter(date>=max(date)-14)%>%
  # dplyr::ungroup()%>%
  dplyr::filter(month%in%c('05-20','06-20'))%>%
  dplyr::select(date,state.region,state.abb,!!rlang::sym(metric),!!rlang::sym(glue::glue('{metric}_new_c')))

dat3 <- dat2%>%
  dplyr::group_by(state.abb)%>%
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
  #dplyr::filter(state.region%in%'South')%>%
  identity()

dat_labs2 <- dat4%>%
  dplyr::group_by(state.abb)%>%
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
  geom_path(data = dat4%>%dplyr::filter(state.abb%in%highlight),
            na.rm = TRUE,aes(group = !!rlang::sym(path_by)),
            colour = 'red') +
  geom_point(data = dat_labs2,na.rm = TRUE) +
  ggrepel::geom_label_repel(aes(label = !!rlang::sym(path_by)),
                            data = dat_labs2,
                            segment.color = 'grey90',
                            #nudge_x = 2,
                            size = 2,
                            na.rm = TRUE) +
  facet_wrap(~state.region) +
  scale_y_log10()+
  scale_x_log10() +
  labs(
    y = glue::glue('New {capfirst(metric)} Rate of Growth\n({attr(dat,"window")} Day Rolling Sum)'),
    x = glue::glue('Total {capfirst(metric)} Rate of Growth'),
    subtitle = paste0(range(dat4$date),collapse = ':')
  ) +
  theme_minimal()
