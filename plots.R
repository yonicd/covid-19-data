library(dplyr)
library(ggplot2)
library(patchwork)
library(gganimate)

capfirst <- function(x) gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", x, perl=TRUE)

county_input <- readr::read_csv('us-counties.csv')%>%
  dplyr::left_join(tibble::tibble(
    state.abb = state.abb,
    state = state.name,state.region = state.region),
    by = "state")%>%
  dplyr::mutate(
    state.region = as.character(state.region),
    state.region = ifelse(grepl('^District',state),'South',state.region),
    state.abb = ifelse(is.na(state.region),state,state.abb),
    state.region = ifelse(is.na(state.region),'Territory',state.region),
  )%>%
  dplyr::arrange(county,date)

state_input <- readr::read_csv('us-states.csv')%>%
  dplyr::left_join(tibble::tibble(
    state.abb = state.abb,
    state = state.name,state.region = state.region),
    by = "state")%>%
  dplyr::mutate(
    state.region = as.character(state.region),
    state.region = ifelse(grepl('^District',state),'South',state.region),
    state.abb = ifelse(is.na(state.region),state,state.abb),
    state.region = ifelse(is.na(state.region),'Territory',state.region),
    )%>%
  dplyr::arrange(state.abb,date)


foo_roll <- function(dat,by,window=7){

  dat%>%
    dplyr::group_by(!!!rlang::syms(by))%>%
    dplyr::mutate_at(vars(cases,deaths),list(c=RcppRoll::roll_sumr),n=window)%>%
    dplyr::ungroup()
}

foo_labs <- function(dat,by,scale = 100){

  dat%>%
    dplyr::group_by(!!!rlang::syms(by))%>%
    slice(n())%>%
    dplyr::ungroup()%>%
    dplyr::filter(cases>max(cases,na.rm = TRUE)/scale)
}

foo_plot <- function(dat,metric = 'cases', label_by,path_by,facet_by = NULL,lab_scale = 100){

  dat_labs <- dat%>%
    foo_labs(by = c(facet_by,label_by),scale = lab_scale)

  p <- dat%>%
    ggplot(aes(x=!!rlang::sym(glue::glue('{metric}_c')),
               y=!!rlang::sym(glue::glue('{metric}')))) +
    geom_abline(intercept=0,slope=1)+
    geom_path(aes(group=!!rlang::sym(path_by),colour=as.numeric(date)),
              na.rm = TRUE,show.legend = FALSE) +
    geom_point(data = dat_labs,na.rm = TRUE) +
    ggrepel::geom_label_repel(aes(label = !!rlang::sym(path_by)),
                              data = dat_labs,
                              nudge_x = 10,
                              segment.color = 'grey80',
                              size = 2,
                              na.rm = TRUE) +
    scale_y_log10()+
    scale_x_log10() +
    labs(y = glue::glue('New {capfirst(metric)}\n(7 Day Rolling Sum)'),
         x = glue::glue('Total {capfirst(metric)}')
    ) +
    viridis::scale_colour_viridis(direction = -1) +
    theme(axis.text = element_text(size=rel(0.5)))

  if(!is.null(facet_by))
    p <- p + facet_wrap(as.formula(glue::glue('~{facet_by}')))

  p

}

# state plots
purrr::walk(c('deaths','cases'),.f=function(x,data){
    p <- data%>%foo_plot(metric = x,label_by = 'state',path_by = 'state.abb',facet_by = 'state.region')
    ggsave(filename = glue::glue('state_{x}.png'),p,width=7,height=7)
  },data =state_input%>%foo_roll('state'))

# region plots

purrr::walk(c('deaths','cases'),.f=function(x,data){
  p <- data%>%
    foo_plot(metric = x,'state.region','state.region')
  ggsave(filename = glue::glue('region_{x}.png'),p,width=7,height=7)
},data = state_input%>%
  dplyr::group_by(state.region,date)%>%
  dplyr::summarise_at(vars(cases,deaths),list(sum))%>%
  foo_roll('state.region'))

# county plots
p_county <- county_input%>%
  foo_roll('county')%>%
  split(.$state.region)%>%
  purrr::map(
    foo_plot,
    metric = 'deaths',
    label_by = 'county',
    path_by = 'county',
    facet_by = 'state',
    lab_scale = 50
  )

purrr::iwalk(p_county,function(x,y) ggsave(file = glue::glue('county_{gsub("\\\\s","_",y)}_deaths.png'),x,width = 10,height = 10))

## animation

dat_labs <- state_input%>%
  foo_roll('state')%>%
  dplyr::group_by(date)%>%
  dplyr::filter(deaths>max(deaths,na.rm = TRUE)/50)%>%
  dplyr::ungroup()

p_anim <- state_input%>%
  foo_roll('state')%>%
  dplyr::filter(!is.na(deaths_c))%>%
  dplyr::filter(date>as.Date('2020-03-01'))%>%
  dplyr::mutate(state.abb = ifelse(is.na(state.abb),'DC',state.abb))%>%
  ggplot(aes(x=deaths_c,y=deaths)) +
  geom_abline(intercept=0,slope=1)+
  geom_path(aes(group=state.abb,colour=as.numeric(date)),show.legend = FALSE) +
  geom_point(data = dat_labs,na.rm = TRUE) +
  ggrepel::geom_label_repel(aes(label = state.abb),
                            data = dat_labs,
                            nudge_x = 10,
                            segment.color = 'grey80',
                            size = 2,
                            na.rm = TRUE) +
  scale_y_log10()+
  scale_x_log10() +
  labs(y = 'New Deaths\n(7 Day Rolling Sum)',
       x = 'Total Deaths',
       caption = 'Date:{frame_along}'
  ) +
  viridis::scale_colour_viridis(direction = -1) +
  theme(axis.text = element_text(size=rel(0.5))) +
  facet_wrap(~state.region) +
  gganimate::transition_reveal(date) +
  gganimate::ease_aes('circular-out')

magick::image_write(gganimate::animate(p_anim,fps=5), path="states_death.gif")
