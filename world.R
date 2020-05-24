source('foos.R')
library(furrr)
library(future)
library(magrittr)
library(dplyr)
library(ggplot2)
library(patchwork)

world <- jsonlite::read_json('https://pomber.github.io/covid19/timeseries.json')
options(future.fork.enable = TRUE)
future::plan(future::multicore,workers = future::availableCores() - 1)

world_dat <- world%>%
  furrr::future_map_dfr(.f=function(country){
    ret <- country%>%
      purrr::map_df(.f=function(x){
      x%>%
        purrr::transpose()%>%
        purrr::flatten()%>%
        tibble::as_tibble()
    })%>%
      dplyr::rename(
        cases = confirmed
      )

    ret%>%
      dplyr::mutate_at(dplyr::vars(cases,deaths),
                       list(new = function(x) x - lag(x,1)))%>%dplyr::filter(date>min(date))

  },.id = 'country')

mf <- c('04-20','02-20','03-20')
facet_by <- 'region'
path_by <- 'country'
metric <- 'cases'
h_c <- c('Israel','US','Spain','Italy','China','Germany','Canada','Korea, South','France','Japan','Iran','Turkey','Sweden')

rm_c <- c('Diamond Princess', 'Kosovo', 'MS Zaandam')

world_roll <- world_dat%>%
  dplyr::filter(!country%in%rm_c)%>%
  dplyr::mutate(
    region =
      countrycode::countrycode(sourcevar = country, origin = "country.name",destination = "region"),
    month = strftime(date,'%m-%y')
  )%>%
  foo_roll('country')%>%
  dplyr::filter(month%in%mf)

world_roll_first <- world_roll%>%
  dplyr::group_by(country)%>%
  dplyr::slice(1)%>%
  dplyr::select(country,cases_new_c1 = cases_new_c,cases1 = cases,
                deaths_new_c1 = deaths_new_c,deaths1 = cases)

world_roll_unit <- world_roll%>%
  dplyr::left_join(world_roll_first)%>%
  dplyr::mutate_at(dplyr::vars(cases,cases1,cases_new_c,cases_new_c1,
                               deaths,deaths1,deaths_new_c,deaths_new_c1),list(function(x) x+1))%>%
  dplyr::mutate(
    cases = cases/cases1,
    cases_new_c = cases_new_c/cases_new_c1,
    deaths = deaths/deaths1,
    deaths_new_c = deaths_new_c/deaths_new_c1,
  )

world_labs <- world_roll%>%
  foo_labs(by = c(facet_by,path_by,'month'),scale = 100,highlight = h_c,
           highlight_var = 'country',metric = 'cases')

world_labs_unit <- world_roll_unit%>%
  foo_labs(by = c(facet_by,path_by,'month'),scale = 1,highlight = h_c,
           highlight_var = 'country',metric='deaths')

# ps <- purrr::map(mf,p,dat = world_roll,metric = metric,facet_by = facet_by,highlight = highlight,highlight_var = 'country',dat_labs = world_labs)
#
# p(dat = world_roll,mf = mf,metric = metric,facet_by = facet_by,highlight = highlight,highlight_var = 'country',dat_labs = world_labs)
#
# purrr::reduce(ps,`+`) + plot_layout(design = 'BCA')


p_colour <- function(dat,mf,metric,facet_by,highlight,highlight_var = 'county',dat_labs){
  dat%>%
    dplyr::filter(month%in%mf)%>%
    ggplot(aes(
      x = !!rlang::sym(glue::glue('{metric}')),
      y = !!rlang::sym(glue::glue('{metric}_new_c')))) +
    facet_wrap(~region) +
    geom_abline(intercept=0,slope=1,linetype = 2) +
    geom_path(na.rm = TRUE,aes(group = !!rlang::sym(path_by),colour = month)) +
    geom_point(data = dat_labs%>%dplyr::filter(month%in%mf),na.rm = TRUE) +
    scale_y_log10()+
    scale_x_log10() +
    ggrepel::geom_label_repel(aes(label = !!rlang::sym(path_by)),
                              data = dat_labs%>%dplyr::filter(month%in%mf)%>%dplyr::distinct(),
                              nudge_x = 3,
                              segment.color = 'grey80',
                              size = 2,
                              na.rm = TRUE) +
    labs(
      colour = 'Month',
      y = glue::glue('New {capfirst(metric)}\n({attr(dat,"window")} Day Rolling Sum)'),
      x = glue::glue('Total {capfirst(metric)}')
    ) +
    viridis::scale_colour_viridis(direction = -1,option = 'A',discrete = TRUE) +
    theme_minimal() +
    theme(axis.text = element_text(size=rel(0.5)),legend.position = 'top')
}

regs <- c('Northern America','Northern Europe','Western Europe')

# regs <- unique(grep('Asia',world_roll$region,value = TRUE))

world_labs_cases <- world_roll%>%
  foo_labs(by = c(facet_by,path_by,'month'),scale = 100,highlight = h_c,highlight_var = 'country',metric = 'cases')

p_cases <- p_colour(dat = world_roll%>%dplyr::filter(region%in%regs),mf = mf,metric = 'cases',facet_by = facet_by,highlight = highlight,highlight_var = 'country',dat_labs = world_labs_cases%>%dplyr::filter(month=='04-20')%>%dplyr::filter(region%in%regs))

world_labs_deaths <- world_roll%>%
  foo_labs(by = c(facet_by,path_by,'month'),scale = 100,highlight = h_c,
           highlight_var = 'country',metric = 'deaths')

p_deaths <- p_colour(dat = world_roll%>%dplyr::filter(region%in%regs),mf = mf,metric = 'deaths',facet_by = facet_by,highlight = highlight,highlight_var = 'country',dat_labs = world_labs_deaths%>%dplyr::filter(month=='04-20')%>%dplyr::filter(region%in%regs))

p_cases + geom_hline(aes(yintercept = cases_new_c),data = world_labs_cases%>%dplyr::filter(month=='04-20')%>%dplyr::filter(country%in%"Sweden")%>%dplyr::select(-region),linetype = 1) / p_deaths

world_labs_cases_unit <- world_roll_unit%>%
  foo_labs(by = c(facet_by,path_by,'month'),scale = 100,highlight = h_c,highlight_var = 'country',metric = 'cases')

p_cases_unit <- p_colour(dat = world_roll_unit%>%dplyr::filter(region%in%'Northern Europe'),mf = mf,metric = 'cases',facet_by = facet_by,highlight = highlight,highlight_var = 'country',dat_labs = world_labs_cases_unit%>%dplyr::filter(month=='04-20')%>%dplyr::filter(region%in%'Northern Europe'))

world_labs_deaths_unit <- world_roll_unit%>%
  foo_labs(by = c(facet_by,path_by,'month'),scale = 100,highlight = h_c,highlight_var = 'country',metric = 'deaths')

p_deaths_unit <- p_colour(dat = world_roll_unit%>%dplyr::filter(region%in%'Northern Europe'),mf = mf,metric = 'deaths',facet_by = facet_by,highlight = highlight,highlight_var = 'country',dat_labs = world_labs_deaths_unit%>%dplyr::filter(month=='04-20')%>%dplyr::filter(region%in%'Northern Europe'))

p_cases_unit / p_deaths_unit
