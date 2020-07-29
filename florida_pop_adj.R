library(magrittr)
library(dplyr)
library(ggplot2)

source('foos.R')

# pop data from here: http://edr.state.fl.us/Content/population-demographics/data/index-floridaproducts.cfm
fl_pop <- readr::read_csv('FloridaPopulation.csv')%>%
  dplyr::filter(Age_group!='Total')%>%
  dplyr::mutate(Age_group = case_when(
    Age_group%in%c('<1','1-4')~ '0-4',
    Age_group%in%c('5-9','10-14')~ '5-14',
    Age_group%in%c('15-19','20-24')~ '15-24',
    TRUE ~ Age_group
    )
  )%>%
  dplyr::group_by(county,Age_group)%>%
  dplyr::summarise_at(dplyr::vars(Population),list(sum))

fl_pop$Age_group <- factor(fl_pop$Age_group,levels = unique(fl_pop$Age_group)[c(1,6,2,3,4,5,7,8,9,10)])

fl_covid <- readr::read_csv('https://opendata.arcgis.com/datasets/37abda537d17458bae6677b8ab75fcb9_0.csv')

flc <- xml2::read_html('http://www.floridacountiesmap.com/counties_list.shtml')
flct <- rvest::html_table(flc)

flc_df <- flct[[1]]%>%
  dplyr::rename('region' = 'X1','county_str' = 'X2')%>%
  dplyr::mutate(
    county = sapply(strsplit(county_str,'[0-9]\\.'),function(x){
      ret <- gsub('(^\\s)|( - (.*?)$)','',x)
      ret <- ret[nzchar(ret)]
    })
  )%>%
  dplyr::select(-county_str)%>%
  tidyr::unnest(county)%>%
  dplyr::mutate(
    county = ifelse(county=='St Johns','Saint Johns',county),
    county = ifelse(county=='St. Lucie','Saint Lucie',county),
    county = ifelse(county=='DeSoto','Desoto',county)
  )%>%
  dplyr::left_join(fl_pop)%>%
  dplyr::mutate(
    county = case_when(
      county=='Miami-Dade' ~ 'Dade',
      county=='Saint Johns' ~ 'St. Johns',
      county=='Saint Lucie' ~ 'St. Lucie',
      TRUE ~ county)
  )


flc_df$region <- factor(flc_df$region,unique(flc_df$region)[c(6,4,5,3,1,2,8,7)])

fl_data <- fl_covid%>%
  dplyr::filter(Age_group!='Unknown')%>%
  dplyr::filter(!County%in%c('Unknown','State-Call Center'))%>%
  dplyr::mutate(date = as.Date(Case1),
                case = ifelse(Case_=='Yes',1,0),
                death = ifelse(is.na(Died),0,1),
                hosp = ifelse(Hospitalized=='YES',1,0)
                )%>%
  dplyr::group_by(County,Age_group,date)%>%
  dplyr::summarise(
    cases_new = sum(case),
    deaths_new = sum(death),
    hosp_new = sum(hosp)
  )%>%
  dplyr::ungroup()

dat_dates <- seq.Date(from = min(fl_data$date),to = max(fl_data$date),by='day')

fl_data_fill <- fl_data%>%
  dplyr::select(County,Age_group)%>%dplyr::distinct()%>%
  dplyr::mutate(date = list(dat_dates))%>%
  tidyr::unnest(date)%>%
  dplyr::left_join(fl_data,by=c('County','Age_group','date'))%>%
  dplyr::mutate_at(dplyr::vars(cases_new,deaths_new,hosp_new),list(function(x) ifelse(is.na(x),0,x)))

fl_data_fill$Age_group <- gsub(' years','',fl_data_fill$Age_group)
fl_data_fill$Age_group <- factor(fl_data_fill$Age_group,levels = unique(fl_data_fill$Age_group)[c(1,6,2,3,4,5,7,8,9,10)])

facet_by <- 'County'
path_by <- 'Age_group'
metric <- 'cases'

dat <- fl_data_fill%>%
  dplyr::mutate(
    state = 'FL',
    month = strftime(date,'%m-%y')
  )%>%
  foo_roll(by = c(facet_by,path_by),vars = c('cases_new','deaths_new','hosp_new'))%>%
  dplyr::filter(!is.na(cases_new_c))%>%
  dplyr::left_join(flc_df,by=c('County'='county','Age_group'))

dat <- dat%>%
  dplyr::mutate(
    cases_pop = 1000 * cases_new_c/Population,
    deaths_pop = 1000 * deaths_new_c/Population,
    hosp_pop = 1000 * hosp_new_c/Population,
    )%>%
  dplyr::arrange(region,County)

dat_state <- dat%>%
  group_by(date,Age_group)%>%
  dplyr::summarise_at(
    dplyr::vars(dplyr::ends_with('new_c'),
                dplyr::ends_with('pop')),
    list(sum))%>%
  dplyr::ungroup()


dat_state%>%
  ggplot(aes(x=date,y=Age_group)) +
  geom_tile(aes(fill=log(cases_pop + .0001)),show.legend = FALSE) +
  geom_text(data = dat_state%>%dplyr::filter(date==max(date)),
            aes(label = round(hosp_pop,1)),
            nudge_x = 6,size=3) +
  viridis::scale_fill_viridis() +
theme(
  legend.position = 'top',
  axis.text.y = element_text(size=rel(0.75))
) +
  labs(
    title = '7 Day rolling Sum of New Hospitalizations per 1000 indiv by Age Group, Date',
    subtitle = 'County Level Population',
    y = 'Age group',
    x = 'Date'
  )

dat%>%
  group_by(date,Age_group,region)%>%
  dplyr::summarise_at(dplyr::vars(dplyr::ends_with('new_c'),dplyr::ends_with('pop')),list(sum))%>%
  ggplot(aes(x=date,y=Age_group,fill=log(cases_pop + .0001))) +
  geom_tile() +
  viridis::scale_fill_viridis() +
  facet_wrap(~region) +
  theme(legend.position = 'top') +
  labs(
    title = '7 Day rolling Sum of New Hospitalizations per 1000 indiv by Age Group, Date and County Region',
    subtitle = 'County Level Population',
    fill = 'New Hospitalizations (log)',
    y = 'Age group',
    x = 'Date'
  )

dat%>%
  tidyr::unite(col = 'county',region,County,sep=':')%>%
  dplyr::mutate(county = factor(county,unique(county)))%>%
  # group_by(date,Age_group)%>%
  # dplyr::summarise_at(dplyr::vars(dplyr::ends_with('new_c')),list(sum))%>%
  ggplot(aes(x=date,y=Age_group,fill=log(cases_pop + .001))) +
  geom_tile() +
  viridis::scale_fill_viridis() +
  facet_wrap(~county) +
  theme(legend.position = 'top',axis.text.y = element_text(size=rel(0.5))) +
  labs(
    title = '7 Day rolling Sum of New Hospitalizations by Age Group, Date and County',
    subtitle = 'County Level Population',
    fill = 'New Hospitalizations (log)',
    y = 'Age group',
    x = 'Date'
  )


p <- dat%>%
  # group_by(date,region)%>%
  # dplyr::summarise_at(dplyr::vars(dplyr::ends_with('new_c'),dplyr::ends_with('pop')),list(sum))%>%
  dplyr::filter(date>=as.Date('2020-06-01')&grepl('South',region))%>%
  ggplot(aes(y = hosp_pop,x = cases_pop)) +
  geom_point(aes(colour = County)) +
  #ggrepel::geom_label_repel(aes(label = County)) +
  facet_wrap(~Age_group) +
  gganimate::transition_time(date) +
  gganimate::ease_aes('linear') +
  labs(
    x = 'New Cases per 1000 persons',
    y = 'New Hospitalizations per 1000 persons',
    title = 'Florida New Cases vs New Hospitalizations per 1000 persons 06/2020-07/2020',
    subtitle = 'Date:{frame_time}'
    )

gganimate::anim_save('FL_anim.gif',animation = p,fps = 5)
