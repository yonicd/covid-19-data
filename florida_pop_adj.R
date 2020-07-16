library(magrittr)
library(dplyr)
library(ggplot2)

source('foos.R')

# pop data from here: http://edr.state.fl.us/Content/population-demographics/data/index-floridaproducts.cfm
fl_pop <- readr::read_csv('fl_pop_2020.csv')%>%
  dplyr::mutate(county = gsub(' County','',county))

fl_covid <- readr::read_csv('https://opendata.arcgis.com/datasets/37abda537d17458bae6677b8ab75fcb9_0.csv')

flc_df <- flct[[1]]
names(flc_df) <- c('region','county_str')

flc_df <- flc_df%>%
  dplyr::mutate(
    county = sapply(strsplit(county_str,'[0-9]\\.'),function(x){
      ret <- gsub('(^\\s)|( - (.*?)$)','',x)
      ret <- ret[nzchar(ret)]
    })
  )%>%
  dplyr::select(-county_str)%>%
  tidyr::unnest(county)%>%
  dplyr::mutate(
    county = ifelse(county=='St Johns','St. Johns',county)
  )%>%
  dplyr::left_join(fl_pop)%>%
  dplyr::mutate(
    county = ifelse(county=='Miami-Dade','Dade',county),
    county = ifelse(county=='DeSoto','Desoto',county)
  )


flc_df$region <- factor(flc_df$region,unique(flc_df$region)[c(6,4,5,3,1,2,8,7)])

flc_df%>%dplyr::left_join(fl_pop)

fl_data <- fl_covid%>%
  dplyr::mutate(date = as.Date(Case1),
                case = ifelse(Case_=='Yes',1,0),
                death = ifelse(is.na(Died),0,1))%>%
  dplyr::group_by(County,Age_group,date)%>%
  dplyr::summarise(
    cases_new = sum(case),
    deaths_new = sum(death)
  )%>%
  dplyr::ungroup()

dat_dates <- seq.Date(from = min(fl_data$date),to = max(fl_data$date),by='day')

fl_data_fill <- fl_data%>%
  dplyr::select(County,Age_group)%>%dplyr::distinct()%>%
  dplyr::mutate(date = list(dat_dates))%>%
  tidyr::unnest(date)%>%
  dplyr::left_join(fl_data,by=c('County','Age_group','date'))%>%
  dplyr::mutate_at(dplyr::vars(cases_new,deaths_new),list(function(x) ifelse(is.na(x),0,x)))

fl_data_fill$Age_group <- gsub(' years','',fl_data_fill$Age_group)
fl_data_fill$Age_group <- factor(fl_data_fill$Age_group,levels = unique(fl_data_fill$Age_group)[c(1,6,2,3,4,5,7,8,9,10,11)])

facet_by <- 'County'
path_by <- 'Age_group'
metric <- 'cases'

dat <- fl_data_fill%>%
  dplyr::mutate(
    state = 'FL',
    month = strftime(date,'%m-%y')
  )%>%
  foo_roll(by = c(facet_by,path_by))%>%
  dplyr::filter(Age_group!='Unknown'&!is.na(cases_new_c))%>%
  dplyr::filter(County!='Unknown')

dat <- dat%>%
  dplyr::left_join(flc_df,by=c('County'='county'))%>%
  dplyr::mutate(cases_pop = 1000*cases_new_c/population)%>%
  dplyr::arrange(region,County)

dat%>%
  group_by(date,Age_group)%>%
  dplyr::summarise_at(dplyr::vars(dplyr::ends_with('new_c'),cases_pop),list(sum))%>%
  ggplot(aes(x=date,y=Age_group,fill=log(cases_pop + .01))) +
  geom_tile() +
  viridis::scale_fill_viridis() +
theme(
  legend.position = 'top',
  axis.text.y = element_text(size=rel(0.75))
) +
  labs(
    title = '7 Day rolling Sum of New Cases per 1000 indiv by Age Group, Date',
    subtitle = 'County Level Population',
    fill = 'New Cases (log)',
    y = 'Age group',
    x = 'Date'
  )

dat%>%
  group_by(date,Age_group,region)%>%
  dplyr::summarise_at(dplyr::vars(dplyr::ends_with('new_c'),cases_pop),list(sum))%>%
  ggplot(aes(x=date,y=Age_group,fill=log(cases_pop + .01))) +
  geom_tile() +
  viridis::scale_fill_viridis() +
  facet_wrap(~region) +
  theme(legend.position = 'top') +
  labs(
    title = '7 Day rolling Sum of New Cases per 1000 indiv by Age Group, Date and County Region',
    subtitle = 'County Level Population',
    fill = 'New Cases (log)',
    y = 'Age group',
    x = 'Date'
  )

dat%>%
  tidyr::unite(col = 'county',region,County,sep=':')%>%
  dplyr::mutate(county = factor(county,unique(county)))%>%
  # group_by(date,Age_group)%>%
  # dplyr::summarise_at(dplyr::vars(dplyr::ends_with('new_c')),list(sum))%>%
  ggplot(aes(x=date,y=Age_group,fill=log(cases_pop + .01))) +
  geom_tile() +
  viridis::scale_fill_viridis() +
  facet_wrap(~county) +
  theme(legend.position = 'top',axis.text.y = element_text(size=rel(0.5))) +
  labs(
    title = '7 Day rolling Sum of New Cases per 1000 indiv by Age Group, Date and County',
    subtitle = 'County Level Population',
    fill = 'New Cases (log)',
    y = 'Age group',
    x = 'Date'
  )

