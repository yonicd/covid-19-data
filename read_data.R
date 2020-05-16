county_input <- readr::read_csv('us-counties.csv')%>%
  tidyr::nest(data = -c(state,county))%>%
  dplyr::mutate(
    data = purrr::map(data,function(x){
      x%>%dplyr::mutate_at(dplyr::vars(cases,deaths), list(new = function(x) x - lag(x,1)))%>%dplyr::filter(date>min(date))
    })
  )%>%
  tidyr::unnest(data)%>%
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
  tidyr::nest(data = -c(state))%>%
  dplyr::mutate(
    data = purrr::map(data,function(x){
      x%>%dplyr::mutate_at(dplyr::vars(cases,deaths), list(new = function(x) x - lag(x,1)))%>%dplyr::filter(date>min(date))
    })
  )%>%
  tidyr::unnest(data)%>%
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
