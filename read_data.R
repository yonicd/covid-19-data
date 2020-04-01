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
