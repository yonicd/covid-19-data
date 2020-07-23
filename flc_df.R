flc <- xml2::read_html('http://www.floridacountiesmap.com/counties_list.shtml')
flct <- rvest::html_table(flc)
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
    county = ifelse(county=='St Johns','St. Johns',county),
    county = ifelse(county=='Miami-Dade','Dade',county),
    county = ifelse(county=='DeSoto','Desoto',county)
  )


flc_df$region <- factor(flc_df$region,unique(flc_df$region)[c(6,4,5,3,1,2,8,7)])


