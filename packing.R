packing <- readr::read_csv('https://raw.githubusercontent.com/anthonydb/advanced-sql-nicar15/master/data/meat-poultry-inspect.csv')

zipcounty <- readr::read_csv('https://query.data.world/s/osj2ywp6fwklytrefp5qhxzxqdgebh')%>%
  dplyr::mutate(ZIP = as.character(ZIP))

#https://wonder.cdc.gov/wonder/sci_data/codes/fips/type_txt/cntyxref.asp

mypack <- packing%>%
  dplyr::mutate(Zip = gsub('-(.*?)$','',Zip))%>%
  dplyr::left_join(
    zipcounty%>%
      dplyr::select(Zip = ZIP,county = COUNTYNAME,state = STATE))%>%
  dplyr::filter(!is.na(county))

mypack%>%dplyr::left_join(
  county_input%>%dplyr::select(state = state.abb,county,fips)%>%dplyr::distinct())%>%View

read_fip <- function(path){

fips <- readr::read_lines(path)

fips_split <- fips%>%
  parallel::mclapply(function(x) {

  ret <- stringi::stri_sub(x,
                    c(0,6,16,18,20,22,24,26,29),
                    c(5,15,17,19,21,23,25,28,53))
  names(ret) <- c('zipcode','update_key','zip_sector_low','zip_segment_low',
                  'zip_sector_high','zip_segment_high','state.abb','fip',
                  'county')

  ret <- ret[c('zipcode','state.abb','fip','county')]

  ret <- as.data.frame(t(ret),stringsAsFactors = FALSE)

  ret$county <- gsub('\\s(.*?)$','',ret$county)
  ret
  },
  mc.cores = 7)

fips_split%>%
  dplyr::bind_rows()%>%
  tibble::as_tibble()%>%
  dplyr::distinct()

}

fipsa <- purrr::map_df(1:5,function(x) read_fip(sprintf('~/Desktop/zipctyA/zipcty%s',x)))
