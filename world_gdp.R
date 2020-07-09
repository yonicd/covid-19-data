source('world.R')
h <- xml2::read_html('https://www.worldometers.info/gdp/gdp-per-capita/')

ht <- h%>%
  rvest::html_table()%>%
  purrr::flatten_df()%>%
  tibble::as_tibble()%>%
  dplyr::select(-1)

names(ht) <- c('country','gdp_ppp','gdp_nom','gdp_comp')

gdp <- ht%>%
  dplyr::mutate_at(dplyr::vars(dplyr::contains('gdp')),
                   list(function(x) as.numeric(gsub('[$,%]','',x)))
  )

gdp_dat <- gdp%>%
  dplyr::left_join(
    world_roll_unit%>%
      dplyr::filter(date>=as.Date(date)-5)%>%
      # dplyr::filter(date==max(date))%>%
      dplyr::mutate(
        country = case_when(
          grepl('US',country) ~ 'United States',
          grepl('Korea, South',country) ~ 'South Korea',
          grepl('Czechia',country) ~ 'Czech Republic (Czechia)',
          grepl('Congo',country) ~ 'Congo',
          TRUE ~ country
        ),
        region1 = gsub('^(.*?)\\s','',region),
        region1 = ifelse(region1=='and New Zealand','Australia',region1)
      ),
    by='country')%>%
  dplyr::filter(!is.na(date))%>%
  dplyr::filter(region1!='Melanesia')%>%
  dplyr::mutate(date = as.Date(date))

# gdp_dat <-gdp_dat%>%
#   dplyr::filter(region1!='Africa')

gdp_dat_labs1 <- gdp_dat%>%
  dplyr::group_by(date)%>%
  dplyr::arrange(dplyr::desc(gdp_ppp))%>%
  dplyr::slice(1:10)%>%
  dplyr::ungroup()

gdp_dat_labs2 <- gdp_dat%>%
  dplyr::group_by(date)%>%
  dplyr::arrange(dplyr::desc(cases_new_c))%>%
  dplyr::slice(1:10)%>%
  dplyr::ungroup()

gdp_dat_labs3 <- gdp_dat%>%
  dplyr::filter(country%in%c('Israel','United States','Sweden'))

gdp_dat_labs <- dplyr::bind_rows(
  # gdp_dat_labs1,
  gdp_dat_labs2,
  gdp_dat_labs3)%>%
  dplyr::distinct()

library(gganimate)

StatChull <- ggproto("StatChull", Stat,
                     compute_group = function(data, scales) {
                       data[chull(data$x, data$y), , drop = FALSE]
                     },

                     required_aes = c("x", "y")
)

stat_chull <- function(mapping = NULL, data = NULL, geom = "polygon",
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatChull, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

p <- gdp_dat%>%
  ggplot(aes(x = gdp_ppp,y= cases_new_c)) +
  geom_point(aes(colour = region1),na.rm = TRUE) +
  geom_smooth(se= FALSE,na.rm = TRUE) +
  geom_hline(aes(yintercept = 1),linetype = 2) +
  ggrepel::geom_label_repel(data = gdp_dat_labs,
                            aes(label = country),size = 2) +
  scale_y_log10() +
  scale_x_log10() +
  stat_chull(aes(fill = region1),alpha=0.10,na.rm = TRUE) +
  labs(
    title = 'COVID New Cases Rate: {frame_time}',
    subtitle = 'Base Date: 2020-05-01',
    x = 'GDP per Capita (PPP)',
    y = 'New Cases Rate\n(7 Day Rolling Sum)',
    colour = 'Region',
    fill = 'Region'
  ) +
  theme_minimal() +
  transition_time(date) +
  ease_aes('linear')

gganimate::anim_save(filename = 'anim.gif',animation = p, fps=5)

