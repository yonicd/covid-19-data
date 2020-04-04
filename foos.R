foo_roll <- function(dat,by,window=7){

  ret <- dat%>%
    dplyr::group_by(!!!rlang::syms(by))%>%
    dplyr::mutate_at(vars(cases,deaths),list(c=RcppRoll::roll_sumr),n=window)%>%
    dplyr::ungroup()

  attr(ret,'window') <- window

  ret
}

foo_labs <- function(dat,by,scale = 100){

  dat%>%
    dplyr::group_by(!!!rlang::syms(by))%>%
    slice(n())%>%
    dplyr::ungroup()%>%
    dplyr::filter(cases>max(cases,na.rm = TRUE)/scale)
}

foo_plot <- function(dat,metric = 'cases', label_by,path_by,facet_by = NULL,lab_scale = 100,add_labs = TRUE){

  dat_labs <- dat%>%
    foo_labs(by = c(facet_by,label_by),scale = lab_scale)

  p <- dat%>%
    ggplot(aes(x=!!rlang::sym(glue::glue('{metric}_c')),
               y=!!rlang::sym(glue::glue('{metric}')))) +
    geom_abline(intercept=0,slope=1)+
    geom_path(aes(group=!!rlang::sym(path_by),colour=as.numeric(date)),
              na.rm = TRUE,show.legend = FALSE) +
    geom_point(data = dat_labs,na.rm = TRUE) +
    scale_y_log10()+
    scale_x_log10() +
    labs(y = glue::glue('New {capfirst(metric)}\n({attr(dat,"window")} Day Rolling Sum)'),
         x = glue::glue('Total {capfirst(metric)}')
    ) +
    viridis::scale_colour_viridis(direction = -1) +
    theme(axis.text = element_text(size=rel(0.5)))

  if(!is.null(facet_by))
    p <- p + facet_wrap(as.formula(glue::glue('~{facet_by}')))

  if(add_labs)
   p <- p + ggrepel::geom_label_repel(aes(label = !!rlang::sym(path_by)),data = dat_labs,nudge_x = 10,
                                segment.color = 'grey80',
                                size = 2,
                                na.rm = TRUE)

  p

}

capfirst <- function(x) gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", x, perl=TRUE)
