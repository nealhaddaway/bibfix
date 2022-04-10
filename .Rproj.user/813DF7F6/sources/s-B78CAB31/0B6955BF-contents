#' Plot RIS file health
#' remotes::install_git("https://git.rud.is/hrbrmstr/ggchicklet.git")
#' report <- scan_file(refs)
#' health_plot <- plot_health(report)
#' health_plot
plot_health <- function(health_check){
  
  reportdf <- as.data.frame(health_check)
  reportdf <- select(reportdf, -c('n_complete', 'n_findable_titles', 'n_total'))
  names(reportdf) <- str_to_sentence(gsub('_', ' ', gsub('n_', '', names(reportdf))))
  names(reportdf) <- gsub('Doi', 'DOI', names(reportdf))
  reportdf <- data.frame(field = names(reportdf), 
                         value = t(reportdf),
                         row.names = NULL)
  reportdf$health <- 'healthy'
  reportdf$field <- factor(reportdf$field, levels = c('DOI', 'End page', 'Start page', 'Issue', 'Volume', 'Journal', 'Abstract', 'Title', 'Year', 'Author'))
  new <- reportdf
  new$value <- health_check$n_total - reportdf$value
  new$health <- 'missing'
  reportdf <- rbind(reportdf, new)
  reportdf$health <- factor(reportdf$health, levels = c('missing', 'healthy'))
  
  geom.text.size <- 4
  theme.size <- 16
  
  plot <- ggplot2::ggplot(data=reportdf, ggplot2::aes(x=value, y=field, fill=health)) +
    ggplot2::geom_bar(stat="identity", position = 'stack') +
    #geom_text(aes(label=value), position = position_dodge(width=0.9), hjust=-0.5, size=geom.text.size, color='grey50') + 
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank(),
          text = ggplot2::element_text(size = theme.size, color="grey50"),
          axis.text = ggplot2::element_text(color="grey50")) +
    ggplot2::xlim(0, 1.1*max(reportdf$value)) +
    ggplot2::labs(x = "Health level of field") +
    ggplot2::scale_fill_manual(values=c('grey90', '#e43235')) + 
    ggplot2::theme(legend.position = "none")
  
  plot <- plotly::ggplotly(plot, tooltip="x")
  config(plot, displayModeBar = FALSE)
  
  return(plot)
  
}
