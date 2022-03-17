#' Plot RIS file health
#' remotes::install_git("https://git.rud.is/hrbrmstr/ggchicklet.git")
#' health_check <- scan_file(refs)
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
  
  #correct text size in ratio 14/5: https://stackoverflow.com/questions/25061822/ggplot-geom-text-font-size-control
  geom.text.size <- 5
  theme.size <- 20
  
  plot <- ggplot(data=reportdf, aes(x=value, y=field, fill=health)) +
    geom_bar(stat="identity", position = 'stack') +
    #geom_text(aes(label=value), position = position_dodge(width=0.9), hjust=-0.5, size=geom.text.size, color='grey50') + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y = element_blank(),
          text = element_text(size = theme.size, color="grey50"),
          axis.text = element_text(color="grey50")) +
    xlim(0, 1.1*max(reportdf$value)) +
    labs(x = "Health level of field") +
    scale_fill_manual(values=c('grey90', '#e43235')) + 
    theme(legend.position = "none")
  
  return(plot)
  
}
