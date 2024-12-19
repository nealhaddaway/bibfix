#' Plot RIS file health
#'
#' @param health_check result of bibfix::scan_file()
#' @param session if used in shiny the plot is a plotly version
#'
#' @export
plot_health <- function(health_check, session=NULL){
  
  
  health_check<-health_check[-1]
  reportdf <- as.data.frame(health_check)
  
  # Select relevant fields including n_retracted
  reportdf <- select(reportdf, -c('n_complete', 'n_findable_titles', 'n_total'))
  names(reportdf) <- str_to_sentence(gsub('_', ' ', gsub('n_', '', names(reportdf))))
  names(reportdf) <- gsub('Doi', 'DOI', names(reportdf))
  
  # Add n_retracted to the report
  reportdf <- data.frame(field = names(reportdf), 
                         value = t(reportdf),
                         row.names = NULL)
  
  # Include retracted in field levels
  reportdf$field <- factor(reportdf$field, 
                           levels = c('DOI', 'End page', 'Start page', 
                                      'Issue', 'Volume', 'Journal', 
                                      'Abstract', 'Title', 'Year', 
                                      'Author', 'Retracted'))
  
  # Create missing data summary
  reportdf$health <- 'healthy'
  new <- reportdf
  new$value <- health_check$n_total - reportdf$value
  new$health <- 'missing'
  reportdf <- rbind(reportdf, new)
  reportdf$health <- factor(reportdf$health, levels = c('missing', 'healthy'))
  
  # Plot setup
  geom.text.size <- 4
  theme.size <- 16
  
  plot <- ggplot2::ggplot(data = reportdf, 
                          ggplot2::aes(x = value, y = field, fill = health)) +
    ggplot2::geom_bar(stat = "identity", position = 'stack') +
    ggplot2::theme_minimal(base_size = theme.size) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      text = ggplot2::element_text(size = theme.size, color = "grey50"),
      axis.text = ggplot2::element_text(color = "grey50"),
      legend.position = "none"
    ) +
    ggplot2::xlim(0, 1.1 * max(reportdf$value)) +
    ggplot2::labs(x = "Health level of field") +
    ggplot2::scale_fill_manual(values = c('grey90', '#e43235'))
  if (shiny::isTruthy(session)) {
  # Convert to interactive plot
  plot <- plotly::ggplotly(plot, tooltip = "x")
  config(plot, displayModeBar = FALSE)
  
  }else{
    plot
   }
  
  return(plot)
}
