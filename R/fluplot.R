#' @title fluplot: Convenience wrapper for standard trend plot
#' 
#' @description This models wraps ggplot2 commands into a standard 
#' form for making cyclical trend line plots of influenza. Developed 
#' only for convenience, the authors encourage individuals to make thier
#' own customized graphs as appropriate.    
#' 
#' @param data A dataframe class object, must contain time variable, 
#' outcome variable  
#' 
#' @param xvar x-axis variable, must be date class  
#' 
#' @param yvar y-axis, outcome variable
#' 
#' @param baseline second line variable, fitted cyclical trend. Default="y0". 
#' 
#' @param threshold third line, upper threshold for cyclical trend. Default="y0_ul".    
#' 
#' @param linenames Defaults specified, but can override. Must be character 
#' vector of length=3.      
#'
#' @param ylab Character vector for Y-axis label
#' 
#' @param title Character vector for Plot Title  
#' 
#' @return an object of class gg, ggplot. 
#' 
#' @export
#' 
#' @examples
#' require(flumodelr)
#' fludta <- flumodelr::fludta
#' flu_fit <- serflm(fludta, outc = fludeaths, time = yrweek_dt)  
#'               
#' fluplot(flu_fit, xvar=yrweek_dt, yvar=fludeaths)
#' 
#' @import rlang ggplot2 
#' 
 
fluplot <- function(data=NULL, xvar=NULL, yvar=NULL, 
                    baseline=y0, threshold=y0_ul,
                    linenames=NULL, ylab="[OUTCOME]", title="[INSERT TITLE]") {
  #tidy evaluation
    xvar_eq <- enquo(xvar)
    yvar_eq <- enquo(yvar)
    bl_eq <- enquo(baseline)
    th_eq <- enquo(threshold)
  
  #Default names
    if (is.null(linenames)==T) def.names <- c("Observed", "Predicted", "Epidemic Threshold")
  
  #Add colour scale
    col_vals <- c("#CC0000", "black", "black")
    names(col_vals) = def.names
    
    linetype_vals <- c(1, 1, 2)
    names(linetype_vals) = def.names
    
  #define base graph
    g <- ggplot(data, aes_string(x=rlang::quo_text(xvar_eq)))  

    #Add lines
    g <- g + geom_line(aes_string(y= rlang::quo_text(yvar_eq),
                                  colour=shQuote(def.names[1]),
                                  linetype=shQuote(def.names[1])), size=0.8) +
      geom_line(aes_string(y= rlang::quo_text(bl_eq),
                           colour=shQuote(def.names[2]),
                           linetype=shQuote(def.names[2])), size=0.8) +
      geom_line(aes_string(y= rlang::quo_text(th_eq),
                           colour=shQuote(def.names[3]),
                           linetype=shQuote(def.names[3])), size=0.8) + 
      scale_colour_manual("Line", breaks=def.names, 
                          values = col_vals) +
      scale_linetype_manual("Line", breaks=def.names,
                            values = linetype_vals) +
      scale_x_date(date_breaks="1 year",
                   labels = scales::date_format("%Y"), 
                   expand=c(0, .9)) 
  
  #final touches
    g <- g + xlab("Year") + 
      ylab(ylab) + 
      theme_light(base_size=14) +
      theme(legend.text=element_text(size=10), 
            plot.title = element_text(size=14)) +
      labs(title=title,
           caption="Fitted Model + threshold for epidemic level") +
      guides(colour = guide_legend("Line"), linetype = guide_legend("Line"))
    
  return(g)
}  
  
  