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
#' @param x x-axis variable, must be date class  
#' 
#' @param y y-axis, outcome variable
#' 
#' @param y0 second line variable, fitted cyclical trend. Default="y0". 
#' 
#' @param y0_ul third line, upper threshold for cyclical trend. Default="y0_ul".    
#' 
#' @param linenames Defaults specified, but can override. Must be character 
#' vector of length=3.      
#'
#' @return an object of class gg
#' 
#' @export
#' 
#' @examples
#' require(flumodelr)
#' flu_ex <- flumodelr::flu_ex
#' flu_fit <- serflm(flu_ex, outc = "fludeaths", time = "yrweek_dt")  
#'               
#' fluplot(flu_fit, x="yrweek_dt", y="fludeaths")
#' 
 
fluplot <- function(data=NULL, xvar=NULL, yvar=NULL, y0="y0", y0_ul="y0_ul",
                    linenames=NULL, ylab="") {
  #Sanity checks
    stopifnot(is.null(data)==F)

    stopifnot(is.null(x)==F)
  
    stopifnot(is.null(y)==F)
  
  #define base graph
  g <- ggplot(data, x=UQ(as.name(xvar)))  
    
  #Default names
    if (is.null(linenames)==T) def.names <- c("Observed", "Predicted", "Epidemic Threshold")
  
  #Add lines
  g <- g + geom_line(aes(y=UQ(as.name(yvar)), colour=def.names[[1]], 
                  linetype=def.names[[1]]), size=0.8) +
            geom_line(aes(y=UQ(as.name(y0)), colour=def.names[[2]], 
                  linetype=def.names[[2]]), size=0.8) +
            geom_line(aes(y=UQ(as.name(y0_ul)), colour=def.names[[3]], 
                  linetype=def.names[[3]]), size=0.8)
  
  #Add colour scale
  g <- g + scale_colour_manual("Line",
                        breaks=c(def.names[[1]], 
                                 def.names[[2]], 
                                 def.names[[3]]),
                        values = c(def.names[[1]]="#CC0000", 
                                   def.names[[2]]="black", 
                                   def.names[[3]]="black"))
  
  #Add Line shape
  g <- g + scale_linetype_manual("Line", 
                                 breaks=c(def.names[[1]], 
                                          def.names[[2]], 
                                          def.names[[3]]),
                                 values = c(def.names[[1]]=1, 
                                            def.names[[2]]=2, 
                                            def.names[[3]]=3))
  
  #Add scale x date  
  g <- g + scale_x_date(labels = date_format("%Y"), 
                        date_breaks="1 year",
                        expand=c(0, .9)) 
  g <- g + xlab("Year") + 
    ylab("Deaths per 100,000") + 
    theme_light(base_size=14) +
    theme(legend.text=element_text(size=10), 
          plot.title = element_text(size=14)) +
    labs(title="Figure 4. Pneumonia and Influenza Deaths Over Time",
         caption="Serfling Model + Threshold for epidemic level") +
    guides(colour = guide_legend("Line"), linetype = guide_legend("Line"))
}

  