#___________________________________________________________________________________________________________________________
numeric_class_plot = function( var, xlim1 = -Inf ,  xlim2 = +Inf  ) {
  
  nc = which(colnames(USCensusData) == var)
  data = USCensusData[USCensusData[, nc] > xlim1 & USCensusData[, nc] < xlim2, ]
  
  p1 = ggplot(data, aes(data[, var], fill = y)) + geom_density(alpha = 0.2)+
    ggtitle(var) + xlab(var) +theme(plot.title = element_text(lineheight=2, face="bold"))
  p2 = ggplot(data, aes(data[, var], fill = y)) +
    geom_histogram(alpha = 0.4, aes(y = ..density..), bins = 30, position = 'identity')+
    ggtitle(var)+ xlab(var) +theme(plot.title = element_text(lineheight=2, face="bold"))
  print(multiplot(p1, p2,  cols=2))
  stat = list(describe(data[data$y == " - 50000.", var]),
              describe(data[data$y == " 50000+.", var]))
  return(stat)
}
#___________________________________________________________________________________________________________________________
factor_class_plot = function(var, remove_universe = TRUE) {
  if (!remove_universe) 
    data = USCensusData
  else{
    nc = which(colnames(USCensusData) == var)
    data = USCensusData[as.character(USCensusData[, nc]) != " Not in universe", ]
    
  }
  var = data[, var]
  p1 =qplot(factor(var), data=data, geom="bar", fill=factor(y)) +coord_flip() +
    ggtitle(var)+ xlab(var) +theme(plot.title = element_text(lineheight=2, face="bold"))
  print(p1)
  stat = list(describe(data[data$y == " - 50000.", var]),
              describe(data[data$y == " 50000+.", var]))
  return(stat)
}
#___________________________________________________________________________________________________________________________
maps = function(var){
  
  data = data.frame(region=as.character(tolower((USCensusData$state_of_previous_residence))), 
                    variable=(USCensusData[, var]), 
                    stringsAsFactors=F)
  data = data[data$region != " not in universe" & data$region != " ?", ]
  data$region = substring(data$region ,2, 1000)
  
  states_map <- map_data("state")
  print(ggplot(data, aes(map_id = region)) + 
          geom_map(aes(fill = variable), map = states_map) +
          scale_fill_gradientn(colours=c("blue","green","yellow","red")) + 
          expand_limits(x = states_map$long, y = states_map$lat))
  
}_____________________________________________________________________________

multiplot <-
  function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    require(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
      # Make the panel
      # ncol: Number of columns of plots
      # nrow: Number of rows needed, calculated from # of cols
      layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                       ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
      print(plots[[1]])
      
    } else {
      # Set up the page
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
      
      # Make each plot, in the correct location
      for (i in 1:numPlots) {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        
        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col))
      }
    }
  }

