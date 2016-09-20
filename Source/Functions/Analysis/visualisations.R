
getEventsForPlot <- function(t, markers, groups)
{
  df <- data.frame(markers = markers, t = t, groups = groups)
  dfs <- split(df, df$groups)
  locations <- lapply(dfs, FUN = function(x) 
  {
    list(start = head(x$t, 1), 
         end = tail(x$t, 1),
         event = unique(x$markers))
  })
  locations <- rbindlist(locations)
  return(locations)
}

plotChannel <- function(channels, events, line.size = 1, 
                        xlab = "Time, sec", ylab = "Channel value",
                        xlim = c(min(channels$t), max(channels$t)), 
                        ylim = c(min(channels$value), max(channels$value)), 
                        title = "Events, channel", 
                        events.title = "Events", 
                        channels.title = "Channels") 
{
  #Data preparation
  ###Events
  starts <- head(which(events$start > xlim[1]), 1)
  ends <- tail(which(events$end < xlim[2]), 1)
  new.events <- rbind(data.frame(start = xlim[1], 
                                 end = events$end[ifelse(starts > 1, starts - 1, 1)], 
                                 event = events$event[ifelse(starts > 1, starts - 1, 1)]),
                      events[starts:ends,],
                      data.frame(start = events$start[ifelse(ends < nrow(events), ends + 1, nrow(events))],
                                 end = xlim[2],
                                 event = events$event[ifelse(ends < nrow(events), ends + 1, nrow(events))]))
  events <- new.events
  ###Channels
  channels <- channels[which(channels$t >= xlim[1] & channels$t <= xlim[2]),]
  channels <- channels[which(channels$value >= ylim[1] & channels$value <= ylim[2]),]
  #Plotting base
  p <- ggplot() + 
    scale_x_continuous(name = xlab, limits = xlim) + 
    scale_y_continuous(name = ylab, limits = ylim) +
    ggtitle(title) + labs(col = channels.title, fill = events.title)
  #Plotting
  p <- p + geom_line(data = channels, aes(x = t, y = value, col = name), size = line.size) +
    geom_rect(data = events, aes(xmin = start, 
                                 xmax = end, 
                                 fill = event, 
                                 ymin = -Inf, 
                                 ymax = Inf), alpha = .2)
  return(p)
}

#Stimulus plotting
plotStimulus <- function(stimulus, x, y, events = NULL, 
                         sampleRate = ceiling(length(x)/10000), 
                         add.background.line = T, 
                         xlab = "X coordinate, px", 
                         ylab = "Y coordinate, px",
                         xlim = c(0, ncol(stimulus)), 
                         ylim = c(0, nrow(stimulus)), 
                         title = "PORs on stimulus", 
                         xbreaks = (xlim[2] - xlim[1])/4,
                         ybreaks = (ylim[2] - ylim[1])/4,
                         xinter = xbreaks/4, 
                         yinter = ybreaks/4,
                         grid.alpha = .3, 
                         point.size = 1.5, 
                         point.alpha = 1, 
                         legend.title = "Events") 
{
  #sub data frame and plot base
  indexes <- seq(1, length(x), sampleRate)
  if (is.null(events)) {
    df <- data.frame(x, y)[indexes,]  
    df <- df[which(df$x >= xlim[1] & df$x <= xlim[2]),]
    df <- df[which(df$y >= ylim[1] & df$y <= ylim[2]),]
    p <- ggplot(df, aes(x = x, y = y))
  }
  else {
    df <- data.frame(x, y, events)[indexes,]  
    df <- df[which(df$x >= xlim[1] & df$x <= xlim[2]),]
    df <- df[which(df$y >= ylim[1] & df$y <= ylim[2]),]
    p <- ggplot(df, aes(x = x, y = y, col = events))
  }
  
  #Plot settings
  breaks.x <- seq(xlim[1], xlim[2], xbreaks)
  breaks.y <- seq(ylim[1], ylim[2], ybreaks)
  scale.x <- scale_x_continuous(breaks = breaks.x, limits = xlim, name = xlab)
  scale.y <- scale_y_reverse(breaks = breaks.y, limits = ylim[c(2,1)], name = ylab)
  stimulus <- annotation_raster(stimulus, 0, ncol(stimulus), 0, -nrow(stimulus))
  grid.v <- geom_vline(xintercept = seq(xlim[1], xlim[2], xinter), alpha = grid.alpha)
  grid.h <- geom_hline(yintercept = seq(ylim[1], ylim[2], yinter), alpha = grid.alpha)
  #Plotting
  p <- p + scale.x + scale.y + stimulus + grid.v + grid.h +
    ggtitle(title) + labs(colour = legend.title) +
    theme(panel.border = element_rect(fill = NA, size = 1))
  if (add.background.line) p <- p + geom_path(col = "black")
  if(point.size > 0) p <- p + geom_point(size = point.size, alpha = point.alpha)  
  return(p)
}

#Heatmap plotting
plotHeatmap <- function(stimulus, x, y, 
                        xlab = "X coordinate, px", 
                        ylab = "Y coordinate, px",
                        xlim = c(0, ncol(stimulus)), 
                        ylim = c(0, nrow(stimulus)), 
                        fill.color = "red",
                        title = "Heatmap", 
                        legend.title = "Density", 
                        axes.hide = F) 
{
  #sub data frame and plotting
  df <- data.frame(x, y)
  df <- df[which(df$x >= xlim[1] & df$x <= xlim[2]),]
  df <- df[which(df$y >= ylim[1] & df$y <= ylim[2]),]
  p <- ggplot(df, aes(x = x, y = y))
  if (axes.hide) {
    p <- p + theme(axis.ticks = element_blank(), axis.text.y = element_blank(), axis.text.x = element_blank())
    ylab = ""; xlab = ""
  }
  p <- p + scale_x_continuous(name = xlab, limits = xlim) +
    scale_y_reverse(name = ylab, limits = ylim[c(2,1)]) +
    annotation_raster(stimulus, 0, ncol(stimulus), 0, -nrow(stimulus)) +
    stat_density2d(aes(fill = ..density.., alpha = ifelse(..density..<1e-7,0,.9)), geom = "raster", contour = F) +
    scale_alpha_continuous(range = c(0, .7), guide = F) + ggtitle(title)
  if (legend.title != F) p <- p + scale_fill_gradient(low = "white", high = fill.color) + labs(fill = legend.title)
  else p <- p + scale_fill_gradient(low = "white", high = fill.color, guide = F)
  return(p)
}

#Scanpath plotting
plotScanpath <- function(x.center, y.center, duration, stimulus = matrix(0, ncol = 1280, nrow = 1024),
                         xlab = "X coordinate, px", ylab = "Y coordinate, px", 
                         xlim = c(0, ncol(stimulus)), ylim = c(0, nrow(stimulus)), 
                         plotting.indexes = 1:length(x.center), 
                         title = "Scanpath on stimulus", legend.title = "Duration", 
                         legend.levels = 5, fix.color = "red", 
                         fix.alpha = .7, fix.max.size = 10, 
                         max.arrows = length(x.center) - 1,
                         draw.arrow = T, arrow.color = "green", 
                         arrow.width = 1, arrow.head = 15, 
                         arrow.angle = 20, arrow.alpha = .7) 
{
  #Data preparation
  df <- data.frame(x = x.center, y = y.center, size = duration)
  df <- df[plotting.indexes,]
  df <- df[which(df$x >= xlim[1] & df$x <= xlim[2]),]
  df <- df[which(df$y >= ylim[1] & df$y <= ylim[2]),]
  #Base of plot
  p <- ggplot(df, aes(x = x, y = y, size = size)) +
    scale_x_continuous(limits = xlim, name = xlab) +
    scale_y_reverse(limits = ylim[c(2,1)], name = ylab) +
    ggtitle(title) + labs(size = legend.title)
  #Stimulus
  if (!is.matrix(stimulus))
    p <- p + annotation_raster(stimulus, 0, ncol(stimulus), 0, -nrow(stimulus))
  #Arrows
  if (draw.arrow) {
    if (max.arrows < 1) max.arrows <- 1
    if (max.arrows > (length(df$x) - 1)) max.arrows <- length(df$x) - 1
    fx <- df$x[-length(df$x)]; lx <- df$x[-1] #fx - First X, lx - Last X
    fx <- fx[seq(1, length(fx), length.out = max.arrows)]
    lx <- lx[seq(1, length(lx), length.out = max.arrows)]
    fy <- df$y[-length(df$y)]; ly <- df$y[-1] #fy - First Y, ly - Last Y
    fy <- fy[seq(1, length(fy), length.out = max.arrows)]
    ly <- ly[seq(1, length(ly), length.out = max.arrows)]
    p <- p + annotate("segment", x = fx, y = fy, 
                      xend = lx, yend = ly, 
                      arrow = arrow(
                        angle = arrow.angle,
                        length = unit(arrow.head, "points")
                      ), col = arrow.color, size = arrow.width, alpha = arrow.alpha)
  }
  #Points
  p <- p + geom_point(col = fix.color, alpha = fix.alpha) +
    scale_size_area(max_size = fix.max.size, 
                    breaks = round(seq(min(df$size), 
                                       max(df$size), 
                                       length.out = legend.levels), 2))
  #Result
  return(p)
}
