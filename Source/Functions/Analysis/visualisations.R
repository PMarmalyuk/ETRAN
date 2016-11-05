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

#X(t) plotting, markers or line, events coloring 
plotChannel <- function(t, value, value.name = "Value", events = NULL, line.size = 1, xlab = "Time, sec", ylab = "Channel value",
                        xlim = c(min(channels$t), max(channels$t)), ylim = c(min(channels$value), max(channels$value)), 
                        title = "Events, channel", events.title = "Events", channels.title = "Channels", show.warnings = T,
                        events.color.palette = adjustcolor(2:8)) {
  channels <- data.frame()
  #List input
  if (is.list(value)) {
    if (length(value) != length(value.name))
      value.name <- names(value)
    for (i in 1:length(value)) {
      if (length(value[[i]]) == 0)
        stop("Empty vector: ", names(value)[i], " value.", call. = F)
      if (length(value[[i]]) != length(t))
        stop("Different lengths of vectors \"t\" (", length(t), ") and \"", names[i], "-value\" (", length(value[[i]]), ").", call. = F)
      channels <- rbind(channels, data.frame(t = t, value = as.numeric(value[[i]]), name = value.name[i]))
    }
  } else {
    #Simple input
    if (length(t) == 0)
      stop("Empty t-vector.", call. = F)
    if (length(value) == 0)
      stop("Empty value-vector", call. = F)
    if (length(t) != length(value))
      stop("Different lengths of vectors \"t\" (", length(t), ") and \"value\" (", length(value), ").", call. = F)
    channels <- data.frame(t = t, value = as.numeric(value), name = value.name)
  }
  #Data preparation
  if (length(channels) == 0) stop("Empty \"channels\" data frame.")
  #X (time) limits check
  if (max(channels$t) < xlim[1] | min(channels$t) > xlim[2]) {
    stop("xlim: no values in such limits [", 
         round(xlim[1], 1), ";", round(xlim[2], 1), "].", sep = "", call. = FALSE)
  }
  #Y (value) limits check
  if (max(channels$value) < ylim[1] | min(channels$value) > ylim[2]) {
    stop("ylim: no values in such limits [", 
         round(ylim[1], 1), ";", round(ylim[2], 1), "].", sep = "", call. = FALSE)
    ylim <- c(min(channels$value), max(channels$value))
  }
  ###Channels
  channels <- channels[which(channels$t >= xlim[1] & channels$t <= xlim[2]),]
  channels <- channels[which(channels$value >= ylim[1] & channels$value <= ylim[2]),]
  #Plotting base
  p <- ggplot() + 
    scale_x_continuous(name = xlab, limits = xlim) + 
    scale_y_continuous(name = ylab, limits = ylim) +
    ggtitle(title) + labs(col = channels.title, fill = events.title)
  #Plotting events (additional)
  if (!is.null(events)) {
    if (max(events$end) < xlim[1] | min(events$start) > xlim[2]) {
      warning("Events: no events in such limits. Current limits are [",
              round(xlim[1], 1), ";", round(xlim[2], 1), "]. ",
              "Events are in limits [",
              round(min(events$start), 1), ";", round(max(events$end), 1), "].", sep = "", call. = FALSE)
      return(p)
      break
    }
    new.events <- data.frame()
    for(i in 1:nrow(events)) {
      left <- events$start[i]; right <- events$end[i]; marker <- events$event[i]
      if (events$start[i] > xlim[2] | events$end[i] < xlim[1]) next
      if (events$start[i] < xlim[1]) left <- xlim[1]
      if (events$end[i] > xlim[2]) right <- xlim[2]
      new.events <- rbind(new.events, data.frame(start = left, end = right, event = marker))
    }
    if (nrow(new.events) == 0) {
      return(p)
      break
    }
    if (length(events.color.palette) < length(levels(events$event))) {
      #Warning message
      warning("events.color.palette: input color vector is too short. Current length is ",
              length(events.color.palette),", needed length is ",
              length(levels(events$event)), ". Default palette is used.", sep = "", call. = F)
      #Use default color palette
      events.color.palette <- adjustcolor(2:8)
    }
    p <- p + geom_rect(data = new.events, aes(xmin = start, 
                                              xmax = end, 
                                              fill = event, 
                                              ymin = -Inf, 
                                              ymax = Inf), alpha = .1) +
      scale_fill_manual(values = events.color.palette, drop = F)
  }
  #Plotting channel
  p <- p + geom_line(data = channels, aes(x = t, y = value, col = name), size = line.size)
  return(p)
}
#Stimulus plotting
plotStimulus <- function(stimulus, x, y, events = NULL, decimation.degree = ceiling(length(x)/10000), 
                         add.background.line = T, xlab = "X coordinate, px", ylab = "Y coordinate, px",
                         xlim = c(0, ncol(stimulus)), ylim = c(0, nrow(stimulus)), title = "PORs on stimulus", 
                         xbreaks = (xlim[2] - xlim[1])/4, ybreaks = (ylim[2] - ylim[1])/4,
                         xinter = xbreaks/4, yinter = ybreaks/4,
                         grid.alpha = .3, point.size = 1.5, point.alpha = 1, legend.title = "Events",
                         events.color.palette = adjustcolor(2:8), autoscale = F, fixed.aspect.ratio = T) {
  #Autoscaling
  if (autoscale) {
    xlim <- round(c(min(x), max(x)))
    ylim <- round(c(min(y), max(y)))
  }
  #X limits check
  if (max(x) < xlim[1] | min(x) > xlim[2]) {
    stop("xlim: no values in such limits [", 
         round(xlim[1]), ";", round(xlim[2]), "].", sep = "", call. = FALSE)
  }
  #Y limits check
  if (max(y) < ylim[1] | min(y) > ylim[2]) {
    stop("ylim: no values in such limits [", 
         round(ylim[1]), ";", round(ylim[2]), "].", sep = "", call. = FALSE)
  }
  #sub data frame and plot base
  indexes <- seq(1, length(x), decimation.degree)
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
    if (nrow(df) == 0) {
      stop("There is no data to plot in such limits:\n\txlim: [", 
           xlim[1], ";", xlim[2], "]\n\tylim: [", 
           ylim[1], ";", ylim[2], "]", sep = "", call. = F)
    }
    p <- ggplot(df, aes(x = x, y = y, col = events))
    #Checking input colors length
    if (length(events.color.palette) < length(levels(events))) {
      #Warning message
      warning("events.color.palette: input color vector is too short. Current length is ",
              length(events.color.palette),", needed length is ",
              length(levels(events)), ". Default palette is used.", sep = "", call. = F)
      #Use default color palette
      events.color.palette <- adjustcolor(2:8)
    }
  }
  #Plot settings
  breaks.x <- round(seq(xlim[1], xlim[2], xbreaks))
  breaks.y <- round(seq(ylim[1], ylim[2], ybreaks))
  scale.x <- scale_x_continuous(breaks = breaks.x, limits = xlim, name = xlab, expand = c(0, 0))
  scale.y <- scale_y_reverse(breaks = breaks.y, limits = ylim[c(2,1)], name = ylab, expand = c(0, 0))
  stimulus <- annotation_raster(stimulus, 0, ncol(stimulus), 0, -nrow(stimulus))
  grid.v <- geom_vline(xintercept = seq(xlim[1], xlim[2], xinter), alpha = grid.alpha)
  grid.h <- geom_hline(yintercept = seq(ylim[1], ylim[2], yinter), alpha = grid.alpha)
  #Plotting
  p <- p + scale.x + scale.y + stimulus + grid.v + grid.h +
    ggtitle(title) + labs(colour = legend.title) +
    theme(panel.border = element_rect(fill = NA, size = 1)) +
    scale_color_manual(values = events.color.palette, drop = F)
  if (add.background.line) p <- p + geom_path(col = "black")
  if (point.size > 0) p <- p + geom_point(size = point.size, alpha = point.alpha)  
  if (fixed.aspect.ratio) p <- p + coord_fixed()
  return(p)
}
#Heatmap plotting
plotHeatmap <- function(stimulus, x, y, w = NULL, decimation.degree = ceiling(length(x)/10000), fogmap.mode.on = F,
                        xlab = "X coordinate, px", ylab = "Y coordinate, px", screen.res = c(1280, 1024),
                        xlim = c(0, screen.res[1]), ylim = c(0, screen.res[2]), fill.color = c("yellow", "red"),
                        title = "Heatmap", legend.title = "Density", axes.hide = F, fixed.aspect.ratio = T,
                        gridsize = c(ncol(stimulus), nrow(stimulus)), H = NULL, percent.to.hide = .3) {
  #Lengths check
  if (!is.null(w)) {
    if (length(x) != length(y) | length(y) != length(w))
      stop("x, y, w: different vectors size.", call. = FALSE)
  } else {
    if (length(x) != length(y))
      stop("x, y: different vectors size.", call. = FALSE)
  }
  #X limits check
  if (max(x) < xlim[1] | min(x) > xlim[2]) {
    stop("xlim: no values in such limits [", 
         round(xlim[1]), ";", round(xlim[2]), "].", sep = "", call. = FALSE)
  }
  #Y limits check
  if (max(y) < ylim[1] | min(y) > ylim[2]) {
    stop("ylim: no values in such limits [", 
         round(ylim[1]), ";", round(ylim[2]), "].", sep = "", call. = FALSE)
  }
  #sub data frame and plotting
  if (is.null(w)) df <- data.frame(x, y)
  else df <- data.frame(x, y, w)
  df <- df[which(df$x >= xlim[1] & df$x <= xlim[2]),]
  df <- df[which(df$y >= ylim[1] & df$y <= ylim[2]),]
  indexes <- seq(1, length(x), decimation.degree)
  df <- df[indexes,]
  p <- ggplot(df, aes(x = x, y = y))
  if (axes.hide) {
    p <- p + theme(axis.ticks = element_blank(), axis.text.y = element_blank(), axis.text.x = element_blank())
    ylab = ""; xlab = ""
  }
  #KDE data.frame
  if (is.null(w)) 
    if (is.null(H))
      kde.obj <- ks::kde(x = df, xmin = c(xlim[1], ylim[1]), xmax = c(xlim[2], ylim[2]), gridsize = gridsize)
    else
      kde.obj <- ks::kde(x = df, xmin = c(xlim[1], ylim[1]), xmax = c(xlim[2], ylim[2]), gridsize = gridsize, H = H)
  else {
    w <- df$w; df <- df[,-3]
    w <- w/sum(w)*length(w)
    if (is.null(H))
      kde.obj <- ks::kde(x = df, xmin = c(xlim[1], ylim[1]), xmax = c(xlim[2], ylim[2]), w = w, gridsize = gridsize)
    else
      kde.obj <- ks::kde(x = df, xmin = c(xlim[1], ylim[1]), xmax = c(xlim[2], ylim[2]), w = w, gridsize = gridsize, H = H)
  }
  kde.frame <- data.frame(expand.grid(x = kde.obj$eval.points[[1]], y = kde.obj$eval.points[[2]]), z = as.vector(kde.obj$estimate))
  kde.frame$z <- kde.frame$z/(max(kde.frame$z) - min(kde.frame$z))
  #plotting
  p <- p + scale_x_continuous(name = xlab, limits = xlim, expand = c(0, 0)) +
    scale_y_reverse(name = ylab, limits = ylim[c(2,1)], expand = c(0, 0)) +
    # annotation_raster(stimulus, 0, ncol(stimulus), 0, -nrow(stimulus)) + ggtitle(title)
    annotation_raster(raster = stimulus, 
                      xmin = (screen.res[1] - ncol(stimulus))/2, 
                      xmax = ncol(stimulus) + (screen.res[1] - ncol(stimulus))/2, 
                      ymin = 0, ymax = -nrow(stimulus)) + ggtitle(title)
  #Fogmap or heatmap
  zv <- kde.frame$z
  mzv <- max(zv)
  if (fogmap.mode.on) {
    p <- p + geom_tile(data = kde.frame, 
                       aes(x = x, y = y, fill = z, 
                           alpha = ifelse(1 - (max(z) - z)/(max(z) - min(z)) < percent.to.hide, 
                                          (max(z) - z)/(max(z) - min(z)), 1))) + 
      scale_fill_gradient(low = "black", 
                          high = "white", 
                          breaks = c(mzv*.05, mzv*.25, mzv*.5, mzv*.75, mzv*.95), 
                          labels = round(c(mzv*.05, mzv*.25, mzv*.5, mzv*.75, mzv*.95), 6)) +
      scale_alpha(guide = "none")
  }
  else {
    p <- p + geom_tile(data = kde.frame, aes(x = x, y = y, fill = z), 
                       alpha = ifelse(1 - (max(zv) - zv)/(max(zv) - min(zv)) < percent.to.hide, 0, .7)) + 
      scale_fill_gradient(low = fill.color[1], high = fill.color[2], 
                          breaks = c(mzv*.05, mzv*.25, mzv*.5, mzv*.75, mzv*.95), 
                          labels = round(c(mzv*.05, mzv*.25, mzv*.5, mzv*.75, mzv*.95), 6))
  }
  if (legend.title != F) 
    p <- p + labs(fill = legend.title)
  else 
    p <- p + guides(fill = "none")
  if (fixed.aspect.ratio) p <- p + coord_fixed()
  return(p)
}
#Scanpath plotting
plotScanpath <- function(x.center, y.center, duration, stimulus = matrix(0, ncol = 1280, nrow = 1024),
                         xlab = "X coordinate, px", ylab = "Y coordinate, px", xlim = c(0, ncol(stimulus)), ylim = c(0, nrow(stimulus)), 
                         plotting.indexes = 1:length(x.center), title = "Scanpath on stimulus", legend.title = "Duration", 
                         legend.levels = 5, fix.color = "red", fix.alpha = .7, fix.max.size = 10, max.arrows = length(x.center) - 1,
                         draw.arrow = T, arrow.color = "black", arrow.width = 1, arrow.head = 0, arrow.angle = 20, arrow.alpha = .7,
                         draw.label = T, fix.label.color = "black", fix.label.size = 4, fix.label.alpha = 1, fixed.aspect.ratio = T) {
  #Data preparation
  df <- data.frame(x = x.center, y = y.center, size = duration)
  df <- df[plotting.indexes,]
  df <- df[which(df$x >= xlim[1] & df$x <= xlim[2]),]
  df <- df[which(df$y >= ylim[1] & df$y <= ylim[2]),]
  df$number <- 1:nrow(df)
  #Base of plot
  p <- ggplot(df, aes(x = x, y = y, size = size)) +
    scale_x_continuous(limits = xlim, name = xlab, expand = c(0, 0)) +
    scale_y_reverse(limits = ylim[c(2,1)], name = ylab, expand = c(0, 0)) +
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
  #Fixations
  p <- p + geom_point(col = fix.color, alpha = fix.alpha) +
    scale_size_area(max_size = fix.max.size, 
                    breaks = round(seq(min(df$size), 
                                       max(df$size), 
                                       length.out = legend.levels), 2))
  #Fixation numbers
  if (draw.label)
    p <- p + geom_text(aes(label = number), size = fix.label.size, col = fix.label.color, alpha = fix.label.alpha)
  #Result
  if (fixed.aspect.ratio) p <- p + coord_fixed()
  return(p)
}