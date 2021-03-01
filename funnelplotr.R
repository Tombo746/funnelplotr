funnelplotR <- function(fp_data,
                        miny = 0,
                        maxy = 1) {
  library(ggplot2)
  
  # Create the 
  fp_data$p.se <- sqrt((fp_data$p*(1-fp_data$p)) / (fp_data$number)) 
  
  # Set boundaries of the chart
  boundx <- max(fp_data$number) * 1.1
  
  # Common effect (fixed effect model)
  p.fem <- weighted.mean(fp_data$p, 1 / fp_data$p.se ^ 2)
  
  # Draw the weighted mean line
  dfwm <- data.frame(x1 = 0,
                     x2 = boundx,
                     y1 = p.fem,
                     y2 = p.fem)
  
  # Lower and upper limits for 95% and 99.9% CI, based on FEM estimator
  number.seq <- seq(0, boundx, 0.1)
  number.ll95 <-
    p.fem - 1.96 * sqrt((p.fem * (1 - p.fem)) / (number.seq))
  number.ul95 <-
    p.fem + 1.96 * sqrt((p.fem * (1 - p.fem)) / (number.seq))
  number.ll999 <-
    p.fem - 3.29 * sqrt((p.fem * (1 - p.fem)) / (number.seq))
  number.ul999 <-
    p.fem + 3.29 * sqrt((p.fem * (1 - p.fem)) / (number.seq))
  dfCI <-
    data.frame(number.ll95,
               number.ul95,
               number.ll999,
               number.ul999,
               number.seq,
               p.fem)
  
  ggplot(fp_data, aes(x = number, y = p)) +
    
    # Draw the weighted mean line as a segment
    geom_segment(
      aes(
        x = x1,
        y = y1,
        xend = x2,
        yend = y2,
        label = NULL
      ),
      data = dfwm,
      colour = '#F79646',
      size = 1
    ) +
    
    geom_point(size = 3, colour = '#F79646') +
    
    geom_line(
      aes(x = number.seq, y = number.ll95, label = NULL),
      colour = '#515251',
      data = dfCI
    ) +
    geom_line(
      aes(x = number.seq, y = number.ul95, label = NULL),
      colour = '#515251',
      data = dfCI
    ) +
    geom_line(
      aes(x = number.seq, y = number.ll999, label = NULL),
      linetype = "dashed",
      colour = '#515251',
      data = dfCI
    ) +
    geom_line(
      aes(x = number.seq, y = number.ul999, label = NULL),
      linetype = "dashed",
      colour = '#515251',
      data = dfCI
    ) +
    
    # Customise axis
    scale_y_continuous(limits = c(miny, maxy)) +
    
    
    # Draw the vertical line
    geom_segment(aes(
      x = 0,
      y = miny,
      xend = 0,
      yend = maxy,
      label = NULL
    ),
    colour = '#515251',
    size = 1,
    alpha = .5)
}

funnelplotR(fp_data1)