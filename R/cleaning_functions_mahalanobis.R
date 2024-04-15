# NCD-RisC
# Mahalanobis outlier detection
# July 2021

library(rrcov)
library(ggplot2)

maha_clean <- function(var1, var2, level = NA, SD = 6, plot_data = TRUE){
  # default level is equivalent to being 6 sd away from the mean for normal distribution
  if (is.na(level)) level <- (1-pnorm(SD))*2

  data <- as.data.frame(cbind(var1, var2))
  b    <- covMcd(data, alpha=0.95)    # robust estimate of the covariance

  data$m_dist <- mahalanobis(data, b$center,b$cov)

  data$outlier <- "Not detected"
  data[which(data$m_dist > qchisq(1-level, 2)),'outlier'] <- "detected"

  cleaned_rows <- which(data$outlier=="detected")

  if (plot_data & length(cleaned_rows)>0) {
    d2 <- data[cleaned_rows,]
    p <- ggplot(data,aes(x=var1,y=var2)) +
          geom_bin2d(bins = 130) +
          scale_fill_continuous(type = "viridis") +
          theme_bw()+
          geom_point(data=d2, aes(x=var1, y=var2, color=outlier), shape=21, size = 3)  +
          scale_color_manual(name="", values = c("red", "red"))
    return(list(scatter = p, outliers = cleaned_rows))
  } else {
    return(list(scatter = NULL, outliers = cleaned_rows))
  }
}





