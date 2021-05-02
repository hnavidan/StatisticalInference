library(ggplot2)
library(ggExtra)
library(GGally)
library(ggcorrplot)
library(scatterplot3d)
library(ggmosaic)
library(dplyr)
library(moments)
library(reshape2)

# Import Database
song_data <- read.csv ("song_data.csv")
song_info <- read.csv("song_info.csv")

# Check if there are any NANs
sum(is.na(song_data))
sum(is.na(song_info))

############
# Question 1
############
# (a) QQ-Plot
ggplot(data = song_data, aes(sample=song_popularity)) + theme_bw() +
  geom_qq() + xlab("Theoretical Quantiles") + ylab("Sample Quantiles") +
  ggtitle("Normal Q-Q Plot of Popularity")

# (b) Pie Chart
song_data$labels <- NA
song_data$labels[song_data$song_popularity <25] <- '0-25'
song_data$labels[song_data$song_popularity >=25] <- '25-50'
song_data$labels[song_data$song_popularity >=50] <- '50-75'
song_data$labels[song_data$song_popularity >=75] <- '75-100'
piedf <- data.frame(
  class <- c('0-25', '25-50', '50-75', '75-100'),
  n <- c(sum(song_data$labels == '0-25'), sum(song_data$labels == '25-50'),
         sum(song_data$labels == '50-75'), sum(song_data$labels == '75-100')),
  percent <- c(n[1]/nrow(song_data), n[2]/nrow(song_data),
               n[3]/nrow(song_data), n[4]/nrow(song_data))*100
)

ggplot(data=piedf, aes(x = "", y = percent, fill = class)) +
  geom_bar(width = 1, stat = "identity", color= "white") +  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(class, "\n", round(percent), "%")), position = position_stack(vjust = 0.5)) +
  theme_void() + ggtitle("Pie Chart of Popularity")


# (c) Histogram
ggplot(data=song_data, aes(x=song_popularity)) + theme_bw() +
  geom_histogram(bins=16, color="Black", fill="White") +
  xlab("Song Popularity") + ylab("Frequency") + ggtitle ("Histogram of Popularity")

# (d) Density Plot
ggplot(data=song_data, aes(x=song_popularity)) + 
  geom_density(color="darkblue", fill="lightblue") + theme_bw() +
  xlab("Popularity") + ylab("Density") +
  ggtitle("Density of Popularity")

# (e) Modality and Skewness
Mode <- function(x) {
  temp <- unique(x)
  temp[which.max(tabulate(match(x, temp)))]
}
cat("Mode of Song Popularity:", Mode(song_data$song_popularity),
    "\nPearson's moment coefficient of skewness:", skewness(song_data$song_popularity))

# (f) Mean, Variance, STD and Skewness
pop_mean <- mean(song_data$song_popularity)
pop_var <- var(song_data$song_popularity)
pop_sd <- sd(song_data$song_popularity)
pop_med <- median(song_data$song_popularity)
pop_sk <- (pop_mean - pop_med)/pop_sd

cat("Mean of Popularity:", pop_mean, 
    "\nVariance of Popularity:", pop_var,
    "\nSTD of Popularity", pop_sd,
    "\nMedian of Popularity", pop_med,
    "\nNon-parametric Skewness of Popularity:", pop_sk)

# (g) Boxplot
ggplot(data=song_data, aes(x="", y=song_popularity)) + 
  geom_boxplot() + theme_bw() +  coord_flip() +
  xlab("") + ylab("Popularity") + ggtitle("Boxplot of Popularity") 

bxplt_stats <- boxplot.stats(song_data$song_popularity)
IQR <- bxplt_stats$stats[4] - bxplt_stats$stats[2]
cat("Lower Whisker:", bxplt_stats$stats[1],
    "\nUpper Whisker:", bxplt_stats$stats[5],
    "\nLower Quartile:", bxplt_stats$stats[2],
    "\nHigher Quartile:", bxplt_stats$stats[4],
    "\nIQR:", IQR)

############
# Question 2
############
# (a) Barplot
ggplot(song_data, aes(factor(key))) + geom_bar(position = "dodge") +
  xlab("Key") + ylab("Count") + ggtitle("Barplot of Key") + theme_bw()

# (b) Sorted Barplot
ggplot(song_data, aes(x=reorder(key,key,function(x)-length(x)))) + geom_bar(position = "dodge") +
  xlab("Key") + ylab("Count") + ggtitle("Barplot of Key") + theme_bw() + coord_flip()

# (c) Frequency Table
table(song_data$key)

# (d) Violon Plot
ggplot(song_data, aes(x=as.factor(key), y=song_popularity, fill=as.factor(key))) + geom_violin() + 
  xlab("Key") + ylab("Popularity") + ggtitle("Violon Plot of Key and Popularity") +
  guides(fill=guide_legend(title="Key")) + theme_bw()
  
  
############
# Question 3
############
# (a) Scatter Plot
ggplot(song_data, aes(x=energy, y=loudness)) + 
  geom_point(shape=20, color='blue', size=2) + 
  geom_smooth(method=lm, linetype="dashed", color="red") +
  ggtitle("Scatterplot of Loudness vs. Energy") + theme_bw()

# (b) Scatter Plot with Categories
ggplot(song_data, aes(x=energy, y=loudness)) + 
  geom_point(aes(color=as.factor(audio_mode)), shape=20, size=3) + 
  labs(color="Audio Mode") +
  ggtitle("Scatterplot of Loudness vs. Energy") + theme_bw()

# (c) Correlation
cat("Correlation Coefficient:", cor(song_data$energy, song_data$loudness))
cor.test(song_data$energy, song_data$loudness, method = "pearson", alpha = 0.05)

# (d) Hexbin
hexplot <- ggplot(song_data, aes(x=energy, y=loudness)) + geom_point(col="transparent") + 
  geom_hex(bins=20) + theme_bw() + ggtitle("Hexbin with Marginal Distribution")
ggExtra::ggMarginal(hexplot, type = "histogram")

# (e) 2-D Density Plot
ggplot(song_data, aes(x=energy, y=loudness)) + 
  stat_density_2d(aes(fill=..level..), geom="polygon", colour="white") + theme_bw() + 
  ggtitle("2D Histogram of Loudness vs. Energy")

############
# Question 4
############
# (a) correlogram
ggpairs(song_data, 
        columns = c("song_popularity", "loudness", "energy", "danceability"), 
        upper = list(continuous = wrap("cor", 
                                       size = 10)), 
        lower = list(continuous = "smooth")) +  theme_bw()

# (c) Heatmap correlogram
heatmap_data <- subset(song_data, select = c(song_popularity, loudness, energy, danceability))
ggcorrplot(cor(heatmap_data), hc.order = TRUE, type = "lower",
           outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("blue", "white", "red"),
           lab = TRUE) + theme_bw()

# (d) 3D Scatter Plot
scatterplot3d(song_data$song_popularity, song_data$loudness, song_data$energy,
              color="steelblue",
              main="3D Scatter Plot",
              xlab = "Song Popularity",
              ylab = "Loudness",
              zlab = "Energy")

# (e) 3D Scatter Plot with Bars, Categories and Legend
colors <- c("#56B4E9", "#E69F00")
colors <- colors[as.numeric(song_data$audio_mode)+1]
scatterplot3d(song_data$song_popularity, song_data$loudness, song_data$energy,
              pch = 16, type="h", 
              color=colors,
              main="3D Scatter Plot",
              xlab = "Song Popularity",
              ylab = "Loudness",
              zlab = "Energy")
legend(0, 7.5, legend = c("Mono", "Stereo"),
       col =  c("#56B4E9", "#E69F00"), pch = 16)

############
# Question 5
############
# (a) Contingency Table
table(song_data$time_signature, song_data$key)

# (b) Grouped Bar Chart
ggplot(song_data, aes(factor(key))) +
  geom_bar(aes(fill = factor(audio_mode)), position = position_dodge(0.8)) +
  scale_color_manual(values = c("#4286f4", "#a7f442")) +
  guides(fill=guide_legend(title="Audio Mode")) + theme_bw() +
  xlab("Key") + ylab("Count") + ggtitle("Grouped Barplot of Key")

# (c) Segemented Bar Plot 
ggplot(song_data, aes(factor(key)))+
  geom_bar(aes(fill = factor(time_signature)), position = position_stack()) +
  guides(fill=guide_legend(title="Time Signature")) + theme_bw() +
  xlab("Key") + ylab("Count") + ggtitle("Segmented Barplot of Key")

# (d) Mosaic Plot
ggplot(song_data) +
  geom_mosaic(aes(x = product(key), fill = time_signature), inherit.aes = FALSE) + 
  scale_fill_discrete("Time Signature") + theme_bw() +
  xlab("Key") + ylab("Time Signature") + ggtitle("Mosaic Plot")

############
# Question 6
############
# (a) 98% Confidence Interval
calcCI <- function(data, popSd, alpha) {
  xbar <- mean(data)
  n <- length(data)
  
  ME <- qnorm(1-alpha/2)*(popSd/sqrt(n))
  lowerlim <- xbar - ME
  upperlim <- xbar + ME
  
  return(c(lowerlim, upperlim))
}
calcCI(song_data$song_popularity, pop_sd, 0.02)

# (c) Barplot with Confidence Interval
song_data_copy <- song_data
song_data_copy$key <- as.factor(song_data$key)

for (i in 0:11){
  temp <- song_data_copy[song_data_copy$key == i, "song_popularity"]
  CI <- calcCI(temp, sd(temp), 0.02)
  song_data_copy[song_data_copy$key == i, "CI_min"] <- CI[1]
  song_data_copy[song_data_copy$key == i, "CI_max"] <- CI[2]
  song_data_copy[song_data_copy$key == i, "means"] <- mean(temp)
}

ggplot(song_data_copy, aes(x=key, y=means)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=CI_min, ymax=CI_max), width=.5) + theme_bw() + 
  xlab("Key") + ylab("Mean of Popularity") + ggtitle("Errorbar of Song Popularity")

# (d) Hypothesis Test
set.seed(100)
sample_test <- song_data[sample(nrow(song_data), 500), ]

z_test <- function(population, sample) {
  u0 <- mean(population)
  xbar <- mean(sample)
  n <- length(sample)
  std <- sd(population)
  z <- (xbar - u0)/(std/sqrt(n))
  
  if (z < 0) {
    Pvalue <- pnorm(z) * 2
  } else {
    Pvalue <- (1-pnorm(z)) * 2
  }
  
  cat("The P-Value is:", Pvalue, "\n")
  if (Pvalue > 0.05) {
    print("P-value is statistically significant hence H0 can not be rejected.")
  } else {
    print("P-value is not statistically significant hence H0 can be rejected.")
  }
}
z_test(song_data$song_popularity, sample_test$song_popularity)

# (e) 95% Confidence Interval
calcCI(sample_test$song_popularity, sd(sample_test$song_popularity), 0.05)

# (f)(g) Power and Type II eror
u0 <- mean(song_data$song_popularity)
xbar <- mean(sample_test$song_popularity)
pwr_test <- power.t.test(n=length(sample_test$song_popularity), delta=xbar-u0, sd=sd(song_data$song_popularity), 
             sig.level=0.05, type="one.sample", alternative="two.sided")
TypeII <- 1- pwr_test$power
pwr_test
cat("Type II eror:", TypeII)

############
# Question 7
############
set.seed(1000)
sample_test <- song_data[sample(nrow(song_data), 25), ]
t.test(sample_test$danceability, sample_test$speechiness, conf.level = 0.95)

############
# Question 8
############
ggplot(data=song_data, aes(x="", y=instrumentalness)) + 
  geom_boxplot() + theme_bw() +  coord_flip() +
  xlab("") + ylab("Instrumentalness)") + ggtitle("Boxplot of Instrumentalness") 

n_bootstrap = 1000
# (a) Percentile Method
bootstrap <- c()
for (i in 1:1000){
  bs_samples <- sample(song_data$instrumentalness, n_bootstrap, replace=TRUE)
  bootstrap <- c(bootstrap, median(bs_samples))
}
cat("2.5th Percentile:", quantile(bootstrap, 0.025),
    "\n97.5th Percentile:", quantile(bootstrap, 0.975),
    "\n95% CI using Percentile method: (", quantile(bootstrap, 0.025), ",",
    quantile(bootstrap, 0.975),")")

# (b) SE Method
SE_boot <- sd(bootstrap)/sqrt(1000)
DF <- n_bootstrap - 1
tstat <- qt(0.025, DF, lower.tail=FALSE)
CI_min <- median(bootstrap) - tstat*SE_boot
CI_max <- median(bootstrap) + tstat*SE_boot
cat("95% CI using SE method: (", CI_min, ",",
    CI_max, ")")

bootstrap_df <- data.frame(
  "values" <- bootstrap
)
ggplot(data=bootstrap_df, aes(values)) + 
  geom_density(color="darkblue", fill="lightblue") + theme_void() +
  xlab("Bootstrap") + ylab("Density") +
  ggtitle("Bootstrap Distribution")

############
# Question 9
############
# Statistical Analysis of Song Duration
ggplot(data=song_data, aes(x=song_duration_ms)) + theme_bw() +
  geom_histogram(bins=16, color="Black", fill="White") +
  xlab("Song Duration (ms)") + ylab("Frequency") + ggtitle ("Histogram of Song Duration")

cat("Songs Shorter than 3 minutes:", sum(song_data$song_duration_ms < 180000),
    "\nSongs Between 3 and 6 minutes:", sum(song_data$song_duration_ms >= 180000 & 
                                            song_data$song_duration_ms < 360000),
    "\nSongs Between 6 and 10 minutes:", sum(song_data$song_duration_ms >= 360000 & 
                                            song_data$song_duration_ms < 600000),
    "\nSongs Between 10 and 15 minutes:", sum(song_data$song_duration_ms >= 600000 & 
                                               song_data$song_duration_ms < 900000),
    "\nSongs Longer than 15 minutes:", sum(song_data$song_duration_ms >= 900000))

cat("Correlation Coefficient:", cor(song_data$song_popularity, song_data$song_duration_ms))

# Hexplot
hexplot <- ggplot(song_data, aes(x=song_popularity, y=song_duration_ms)) + 
  geom_point(col="transparent") + geom_hex(bins=15) + theme_bw() + 
  ggtitle("Hexbin with Marginal Distribution") + xlab("Popularity") + ylab("Duration (ms)")
ggExtra::ggMarginal(hexplot, type = "histogram")

bxplt_stats_duration <- boxplot.stats(song_data$song_duration_ms)
IQR <- bxplt_stats_duration$stats[4] - bxplt_stats_duration$stats[2]
cat("Lower Whisker:", bxplt_stats_duration$stats[1],
    "\nUpper Whisker:", bxplt_stats_duration$stats[5],
    "\nLower Quartile:", bxplt_stats_duration$stats[2],
    "\nHigher Quartile:", bxplt_stats_duration$stats[4],
    "\nmedian:", bxplt_stats_duration$stats[3])


# Categorize data and add corresponding labels
song_data$duration <- NA
song_data$duration[song_data$song_duration_ms < bxplt_stats_duration$stats[3]] <- 'Short'
song_data$duration[song_data$song_duration_ms >=bxplt_stats_duration$stats[3]] <- 'Long'

# Boxplot of these 2 groups
ggplot(data=song_data, aes(x=duration, y=song_popularity)) +
  geom_boxplot() + theme_bw() + coord_flip() + 
  xlab("Duration Label") + ylab("Popularity") + ggtitle("Boxplot of Popularity") 

# Two-sample Z-Test 
two_samp_z_test <- function(sample1, sample2, var1, var2) {
  mean1 <- mean(sample1)
  mean2 <- mean(sample2)
  n1 <- length(sample1)
  n2 <- length(sample2)
  
  z <- (mean1 - mean2)/sqrt((var1/n1)+(var2/n2))
  
  if (z < 0) {
    Pvalue <- pnorm(z) * 2
  } else {
    Pvalue <- (1-pnorm(z)) * 2
  }
  
  cat("The P-Value is:", Pvalue, "\n")
  if (Pvalue > 0.05) {
    print("P-value is statistically significant hence H0 can not be rejected.")
  } else {
    print("P-value is not statistically significant hence H0 can be rejected.")
  }
}

ShortSongs <- song_data[which(song_data$duration=='Short'), ]$song_popularity
LongSongs <- song_data[which(song_data$duration=='Long'), ]$song_popularity
two_samp_z_test(ShortSongs, LongSongs, var(ShortSongs), var(LongSongs))
