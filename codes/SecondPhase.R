library(ggplot2)
library(mosaic)
library(pROC)
library(caret)
library(GGally)
library(gridExtra)

# Import Database
song_data <- read.csv ("song_data.csv")
song_info <- read.csv("song_info.csv")

# Check if there are any NANs
sum(is.na(song_data))
sum(is.na(song_info))

#Remove Duplicates
dups <- song_data[duplicated(song_data),]
cat("Number of duplicate rows:", nrow(dups))
song_data <- song_data[!duplicated(song_data), ]

############
# Question 1
############
n <- 1000 #Number of random samples
split_idx <- sample(nrow(song_data), nrow(song_data)/2)
part_1 <- song_data[split_idx, ]
part_2 <- song_data[-split_idx, ]

sample_1 <- part_1[sample(nrow(part_1), n), "time_signature"]
sample_2 <- part_2[sample(nrow(part_2), n), "key"]

n1 <- sum(sample_1 == 4)
phat_1 <- n1/n
n2 <- sum(sample_2 == 1)
phat_2 <- n2/n

cat("n1p1 = ", n1*phat_1, "and n1(1-p1) = ", n1*(1-phat_1),
    "\nn2p2 = ", n2*phat_2, "and n2(1-p2) = ", n2*(1-phat_2))

# (a) Confidence Interval
z_star <- qnorm(0.025, lower.tail=FALSE)
SE_diff <- sqrt(phat_1*(1-phat_1)/n1 + phat_2*(1-phat_2)/n2)
diff_CI_min <- min(phat_1 - phat_2 - z_star*SE_diff, phat_1 - phat_2 + z_star*SE_diff)
diff_CI_max <- max(phat_1 - phat_2 - z_star*SE_diff, phat_1 - phat_2 + z_star*SE_diff)
cat("95% Confidence Interval for differences:", diff_CI_min, diff_CI_max)

# (a) Independence
table(song_data$key, song_data$time_signature)
chisq.test(table(sample_1, sample_2))

############
# Question 2
############
simulation_sample <- song_data[sample(nrow(song_data), 10), "audio_mode"]

simulate <- function(sample, p_null=0.5, trials=1000)
{
  n <- length(sample)
  observed <- sum(sample)
  phat = observed/n
  
  sim_results <- data.frame(n=integer(), ones=integer(), zeros=integer(), prop=double())
  for (i in 1:trials){
    rands <- rbinom(n, 1, p_null)
    ones <- sum(rands)
    zeros <- n - ones
    prop <- ones/n
    sim_results[nrow(sim_results) + 1,] = c(n, ones, zeros, prop)
  }
  
  if(observed > p_null * n) {
    pvalue <- sum(sim_results$prop >= phat)/trials
  } else {
    pvalue <- sum(sim_results$prop <= phat)/trials
  }

  hist <- histogram(~prop, data = sim_results, 
                    v = phat, 
                    width = 0.025,
                    xlab = "Proportion", 
                    main = "Rnadomization Distribution",
                    groups = prop >= phat)
  
  cat(paste("H0: p =", p_null, "\n"))
  cat(paste("HA: p >", p_null, "\n"))
  cat(paste("p-value = ", pvalue))
  
  return(hist)
}
print(simulation_sample)
simulate(simulation_sample, 0.5, 1000)

############
# Question 3
############
#(a) Goodness of Fit
ggplot(data=song_data, aes(x=key)) +
  geom_histogram(bins=12, color="Black", fill="White") +  theme_bw() +
  xlab("Key") + ylab("Frequency") + ggtitle ("Histogram of Key")

dataset_props <- prop.table(table(song_data$key))
print(round(dataset_props, 4))

sample_a<- song_data[sample(nrow(song_data), 100), 'key']
sample_a_props <- prop.table(table(sample_a))
print(sample_a_props)

biased <- song_data[which(song_data$key < 9), ]
sample_b <- biased[sample(nrow(biased), 100), 'key']
sample_b_props <- prop.table(table(factor(sample_b, levels=0:11)))
print(sample_b_props)

print(table(sample_a))
print(table(factor(sample_b, levels=0:11)))

print(round(dataset_props*100, 4))


chisq.test(table(sample_a), p=unname(dataset_props))
chisq.test(table(factor(sample_b, levels=0:11)), p=unname(dataset_props))

#(b) Independence Test
sample_c<- song_data[sample(nrow(song_data), 100), 'time_signature']
table(sample_a, sample_c)
chisq.test(table(sample_a, sample_c))

############
# Question 4
############
#(a) Least Square Regression
lsreg <- lm(loudness ~ energy, data=song_data)
summary(lsreg)

#(c) Scatter Plot
ggplot(song_data, aes(x=energy, y=loudness)) + 
  geom_point(color='#2980B9', size = 3) +  theme_bw() +
  geom_smooth(method=lm, se=FALSE, color='#000000', linetype="dashed") +
  ggtitle("Scatter Plot with Regression Line") + xlab("Energy") + ylab("Loudness")

#(d.1) Hypothesis Test
reg_sample<- song_data[sample(nrow(song_data), 27), ]
sample_lsreg <- lm(loudness ~ energy, data=reg_sample)
results <- summary(sample_lsreg)
results

slope_test <- function(slope, SE, df) {
  T_stat <- (slope - 0)/SE
  Pvalue <- pt(T_stat, df, lower.tail=FALSE)*2
  
  cat("The P-Value is:", Pvalue, "\n")
  if (Pvalue > 0.05) {
    print("P-value is statistically significant hence H0 can not be rejected.")
    print("Its likely that the slope of regression line is zero.")
  } else {
    print("P-value is not statistically significant hence H0 can be rejected.")
    print("It is likely that the slope of regression line is not zero.")
  }
}

b1 <- results$coefficients[2,1]
SEb1 <- results$coefficients[2,2]
slope_test(b1, SEb1, results$df[2])

#(d.2) Confidence Interval
t_star <- qt(0.025, df=results$df[2], lower.tail=FALSE)
CI_min <- b1 - (t_star*SEb1)
CI_max <- b1 + (t_star*SEb1)
cat("95% Confidence Interval:", CI_min, CI_max)

#(d.3) Linear Regression with two Explnatory Variables
sample_lsreg_2 <- lm(loudness ~ energy + liveness, data=reg_sample)
summary(sample_lsreg_2)

cat("Adj-R2 for Original Model:", results$adj.r.squared,
    "\nAdj-R2 for New Model:", summary(sample_lsreg_2)$adj.r.squared)

############
# Question 5
############
#(a) Model Selection
# Backward Elimination
summary(lm(loudness ~ . - song_name, data=song_data))
summary(lm(loudness ~ . - song_name - key, data=song_data))
summary(lm(loudness ~ . - song_name - key - liveness, data=song_data))

# Forward Selection
summary(lm(loudness ~ song_popularity, data=song_data))$adj.r.squared
summary(lm(loudness ~ song_duration_ms, data=song_data))$adj.r.squared
summary(lm(loudness ~ acousticness, data=song_data))$adj.r.squared
summary(lm(loudness ~ danceability, data=song_data))$adj.r.squared
summary(lm(loudness ~ instrumentalness, data=song_data))$adj.r.squared
summary(lm(loudness ~ key, data=song_data))$adj.r.squared
summary(lm(loudness ~ liveness, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy, data=song_data))$adj.r.squared
summary(lm(loudness ~ audio_mode, data=song_data))$adj.r.squared
summary(lm(loudness ~ tempo, data=song_data))$adj.r.squared
summary(lm(loudness ~ time_signature, data=song_data))$adj.r.squared
summary(lm(loudness ~ audio_valence, data=song_data))$adj.r.squared

summary(lm(loudness ~ energy + song_popularity, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + song_duration_ms, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + acousticness, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + danceability, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + instrumentalness, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + key, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + liveness, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + audio_mode, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + tempo, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + time_signature, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + audio_valence, data=song_data))$adj.r.squared

summary(lm(loudness ~ energy + instrumentalness + song_popularity, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + instrumentalness + song_duration_ms, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + instrumentalness + danceability, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + instrumentalness + acousticness, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + instrumentalness + key, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + instrumentalness + liveness, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + instrumentalness + audio_mode, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + instrumentalness + tempo, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + instrumentalness + time_signature, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + instrumentalness + audio_valence, data=song_data))$adj.r.squared

summary(lm(loudness ~ energy + instrumentalness + danceability + song_popularity, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + instrumentalness + danceability + song_duration_ms, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + instrumentalness + danceability + acousticness, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + instrumentalness + danceability + key, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + instrumentalness + danceability + liveness, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + instrumentalness + danceability + audio_mode, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + instrumentalness + danceability + tempo, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + instrumentalness + danceability + time_signature, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + instrumentalness + danceability + audio_valence, data=song_data))$adj.r.squared

summary(lm(loudness ~ energy + instrumentalness + danceability + audio_valence + song_popularity, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + instrumentalness + danceability + audio_valence + song_duration_ms, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + instrumentalness + danceability + audio_valence + acousticness, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + instrumentalness + danceability + audio_valence + key, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + instrumentalness + danceability + audio_valence + liveness, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + instrumentalness + danceability + audio_valence + audio_mode, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + instrumentalness + danceability + audio_valence + tempo, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + instrumentalness + danceability + audio_valence + time_signature, data=song_data))$adj.r.squared

summary(lm(loudness ~ energy + instrumentalness + danceability + audio_valence + song_duration_ms + song_popularity, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + instrumentalness + danceability + audio_valence + song_duration_ms + acousticness, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + instrumentalness + danceability + audio_valence + song_duration_ms + key, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + instrumentalness + danceability + audio_valence + song_duration_ms + liveness, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + instrumentalness + danceability + audio_valence + song_duration_ms + audio_mode, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + instrumentalness + danceability + audio_valence + song_duration_ms + tempo, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + instrumentalness + danceability + audio_valence + song_duration_ms + time_signature, data=song_data))$adj.r.squared

summary(lm(loudness ~ energy + instrumentalness + danceability + audio_valence + song_duration_ms + song_popularity + acousticness, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + instrumentalness + danceability + audio_valence + song_duration_ms + song_popularity + key, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + instrumentalness + danceability + audio_valence + song_duration_ms + song_popularity + liveness, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + instrumentalness + danceability + audio_valence + song_duration_ms + song_popularity + audio_mode, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + instrumentalness + danceability + audio_valence + song_duration_ms + song_popularity + tempo, data=song_data))$adj.r.squared
summary(lm(loudness ~ energy + instrumentalness + danceability + audio_valence + song_duration_ms + song_popularity + time_signature, data=song_data))$adj.r.squared

final <- lm(loudness ~ energy + instrumentalness + danceability + 
              audio_valence + song_duration_ms + song_popularity, data=song_data)
summary(final)

#(b) 5-fold CV and RMSE
train.control <- trainControl(method = "cv", number = 5)
cv_model <- train(
  loudness ~ energy + instrumentalness + danceability + 
    audio_valence + song_duration_ms + song_popularity,
  song_data,
  method = "lm",
  trControl = train.control)
print(cv_model)

#(c) MLR Conditions
cond_df <- data.frame(
  resid <- residuals(final),
  song_pop <- song_data$song_popularity,
  duration <- song_data$song_duration_ms,
  danceability <- song_data$danceability,
  instrument <- song_data$instrumentalness,
  energy <- song_data$energy,
  audio_valence <- song_data$audio_valence,
  fitted <- final$fitted)

# Condition 1
p1 <- ggplot(cond_df, aes(x=song_pop, y=resid)) + 
  geom_point(color='royal blue') + geom_hline(yintercept=0, size=1.2, linetype='dashed') + 
  theme_bw() + xlab("Song Popularity") + ylab("Residuals") + ggtitle("Residuals vs. Song Popularity") 
p2 <- ggplot(cond_df, aes(x=duration, y=resid)) + 
  geom_point(color='royal blue') + geom_hline(yintercept=0, size=1.2, linetype='dashed') + 
  theme_bw() + xlab("Song Duration (ms)") + ylab("Residuals") + ggtitle("Residuals vs. Song Duration") 
p3 <- ggplot(cond_df, aes(x=danceability, y=resid)) + 
  geom_point(color='royal blue') + geom_hline(yintercept=0, size=1.2, linetype='dashed') + 
  theme_bw() + xlab("Danceability") + ylab("Residuals") + ggtitle("Residuals vs. Danceability") 
p4 <- ggplot(cond_df, aes(x=instrument, y=resid)) + 
  geom_point(color='royal blue') + geom_hline(yintercept=0, size=1.2, linetype='dashed') + 
  theme_bw() + xlab("Instrumentalness") + ylab("Residuals") + ggtitle("Residuals vs. Instrumentalness")
p5 <- ggplot(cond_df, aes(x=energy, y=resid)) + 
  geom_point(color='royal blue') + geom_hline(yintercept=0, size=1.2, linetype='dashed') + 
  theme_bw() + xlab("Energy") + ylab("Residuals") + ggtitle("Residuals vs. Energy") 
p6 <- ggplot(cond_df, aes(x=audio_valence, y=resid)) + 
  geom_point(color='royal blue') + geom_hline(yintercept=0, size=1.2, linetype='dashed') + 
  theme_bw() + xlab("Audio Valence") + ylab("Residuals") + ggtitle("Residuals vs. Audio Valence") 

grid.arrange(p1, p2, p3, p4, p5, p6, ncol=2)

# Condition 2
ggplot(cond_df, aes(x=resid))  + 
  geom_histogram(bins=15, color="Black", fill="White") + theme_bw() +
  xlab("Residuals") + ylab("Frequency") + ggtitle ("Histogram of Residuals")

ggplot(data = cond_df, aes(sample=resid)) + theme_bw() +
  geom_qq() + xlab("Theoretical Quantiles") + ylab("Sample Quantiles") +
  ggtitle("Normal Q-Q Plot of Residuals")

# Condition 3
ggplot(cond_df, aes(x=fitted, y=resid)) + 
  geom_point(color='royal blue') + geom_hline(yintercept=0, size=1.2, linetype='dashed') + 
  theme_bw() + xlab("Fitted") + ylab("Residuals") + ggtitle("Residuals vs. Fitted") 

ggplot(cond_df, aes(x=fitted, y=abs(resid))) + 
  geom_point(color='royal blue') + geom_hline(yintercept=0, size=1.2, linetype='dashed') + 
  theme_bw() + xlab("Fitted") + ylab("Residuals") + ggtitle("Residuals vs. Fitted") 

# Condition 4
ggplot(cond_df, aes(x=1:length(resid), y=resid)) + 
  geom_point(color='royal blue') + geom_hline(yintercept=0, size=1.2, linetype='dashed') + 
  theme_bw() + ylab("Residuals") + xlab("Index") + ggtitle("Residuals") 

#(d) Correlogram
jpeg("Plot.jpg", units="in", width=10, height=10, res=300)
ggpairs(song_data, 
        columns = c("loudness", "song_popularity", "song_duration_ms", "danceability",
                    "instrumentalness", "energy", "audio_valence"), 
        upper = list(continuous = wrap("cor", 
                                       size = 5)), 
        lower = list(continuous = "smooth")) +  theme_bw()
dev.off()

#(e) Variation Explained by the Modle
cat("R2 for Final Model:", summary(final)$r.squared)

############
# Question 6
############
#Add two new binary categorical variables
song_data$danceable <- NA
song_data$danceable[song_data$danceability < 0.5] <- 0
song_data$danceable[song_data$danceability >= 0.5] <- 1

song_data$acoustic <- NA
song_data$acoustic[song_data$acousticness  < 0.5] <- 'No'
song_data$acoustic[song_data$acousticness >= 0.5] <- 'Yes'

#(a) Logistic Regression
logreg <- glm(danceable ~ energy + instrumentalness + loudness + speechiness + tempo +
              acoustic + audio_valence + song_popularity, data=song_data, family = binomial)
logreg_results <- summary(logreg)
logreg_results
exp(logreg$coefficients)

#(c) RoC Curve and AuC
real <- song_data$danceable
preds <- predict(logreg, type=c("response"))

roc_curve <- roc(real ~ preds)
ggroc(roc_curve, alpha=1, colour='darkblue', legacy.axes = TRUE) + theme_bw() + 
  xlab("1-Specificity") + ylab("Sensitivity") + ggtitle("RoC Curve")

auc(roc_curve)

#(d) 98% Confidence Interval for Odd Ratio
PE1 <- logreg_results$coefficients[2,1]
SE1 <- logreg_results$coefficients[2,2]
PE2 <- logreg_results$coefficients[3,1]
SE2 <- logreg_results$coefficients[3,2]
PE3 <- logreg_results$coefficients[4,1]
SE3 <- logreg_results$coefficients[4,2]
PE4 <- logreg_results$coefficients[5,1]
SE4 <- logreg_results$coefficients[5,2]
PE5 <- logreg_results$coefficients[6,1]
SE5 <- logreg_results$coefficients[6,2]
PE6 <- logreg_results$coefficients[7,1]
SE6 <- logreg_results$coefficients[7,2]
PE7 <- logreg_results$coefficients[8,1]
SE7 <- logreg_results$coefficients[8,2]
PE8 <- logreg_results$coefficients[9,1]
SE8 <- logreg_results$coefficients[9,2]
CV <- abs(qnorm(0.01))

cat("98% Confidence Interval for Energy:",
    "\n  Log Odd Ratio:", PE1-(CV*SE1), PE1+(CV*SE1),
    "\n  Odd Ratio:", exp(PE1-(CV*SE1)), exp(PE1+(CV*SE1)),
    "\n98% Confidence Interval for Instrumentalness:",
    "\n  Log Odd Ratio:", PE2-(CV*SE2), PE2+(CV*SE2),
    "\n  Odd Ratio:", exp(PE2-(CV*SE2)), exp(PE2+(CV*SE2)),
    "\n98% Confidence Interval for Loudness:",
    "\n  Log Odd Ratio:", PE3-(CV*SE3), PE3+(CV*SE3),
    "\n  Odd Ratio:", exp(PE3-(CV*SE3)), exp(PE3+(CV*SE3)),
    "\n98% Confidence Interval for Speechiness:",
    "\n  Log Odd Ratio:", PE4-(CV*SE4), PE4+(CV*SE4),
    "\n  Odd Ratio:", exp(PE4-(CV*SE4)), exp(PE4+(CV*SE4)),
    "\n98% Confidence Interval for Tempo:",
    "\n  Log Odd Ratio:", PE5-(CV*SE5), PE5+(CV*SE5),
    "\n  Odd Ratio:", exp(PE5-(CV*SE5)), exp(PE5+(CV*SE5)),
    "\n98% Confidence Interval for Acoustic=Yes:",
    "\n  Log Odd Ratio:", PE6-(CV*SE6), PE6+(CV*SE6),
    "\n  Odd Ratio:", exp(PE6-(CV*SE6)), exp(PE6+(CV*SE6)),
    "\n98% Confidence Interval for Audio Valence:",
    "\n  Log Odd Ratio:", PE7-(CV*SE7), PE7+(CV*SE7),
    "\n  Odd Ratio:", exp(PE7-(CV*SE7)), exp(PE7+(CV*SE7)),
    "\n98% Confidence Interval for Song Popularity:",
    "\n  Log Odd Ratio:", PE8-(CV*SE8), PE8+(CV*SE8),
    "\n  Odd Ratio:", exp(PE8-(CV*SE8)), exp(PE8+(CV*SE8)))

############
# Question 7
############
#(a) Comparing Explanatory Features
energy_reg <- glm(danceable ~ energy, data=song_data, family = binomial)
instru_reg <- glm(danceable ~ instrumentalness, data=song_data, family = binomial)
loudness_reg <- glm(danceable ~ loudness, data=song_data, family = binomial)
speech_reg <- glm(danceable ~ speechiness, data=song_data, family = binomial)
tempo_reg <- glm(danceable ~ tempo, data=song_data, family = binomial)
acoustic_reg <- glm(danceable ~ acoustic, data=song_data, family = binomial)
valence_reg <- glm(danceable ~ audio_valence, data=song_data, family = binomial)
popularity_reg <- glm(danceable ~ song_popularity, data=song_data, family = binomial)

energy_roc <- roc(real ~ predict(energy_reg, type=c("response")))
instru_roc <- roc(real ~ predict(instru_reg, type=c("response")))
loudness_roc <- roc(real ~ predict(loudness_reg, type=c("response")))
speech_roc <- roc(real ~ predict(speech_reg, type=c("response")))
tempo_roc <- roc(real ~ predict(tempo_reg, type=c("response")))
acoustic_roc <- roc(real ~ predict(acoustic_reg, type=c("response")))
valence_roc <- roc(real ~ predict(valence_reg, type=c("response")))
popularity_roc <- roc(real ~ predict(popularity_reg, type=c("response")))

cat("Energy:",auc(energy_roc),
    "\nInstrumentalness:",auc(instru_roc),
    "\nLoudness:",auc(loudness_roc),
    "\nSpeechiness:",auc(speech_roc),
    "\nTempo:",auc(tempo_roc),
    "\nAcoustic:",auc(acoustic_roc),
    "\nAudio Valence:",auc(valence_roc),
    "\nSong Popularity:",auc(popularity_roc))

roc.list <- list("Energy" = energy_roc, "Instrumentalness" = instru_roc, 
                 "Loudness" = loudness_roc, "Speechiness" = speech_roc, 
                 "Tempo" = tempo_roc, "Acoustic" = acoustic_roc,
                 "Audio Valence" = valence_roc, "Song Popularity" = popularity_roc)
ggroc(roc.list, alpha=1, aes=c("color"), lwd=1.2, legacy.axes=TRUE) + geom_abline() + 
  theme_bw() + xlab("1-Specificity") + ylab("Sensitivity") + ggtitle("ROC Curve") 

#(b) Odd Ratio Curve
OR <- exp(logreg$coefficients)['acousticYes']
p_false <- seq(0,0.99,0.01)
p_true <- OR*(p_false/(1-p_false))/(1+OR*(p_false/(1-p_false))) 
odd_ratio <- data.frame(pfalse <- p_false,
                        ptrue <- p_true)

ggplot(odd_ratio, aes(x=pfalse, y=ptrue)) +
  geom_point(color='#2980B9', size = 2) + geom_abline() +
  ggtitle("Acoustic Odd Ratio Curve") +  theme_bw() +
  xlab("P(Danceable | Not Acoustic)") + ylab("P(Danceable | Acoustic)")
cat("Odd Ratio:", OR)

#(c) New Logistic Regression Model
new_logreg <- glm(danceable ~ instrumentalness + speechiness + tempo  + 
                    acoustic + audio_valence, data=song_data, family = binomial)
new_logreg_results <- summary(logreg)
new_logreg_results

new_model_roc <- roc(real ~ predict(new_logreg, type=c("response")))

comparing_list <- list("Original" = roc_curve, "New" = new_model_roc)

ggroc(comparing_list, aes=c("color"), lwd=1.2, legacy.axes = TRUE) + theme_bw() + 
  xlab("1-Specificity") + ylab("Sensitivity") + ggtitle("RoC Curve")
auc(new_model_roc)

#(d) Threshold
result_coords <- coords(new_model_roc, 
                        "best", 
                        best.method="closest.topleft", 
                        ret=c("threshold", "tn", "tp", "fp", "fn", "accuracy"))
print(result_coords)

#(e) Utility
tp_coef <- 1
tn_coef <- 1
fp_coef <- -20
fn_coef <- -5

new_preds <- predict(new_logreg, type=c("response"))
p_thresh <- seq(0,1,0.01)

TN <- c()
FP <- c()
FN <- c()
TP <- c()
for (i in 1:length(p_thresh)){
    conf.matrix <- confusionMatrix(data=factor(as.numeric(new_preds > p_thresh[i])), 
                                   reference = factor(song_data$danceable))
    TN <- c(TN, conf.matrix$table[1])
    FP <- c(FP, conf.matrix$table[2])
    FN <- c(FN, conf.matrix$table[3])
    TP <- c(TP, conf.matrix$table[4])
}

utility.function <- TN*tn_coef + TP*tp_coef + FN*fn_coef + FP*fp_coef
utility.df <- data.frame(p = p_thresh, utility = utility.function)

ggplot(utility.df, aes(x=p, y=utility)) +
  geom_point(color='#2980B9', size = 2) + geom_abline() +
  ggtitle("Utility Curve") +  theme_bw() +
  xlab("P") + ylab("Utility")

cat("Optimal Threshold: ", p_thresh[which(utility.function == max(utility.function))])

############
# Question 8
############
summary(lm(song_duration_ms ~ . - song_name - danceable - acoustic, data=song_data))

cat("Song Duration's Correlation Coefficients with:",
    "\nPopularity: ", cor(song_data$song_duration_ms, song_data$song_popularity),
    "\nAcousticness: ", cor(song_data$song_duration_ms, song_data$acousticness),
    "\nDanceability: ", cor(song_data$song_duration_ms, song_data$danceability),
    "\nEnergy: ", cor(song_data$song_duration_ms, song_data$energy),
    "\nInstrumentalness: ", cor(song_data$song_duration_ms, song_data$instrumentalness),
    "\nKey: ", cor(song_data$song_duration_ms, song_data$key),
    "\nLiveness: ", cor(song_data$song_duration_ms, song_data$liveness),
    "\nLoudness: ", cor(song_data$song_duration_ms, song_data$loudness),
    "\nAudio Mode: ", cor(song_data$song_duration_ms, song_data$audio_mode),
    "\nSpeechiness: ", cor(song_data$song_duration_ms, song_data$speechiness),
    "\nTempoo: ", cor(song_data$song_duration_ms, song_data$tempo),
    "\nTime Signature: ", cor(song_data$song_duration_ms, song_data$time_signature),
    "\nAudio Valence: ", cor(song_data$song_duration_ms, song_data$audio_valence))
