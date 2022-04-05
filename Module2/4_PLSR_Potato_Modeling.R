setwd("C:/Users/sidne/OneDrive/Documents/GradSchoolClasses/2022_Spring/CSS844/2_Module")

# load all data
potato_all <- read.csv("potato_merged_new.csv")
potato_all[potato_all == "."]<- NA

# Change columns to numeric
potato_all[,c(7:1430)] <- lapply(potato_all[,c(7:1430)],as.numeric)

#Subset the data into the rating data and the spectral data
lateblight <- potato_all[, 7:23]
row.names(lateblight) <- potato_all$plot

earlyblight <- potato_all[, 24:40]
row.names(earlyblight) <- potato_all$plot

hyperspec <- as.matrix(potato_all[, 41:1430])
rownames(hyperspec) <- potato_all$plot

# Separate the hyperspec data into 5 different matricies (mean, median, max, min, stdv, indicators)
mean_hyper <- hyperspec[, grep(pattern="mean", colnames(hyperspec))]
min_hyper <- hyperspec[, grep(pattern = "min", colnames(hyperspec))]
median_hyper <- hyperspec[, grep(pattern = "median", colnames(hyperspec))]
max_hyper <- hyperspec[, grep(pattern = "max", colnames(hyperspec))]
stdev_hyper <- hyperspec[, grep(pattern = "stdev", colnames(hyperspec))]
indicators <- hyperspec[,c(1387:1390)]

#Create a dataframe with the disease data and the spectral data matrix
lb_dat <- data.frame(lateblight, I(hyperspec))
eb_dat <- data.frame(earlyblight, I(hyperspec))
eb_mean <- data.frame(earlyblight, I(mean_hyper))
eb_median <- data.frame(earlyblight, I(median_hyper))
eb_stdev <- data.frame(earlyblight, I(stdev_hyper))
eb_min <- data.frame(earlyblight, I(min_hyper))
eb_max <- data.frame(earlyblight, I(max_hyper))
eb_indicators <- data.frame(earlyblight, I(indicators))

#Install the pls package and load the library
library(pls)

#Set up the ranges for the training data set and the testing data set
eb_mean_train <- eb_mean[1:99, ]
eb_mean_test <- eb_mean[100:123, ]

eb_median_train <- eb_median[1:99, ]
eb_median_test <- eb_median[100:123, ]

eb_min_train <- eb_min[1:99, ]
eb_min_test <- eb_min[100:123, ]

eb_max_train <- eb_max[1:99, ]
eb_max_test <- eb_max[100:123, ]

eb_stdev_train <- eb_stdev[1:99, ]
eb_stdev_test <- eb_stdev[100:123, ]

eb_indicators_train <- eb_indicators[1:99, ]
eb_indicators_test <- eb_indicators[100:123, ]

#plsr
model <- plsr(eb_raudpcx100~mean_hyper, ncomp = 20, 
                 data = eb_mean_train, 
                 validation = "LOO")
summary(model)
plot(RMSEP(model), legendpos = "topright", main = "RMSEP of eb_raudpcx100", lwd = 3)

plot(model, ncomp = 5, asp = 1, line = TRUE, pch = 16, col = "blue")
plot(model, plottype = "scores", comps = 1:5)

plot(model, "loadings", legendpos = "topleft", comps = 1:5)
abline(h = 0)

pred<-predict(model, ncomp = 5, newdata = eb_mean_test)
RMSEP(model, newdata = eb_mean_test)

plot(pred, eb_mean_test$eb_raudpcx100, col = "BLUE", pch = 16, 
     xlab = "Predicted Values",
     ylab = "Observed Values",
     main = "Observed vs Predicted RAUDPC for Early Blight")
abline(0, 1, col = "red", lwd = 2)
cor(pred,eb_mean_test$eb_raudpcx100, 
    use = "pairwise.complete.obs")


#plsr
model <- plsr(eb_raudpcx100~median_hyper, ncomp = 20, 
              data = eb_median_train, 
              validation = "LOO")
summary(model)
plot(RMSEP(model), legendpos = "topright", main = "RMSEP of eb_raudpcx100", lwd = 3)

plot(model, ncomp = 3, asp = 1, line = TRUE, pch = 16, col = "blue")
plot(model, plottype = "scores", comps = 1:3)

plot(model, "loadings", legendpos = "topleft", comps = 1:3)
abline(h = 0)

pred<-predict(model, ncomp = 3, newdata = eb_median_test)
RMSEP(model, newdata = eb_median_test)

plot(pred, eb_median_test$eb_raudpcx100, col = "BLUE", pch = 16, 
     xlab = "Predicted Values",
     ylab = "Observed Values",
     main = "Observed vs Predicted RAUDPC for Early Blight")
abline(0, 1, col = "red", lwd = 2)
cor(pred,eb_mean_test$eb_raudpcx100, 
    use = "pairwise.complete.obs")


#Using disease indicators

model <- plsr(eb_raudpcx100~indicators, ncomp = 4, 
              data = eb_indicators_train, 
              validation = "LOO")
summary(model)
plot(RMSEP(model), legendpos = "topright", main = "RMSEP of eb_raudpcx100", lwd = 3)

plot(model, ncomp = 4, asp = 1, line = TRUE, pch = 16, col = "blue")
plot(model, plottype = "scores", comps = 1:4)

plot(model, "loadings", legendpos = "topleft", comps = 1:4)
abline(h = 0)

pred<-predict(model, ncomp = 4, newdata = eb_indicators_test)
RMSEP(model, newdata = eb_indicators_test)

plot(pred, eb_indicators_test$eb_raudpcx100, col = "BLUE", pch = 16, 
     xlab = "Predicted Values",
     ylab = "Observed Values",
     main = "Observed vs Predicted RAUDPC for Early Blight")
abline(0, 1, col = "red", lwd = 2)
cor(pred,eb_indicators_test$eb_raudpcx100, 
    use = "pairwise.complete.obs")







#Linear modeling for disease indicators

lm_indicators <- lm(eb_raudpcx100 ~ disease_index + red_edge + chloro_a + chloro_b, data = potato_all)
summary(lm_indicators)

plot(lm_indicators, which = 1, pch = 16, col = "blue")
library(MASS)
boxcox(lm_indicators)

#Maybe a log transformation could be good?
log_indicators <- lm(sqrt(eb_raudpcx100)~disease_index + red_edge + chloro_a + chloro_b, data = potato_all)
summary(log_indicators)

# Disease index seems to be the only significant indicator for early blight prediction
lm_disease <- lm(eb_raudpcx100 ~ disease_index, potato_all)
summary(lm_disease)

plot(potato_all$disease_index, potato_all$eb_raudpcx100, 
     main = "Disease Index Scatter Plot",
     ylab = "Early Blight - RAUDPC",
     xlab = "Disease Index", pch = 16, col = "blue")
abline(lm_disease$coefficients)


plot(lm_disease, which = 1, pch = 16, col = "green", lwd = 3)
plot(lm_disease, which = 2, pch = 16, col = "purple", lwd = 3)



# Mean Hyperspec data



#eb._dpi_0 - Not enough data

#eb._dpi_23
model <- plsr(eb._dpi_23~mean_hyper, ncomp = 20, 
              data = eb_mean_train, 
              validation = "LOO")
summary(model)
plot(RMSEP(model), legendpos = "topright", main = "RMSEP of eb_raudpcx100", lwd = 3)

plot(model, ncomp = 5, asp = 1, line = TRUE, pch = 16, col = "blue")
plot(model, plottype = "scores", comps = 1:5)

plot(model, "loadings", legendpos = "topleft", comps = 1:5)
abline(h = 0)

pred<-predict(model, ncomp = 5, newdata = eb_mean_test)
RMSEP(model, newdata = eb_mean_test)

plot(pred, eb_mean_test$eb._dpi_23, col = "BLUE", pch = 16, 
     xlab = "Predicted Values",
     ylab = "Observed Values",
     main = "Observed vs Predicted RAUDPC for Early Blight")

cor(pred,eb_mean_test$eb._dpi_23, 
    use = "pairwise.complete.obs")
# eb._dpi_23 : 0.09476696 Correlation
#               9.476696%


# eb._dpi_26
model <- plsr(eb._dpi_26~mean_hyper, ncomp = 20, 
              data = eb_mean_train, 
              validation = "LOO")
summary(model)
plot(RMSEP(model), legendpos = "topright", main = "RMSEP of eb_raudpcx100", lwd = 3)

plot(model, ncomp = 6, asp = 1, line = TRUE, pch = 16, col = "blue")
plot(model, plottype = "scores", comps = 1:6)

plot(model, "loadings", legendpos = "topleft", comps = 1:6)
abline(h = 0)

pred<-predict(model, ncomp = 6, newdata = eb_mean_test)
RMSEP(model, newdata = eb_mean_test)

plot(pred, eb_mean_test$eb._dpi_26, col = "BLUE", pch = 16, 
     xlab = "Predicted Values",
     ylab = "Observed Values",
     main = "Observed vs Predicted RAUDPC for Early Blight")

cor(pred,eb_mean_test$eb._dpi_26, 
    use = "pairwise.complete.obs")

# eb._dpi_26 : -0.27753 Correlation
#               -27.753 % Correlation

# eb._dpi_30
model <- plsr(eb._dpi_30~mean_hyper, ncomp = 20, 
              data = eb_mean_train, 
              validation = "LOO")
summary(model)
plot(RMSEP(model), legendpos = "topright", main = "RMSEP of eb_raudpcx100", lwd = 3)

plot(model, ncomp = 7, asp = 1, line = TRUE, pch = 16, col = "blue")
plot(model, plottype = "scores", comps = 1:7)

plot(model, "loadings", legendpos = "topleft", comps = 1:7)
abline(h = 0)

pred<-predict(model, ncomp = 7, newdata = eb_mean_test)
RMSEP(model, newdata = eb_mean_test)

plot(pred, eb_mean_test$eb._dpi_30, col = "BLUE", pch = 16, 
     xlab = "Predicted Values",
     ylab = "Observed Values",
     main = "Observed vs Predicted RAUDPC for Early Blight")

cor(pred,eb_mean_test$eb._dpi_30, 
    use = "pairwise.complete.obs")
# eb._dpi_30 : 0.2475715 Correlation
#              24.75715 % Correlation


# eb._dpi_37
model <- plsr(eb._dpi_37~mean_hyper, ncomp = 20, 
              data = eb_mean_train, 
              validation = "LOO")
summary(model)
plot(RMSEP(model), legendpos = "topright", main = "RMSEP of eb_raudpcx100", lwd = 3)

plot(model, ncomp = 6, asp = 1, line = TRUE, pch = 16, col = "blue")
plot(model, plottype = "scores", comps = 1:6)

plot(model, "loadings", legendpos = "topleft", comps = 1:6)
abline(h = 0)

pred<-predict(model, ncomp = 6, newdata = eb_mean_test)
RMSEP(model, newdata = eb_mean_test)

plot(pred, eb_mean_test$eb._dpi_37, col = "BLUE", pch = 16, 
     xlab = "Predicted Values",
     ylab = "Observed Values",
     main = "Observed vs Predicted RAUDPC for Early Blight")

cor(pred,eb_mean_test$eb._dpi_37, 
    use = "pairwise.complete.obs")
# eb._dpi_37 : 0.246651 Correlation
#              24.6651 % Correlation


# eb._dpi_43
model <- plsr(eb._dpi_43~mean_hyper, ncomp = 20, 
              data = eb_mean_train, 
              validation = "LOO")
summary(model)
plot(RMSEP(model), legendpos = "topright", main = "RMSEP of eb_raudpcx100", lwd = 3)

plot(model, ncomp = 7, asp = 1, line = TRUE, pch = 16, col = "blue")
plot(model, plottype = "scores", comps = 1:7)

plot(model, "loadings", legendpos = "topleft", comps = 1:7)
abline(h = 0)

pred<-predict(model, ncomp = 7, newdata = eb_mean_test)
RMSEP(model, newdata = eb_mean_test)

plot(pred, eb_mean_test$eb._dpi_43, col = "BLUE", pch = 16, 
     xlab = "Predicted Values",
     ylab = "Observed Values",
     main = "Observed vs Predicted RAUDPC for Early Blight")

cor(pred,eb_mean_test$eb._dpi_43, 
    use = "pairwise.complete.obs")
# eb._dpi_43 : 0.4184527 Correlation
#              41.84527 % Correlation


# eb._dpi_47
model <- plsr(eb._dpi_47~mean_hyper, ncomp = 20, 
              data = eb_mean_train, 
              validation = "LOO")
summary(model)
plot(RMSEP(model), legendpos = "topright", main = "RMSEP of eb_raudpcx100", lwd = 3)

plot(model, ncomp = 5, asp = 1, line = TRUE, pch = 16, col = "blue")
plot(model, plottype = "scores", comps = 1:5)

plot(model, "loadings", legendpos = "topleft", comps = 1:5)
abline(h = 0)

pred<-predict(model, ncomp = 5, newdata = eb_mean_test)
RMSEP(model, newdata = eb_mean_test)

plot(pred, eb_mean_test$eb._dpi_47, col = "BLUE", pch = 16, 
     xlab = "Predicted Values",
     ylab = "Observed Values",
     main = "Observed vs Predicted RAUDPC for Early Blight")

cor(pred,eb_mean_test$eb._dpi_47, 
    use = "pairwise.complete.obs")
# eb._dpi_47 : 0.5297782 Correlation
#              52.97782 % Correlation


# eb_audc.1
model <- plsr(eb_audc.1~mean_hyper, ncomp = 20, 
              data = eb_mean_train, 
              validation = "LOO")
summary(model)
plot(RMSEP(model), legendpos = "topright", main = "RMSEP of eb_raudpcx100", lwd = 3)

plot(model, ncomp = 5, asp = 1, line = TRUE, pch = 16, col = "blue")
plot(model, plottype = "scores", comps = 1:5)

plot(model, "loadings", legendpos = "topleft", comps = 1:5)
abline(h = 0)

pred<-predict(model, ncomp = 5, newdata = eb_mean_test)
RMSEP(model, newdata = eb_mean_test)

plot(pred, eb_mean_test$eb_audc.1, col = "BLUE", pch = 16, 
     xlab = "Predicted Values",
     ylab = "Observed Values",
     main = "Observed vs Predicted RAUDPC for Early Blight")

cor(pred,eb_mean_test$eb_audc.1, 
    use = "pairwise.complete.obs")
# eb_audc.1 : 0.09476696 Correlation
#              9.476696 % Correlation

# eb_audc.2
model <- plsr(eb_audc.2~mean_hyper, ncomp = 20, 
              data = eb_mean_train, 
              validation = "LOO")
summary(model)
plot(RMSEP(model), legendpos = "topright", main = "RMSEP of eb_raudpcx100", lwd = 3)

plot(model, ncomp = 5, asp = 1, line = TRUE, pch = 16, col = "blue")
plot(model, plottype = "scores", comps = 1:5)

plot(model, "loadings", legendpos = "topleft", comps = 1:5)
abline(h = 0)

pred<-predict(model, ncomp = 5, newdata = eb_mean_test)
RMSEP(model, newdata = eb_mean_test)

plot(pred, eb_mean_test$eb_audc.2, col = "BLUE", pch = 16, 
     xlab = "Predicted Values",
     ylab = "Observed Values",
     main = "Observed vs Predicted RAUDPC for Early Blight")

cor(pred,eb_mean_test$eb_audc.2, 
    use = "pairwise.complete.obs")
# eb_audc.2 : -0.1795536 Correlation
#              -17.95536 % Correlation

# eb_audc.3
model <- plsr(eb_audc.3~mean_hyper, ncomp = 20, 
              data = eb_mean_train, 
              validation = "LOO")
summary(model)
plot(RMSEP(model), legendpos = "topright", main = "RMSEP of eb_raudpcx100", lwd = 3)

plot(model, ncomp = 6, asp = 1, line = TRUE, pch = 16, col = "blue")
plot(model, plottype = "scores", comps = 1:5)

plot(model, "loadings", legendpos = "topleft", comps = 1:6)
abline(h = 0)

pred<-predict(model, ncomp = 6, newdata = eb_mean_test)
RMSEP(model, newdata = eb_mean_test)

plot(pred, eb_mean_test$eb_audc.3, col = "BLUE", pch = 16, 
     xlab = "Predicted Values",
     ylab = "Observed Values",
     main = "Observed vs Predicted RAUDPC for Early Blight")

cor(pred,eb_mean_test$eb_audc.3, 
    use = "pairwise.complete.obs")
# eb_audc.3 : 0.2108983 Correlation
#              21.08983 % Correlation


# eb_audc.4
model <- plsr(eb_audc.4~mean_hyper, ncomp = 20, 
              data = eb_mean_train, 
              validation = "LOO")
summary(model)
plot(RMSEP(model), legendpos = "topright", main = "RMSEP of eb_raudpcx100", lwd = 3)

plot(model, ncomp = 6, asp = 1, line = TRUE, pch = 16, col = "blue")
plot(model, plottype = "scores", comps = 1:6)

plot(model, "loadings", legendpos = "topleft", comps = 1:6)
abline(h = 0)

pred<-predict(model, ncomp = 6, newdata = eb_mean_test)
RMSEP(model, newdata = eb_mean_test)

plot(pred, eb_mean_test$eb_audc.4, col = "BLUE", pch = 16, 
     xlab = "Predicted Values",
     ylab = "Observed Values",
     main = "Observed vs Predicted RAUDPC for Early Blight")

cor(pred,eb_mean_test$eb_audc.4, 
    use = "pairwise.complete.obs")
# eb_audc.4 : 0.2669304 Correlation
#              26.69304 % Correlation


# eb_audc.5
model <- plsr(eb_audc.5~mean_hyper, ncomp = 20, 
              data = eb_mean_train, 
              validation = "LOO")
summary(model)
plot(RMSEP(model), legendpos = "topright", main = "RMSEP of eb_raudpcx100", lwd = 3)

plot(model, ncomp = 4, asp = 1, line = TRUE, pch = 16, col = "blue")
plot(model, plottype = "scores", comps = 1:4)

plot(model, "loadings", legendpos = "topleft", comps = 1:4)
abline(h = 0)

pred<-predict(model, ncomp = 4, newdata = eb_mean_test)
RMSEP(model, newdata = eb_mean_test)

plot(pred, eb_mean_test$eb_audc.5, col = "BLUE", pch = 16, 
     xlab = "Predicted Values",
     ylab = "Observed Values",
     main = "Observed vs Predicted RAUDPC for Early Blight")

cor(pred,eb_mean_test$eb_audc.5, 
    use = "pairwise.complete.obs")
# eb_audc.5 : 0.4777227 Correlation
#              47.77227 % Correlation




# eb_audc.6
model <- plsr(eb_audc.6~mean_hyper, ncomp = 20, 
              data = eb_mean_train, 
              validation = "LOO")
summary(model)
plot(RMSEP(model), legendpos = "topright", main = "RMSEP of eb_raudpcx100", lwd = 3)

plot(model, ncomp = 6, asp = 1, line = TRUE, pch = 16, col = "blue")
plot(model, plottype = "scores", comps = 1:6)

plot(model, "loadings", legendpos = "topleft", comps = 1:6)
abline(h = 0)

pred<-predict(model, ncomp = 6, newdata = eb_mean_test)
RMSEP(model, newdata = eb_mean_test)

plot(pred, eb_mean_test$eb_audc.6, col = "BLUE", pch = 16, 
     xlab = "Predicted Values",
     ylab = "Observed Values",
     main = "Observed vs Predicted RAUDPC for Early Blight")

cor(pred,eb_mean_test$eb_audc.6, 
    use = "pairwise.complete.obs")
# eb_audc.6 : 0.3851761 Correlation
#              38.51761 % Correlation


# eb_audc_total
model <- plsr(eb_audc_total~mean_hyper, ncomp = 20, 
              data = eb_mean_train, 
              validation = "LOO")
summary(model)
plot(RMSEP(model), legendpos = "topright", main = "RMSEP of eb_raudpcx100", lwd = 3)

plot(model, ncomp = 5, asp = 1, line = TRUE, pch = 16, col = "blue")
plot(model, plottype = "scores", comps = 1:5)

plot(model, "loadings", legendpos = "topleft", comps = 1:5)
abline(h = 0)

pred<-predict(model, ncomp = 6, newdata = eb_mean_test)
RMSEP(model, newdata = eb_mean_test)

plot(pred, eb_mean_test$eb_audc_total, col = "BLUE", pch = 16, 
     xlab = "Predicted Values",
     ylab = "Observed Values",
     main = "Observed vs Predicted RAUDPC for Early Blight")

cor(pred,eb_mean_test$eb_audc_total, 
    use = "pairwise.complete.obs")
# eb_audc_total : 0.4188338 Correlation
#                 41.88338 % Correlation



# eb_raudpc
model <- plsr(eb_raudpc~mean_hyper, ncomp = 20, 
              data = eb_mean_train, 
              validation = "LOO")
summary(model)
plot(RMSEP(model), legendpos = "topright", main = "RMSEP of eb_raudpcx100", lwd = 3)

plot(model, ncomp = 5, asp = 1, line = TRUE, pch = 16, col = "blue")
plot(model, plottype = "scores", comps = 1:5)

plot(model, "loadings", legendpos = "topleft", comps = 1:5)
abline(h = 0)

pred<-predict(model, ncomp = 6, newdata = eb_mean_test)
RMSEP(model, newdata = eb_mean_test)

plot(pred, eb_mean_test$eb_raudpc, col = "BLUE", pch = 16, 
     xlab = "Predicted Values",
     ylab = "Observed Values",
     main = "Observed vs Predicted RAUDPC for Early Blight")

cor(pred,eb_mean_test$eb_raudpc, 
    use = "pairwise.complete.obs")
# eb_raudpc : 0.4475052 Correlation
#                 44.75052 % Correlation



# eb_raudpcx100
model <- plsr(eb_raudpcx100~mean_hyper, ncomp = 20, 
              data = eb_mean_train, 
              validation = "LOO")
summary(model)
plot(RMSEP(model), legendpos = "topright", main = "RMSEP of eb_raudpcx100", lwd = 3)

plot(model, ncomp = 5, asp = 1, line = TRUE, pch = 16, col = "blue")
plot(model, plottype = "scores", comps = 1:5)

plot(model, "loadings", legendpos = "topleft", comps = 1:5)
abline(h = 0)

pred<-predict(model, ncomp = 6, newdata = eb_mean_test)
RMSEP(model, newdata = eb_mean_test)

plot(pred, eb_mean_test$eb_raudpcx100, col = "BLUE", pch = 16, 
     xlab = "Predicted Values",
     ylab = "Observed Values",
     main = "Observed vs Predicted RAUDPC for Early Blight")

cor(pred,eb_mean_test$eb_raudpcx100, 
    use = "pairwise.complete.obs")
# eb_raudpcx100 : 0.418494 Correlation
#                 41.8494 % Correlation