setwd("/Users/mackenziejacobs/Library/CloudStorage/OneDrive-MichiganStateUniversity/MSUGradSchool/Spring2022/CSS844/Module2")
library(Rmisc)
library(tidyr)
library(ggplot2)


data <- read.csv("2020 Potato Late Blight Trial RAUDPC 20220224.csv", header=FALSE)
data <- data[5:131,]



LB <- data[,1:13]
rownames(LB) <- c(1:127)
colnames(LB) <- LB[1,]
LB <- LB[2:127,]
rownames(LB) <- c(1:126)
LBcols <- c(7:13)
LB[LBcols] <- sapply(LB[LBcols],as.numeric)
LB <- LB[order(LB$`LB%_dpi_47`, decreasing = TRUE),]
rownames(LB) <- c(1:126)
LB0 <- LB[55:118,]
LB0$category <- c("LB0")
LB1 <- LB[34:55,]
LB1$category <- c("LB1")
LB2 <- LB[20:33,]
LB2$category <- c("LB2")
LB3 <- LB[12:19,]
LB3$category <- c("LB3")
LB5 <-LB[1:11,]
LB5$category <- c("LB5")

LBtwo <- rbind(LB0, LB1, LB2, LB3, LB5)
LBtwomeans <- aggregate(LBtwo[ , 7:13], list(LBtwo$category), mean)
LBgathered <- gather(LBtwomeans, key="time", value="infected", 2:8, factor_key=TRUE)
colnames(LBgathered) <- c("group", "time", "infected")

ggplot(LBgathered, aes(x = time, y = infected, colour =group, group = group)) +
  geom_line() +
  geom_point() +
  labs(title="Late Blight Infection Over 47 Days",
       x ="Days", y = "% Infected")



EB <- data[, c(1:6,24:30)]
rownames(EB) <- c(1:127)
colnames(EB) <- EB[1,]
EB <- EB[2:127,]
rownames(EB) <- c(1:126)
EBcols <- c(7:13)
EB[EBcols] <- sapply(EB[EBcols],as.numeric)
EB <- EB[order(EB$`EB%_dpi_47`, decreasing = TRUE),]
rownames(EB) <- c(1:126)
EB0 <-  EB[91:118,]
EB0$category <- c("EB0")
EB10 <- EB[71:90,]
EB10$category <- c("EB10")
EB20 <- EB[51:70,]
EB20$category <- c("EB20")
EB30 <- EB[31:50,]
EB30$category <- c("EB30")
EB40 <- EB[13:30,]
EB40$category <- c("EB40")
EB50 <- EB[9:12,]
EB50$category <- c("EB50")
EB60 <- EB[5:8,]
EB60$category <- c("EB60")
EB70 <- EB[2:4,]
EB70$category <- c("EB70")
EB80 <- EB[1,]
EB80$category <- c("EB80")

EBtwo <- rbind(EB0, EB10, EB30, EB40, EB50, EB60, EB70, EB80)
EBtwomeans <- aggregate(EBtwo[ , 7:13], list(EBtwo$category), mean)
EBgathered <- gather(EBtwomeans, key="time", value="infected", 2:8, factor_key=TRUE)
colnames(EBgathered) <- c("group", "time", "infected")

ggplot(EBgathered, aes(x = time, y = infected, colour =group, group = group)) +
  geom_line() +
  geom_point() +
  labs(title="Early Blight Infection Over 47 Days",
        x ="Days", y = "% Infected")
