# loading packages

library(tidyverse)
library(dplyr)
library(epiDisplay)
library(questionr)
library(gplots)
library(gmodels)
library(ggpubr)
library(stargazer)
library(jtools)

## To conduct the analyses, the datasets "p.RData" and "sq.RData" have to be loaded ##
## into the environment first ##

# creating NA, EU, and 21st century data sets

N.A. <- p %>% 
  filter(continent == "na")

EU. <- p %>% 
  filter(continent == "eu")

twenty1 <- p %>% 
  filter(phdwhen == "2")

EU21 <- twenty1 %>% 
  filter(continent == "eu")

NA21 <- twenty1 %>% 
  filter(continent == "na")


# gender comparison

p$numgender <- NA
p$numgender[p$gender == "f"] <- 1
p$numgender[p$gender == "nb"] <- 0
p$numgender[p$gender == "m"] <- 0

p$Faculty <- NA
p$Faculty[p$position == "a1"] <- "Other Faculty (n = 1967)"
p$Faculty[p$position == "a2"] <- "Other Faculty (n = 1967)"
p$Faculty[p$position == "s"] <- "Other Faculty (n = 1967)"
p$Faculty[p$position == "p"] <- "Full Prof (n = 1810)"

plotmeans(numgender ~ Faculty, data = p, n.label = FALSE, xlab = "", ylab = "Proportion Female", barcol = "black")

Gendercomparison <- p %>% 
  group_by(phdfrom) %>% 
  filter(n() > 30) %>% 
  ungroup()

CrossTable(Gendercomparison$phdfrom, Gendercomparison$gender)

## gender comparison 21st century

Gender21 <- Gendercomparison %>% 
  filter(phdwhen == 2)

CrossTable(Gender21$phdfrom, Gender21$gender)

# weighted rankings

NAR <- wtd.table(NA21$phdfrom, weights = NA21$weight, na.rm = TRUE)
NAR <- as.data.frame(NAR)
NAR <- NAR[order(-NAR$Freq),]
NAR$Rating <- NAR$Freq / 1.7332 # to normalize 100 points for rank 1

EUR <- wtd.table(EU21$phdfrom, weights = EU21$weight, na.rm = TRUE)
EUR <- as.data.frame(EUR)
EUR <- EUR[order(-EUR$Freq),]
EUR$Rating <- EUR$Freq / 1.0744 # to normalize 100 points for rank 1

ALL <- wtd.table(twenty1$phdfrom, weights = twenty1$weight, na.rm = TRUE)
ALL <- as.data.frame(ALL)
ALL <- ALL[order(-ALL$Freq),]
ALL$Rating <- ALL$Freq / 1.8801 # to normalize 100 points for rank 1

# show unweighted number of placements

## all

Rankingtable <- tab1(p$phdfrom, sort.group = "decreasing", graph = FALSE)
Rankingdataset <- as.data.frame(Rankingtable)

NewRankingtable <- tab1(twenty1$phdfrom, sort.group = "decreasing", graph = FALSE)
NewRankingdataset <- as.data.frame(NewRankingtable)


## na

NARankingtable <- tab1(N.A.$phdfrom, sort.group = "decreasing", graph = FALSE)
NARankingdataset <- as.data.frame(NARankingtable)

NANewRankingtable <- tab1(NA21$phdfrom, sort.group = "decreasing", graph = FALSE)
NANewRankingdataset <- as.data.frame(NANewRankingtable)


## eu

EURankingtable <- tab1(EU.$phdfrom, sort.group = "decreasing", graph = FALSE)
EURankingdataset <- as.data.frame(EURankingtable)

EUNewRankingtable <- tab1(EU21$phdfrom, sort.group = "decreasing", graph = FALSE)
EUNewRankingdataset <- as.data.frame(EUNewRankingtable)


# Scatterplots QS/Shanghai and Placement Success

sq$q <- as.numeric(sq$q)
sq$shanghai <- as.numeric(sq$shanghai)


## q

q <- sq %>% 
  filter(q != "NA")
q$placement <- as.numeric(q$placement)

q <- q %>% 
  mutate(top = ifelse(q<50,1,0))

topbottomqplot <- ggplot(q, aes(x = q, y = placement)) + 
  geom_point(size = 3) +
  geom_smooth(method=lm, se = TRUE, color = "black") +
  theme_light() +
  scale_y_continuous(trans = "reverse", breaks = c(1, 25, 50, 75)) +
  scale_x_continuous(trans = "reverse", breaks = c(1, 50, 100, 150)) +
  stat_cor(method = "pearson", size = 7) +
  xlab("QS Ranking") +
  ylab("Placement Ranking") +
  facet_grid(~top) +
  theme(axis.title = element_text(size = 25)) +
  theme(axis.text = element_text(size = 20)) 

topbottomqplot


qplot <- ggplot(q, aes(x = q, y = placement)) + 
  geom_point(size = 3) +
  geom_smooth(method=lm, se = TRUE, color = "black") +
  theme_light() +
  scale_y_continuous(trans = "reverse", breaks = c(1, 25, 50, 75)) +
  scale_x_continuous(trans = "reverse", breaks = c(1, 50, 100, 150)) +
  stat_cor(method = "pearson", size = 7) +
  xlab("QS Ranking") +
  ylab("Placement Ranking") +
  theme(axis.title = element_text(size = 25)) +
  theme(axis.text = element_text(size = 20)) 

qplot


## shanghai

sh <- sq %>% 
  filter(shanghai != "NA")
sh$placement <- as.numeric(sh$placement)

sh <- sh %>% 
  mutate(top = ifelse(shanghai<50,1,0))

topbottomshplot <- ggplot(sh, aes(x = shanghai, y = placement)) + 
  geom_point(size = 3) +
  geom_smooth(method=lm, se = TRUE, color = "black") +
  theme_light() +
  scale_y_continuous(trans = "reverse", breaks = c(1, 25, 50, 75)) +
  scale_x_continuous(trans = "reverse", breaks = c(1, 50, 100, 150, 200, 250, 300)) +
  stat_cor(method = "pearson", size = 7) +
  xlab("Shanghai Ranking") +
  ylab("Placement Ranking") +
  facet_grid(~top) +
  theme(axis.title = element_text(size = 25)) +
  theme(axis.text = element_text(size = 20)) 

topbottomshplot

shanghaiplot <- ggplot(sh, aes(x = shanghai, y = placement)) + 
  geom_point(size = 3) +
  geom_smooth(method=lm, se = TRUE, color = "black") +
  theme_light() +
  scale_y_continuous(trans = "reverse", breaks = c(1, 25, 50, 75)) +
  scale_x_continuous(trans = "reverse", breaks = c(1, 50, 100, 150, 200, 250, 300)) +
  stat_cor(method = "pearson", size = 7) +
  xlab("Shanghai Ranking") +
  ylab("Placement Ranking") +
  theme(axis.title = element_text(size = 25)) +
  theme(axis.text = element_text(size = 20)) 

shanghaiplot


### plot

ggarrange(qplot, shanghaiplot, topbottomqplot, topbottomshplot)


# multivariate analysis and plot (QS/Shanghai + Department Size + Continent-fixed-effects)

m <- sq %>% 
  filter(departmentsize != "NA" & q != "NA" & shanghai != "NA" & continent != "aus")

multimodel <- lm(placement ~ departmentsize + shanghai + q + continent, data = m)
summary(multimodel)

multi <- effect_plot(multimodel, pred = departmentsize, interval = TRUE, 
                     plot.points = TRUE, x.label = "Department Size", 
                     y.label = "Placement Ranking", point.size = 2) +
  scale_y_continuous(trans = "reverse", breaks = c(1, 25, 50, 75)) +
  theme(axis.title = element_text(size = 25)) +
  theme(axis.text = element_text(size = 20)) 


# bivariate analysis and plot of department size and placement ranking

sizemodel <- lm(placement ~ departmentsize, data = m)
summary(sizemodel)

size <- effect_plot(sizemodel, pred = departmentsize, interval = TRUE, 
                    plot.points = TRUE, x.label = "Department Size", 
                    y.label = "Placement Ranking", point.size = 2) +
  scale_y_continuous(trans = "reverse", breaks = c(1, 25, 50, 75)) +
  theme(axis.title = element_text(size = 25)) +
  theme(axis.text = element_text(size = 20)) 


## plot and table of multivariate and bivariate analyses together

ggarrange(size, multi, labels = c("Bivariate OLS, ß1 = -0.37**", "OLS with Controls, ß1 = -0.24*"), font.label=list(size = 30))

stargazer(sizemodel, multimodel, type = "html", out = "regression.html")
