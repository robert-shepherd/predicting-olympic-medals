#install.packages("tidyverse")
#install.packages("skimr")
#install.packages("corrplot")
#install.packages("GGally")
#install.packages("gridExtra")
#install.packages("MASS")
#install.packages("pscl")
#install.packages("lme4")


library(tidyverse)
library(skimr)
library(corrplot)
library(GGally)
library(gridExtra)
library(MASS) #Used for stepwise regression
library(pscl) #Used to build zero inflation glm models
library(lme4) #Used to build random effect models


#Making sure dplyr select is used instead of MASS
select <- dplyr::select

################
#Preparing data#
################

#Reading in data
oldat <- read_csv(url("http://www.stats.gla.ac.uk/~tereza/rp/rioolympics.csv"),na="#N/A")

#Setting working directory
setwd("C:/Users/rshepherd/OneDrive - Merkle Inc/Documents/MSc/Advanced Predictive Models/APM Project Assignment")

#Setting seed
set.seed(123)

#Checking structure
kable(skim(oldat))

#Partitioning data

#Recreating host variable
oldat$host00 <- 0
oldat$host04 <- 0
oldat$host08 <- 0
oldat$host12 <- 0
oldat$host16 <- 0

oldat$host00[oldat$country=="Australia"] <- 1
oldat$host04[oldat$country=="Greece"] <- 1
oldat$host08[oldat$country=="China"] <- 1
oldat$host12[oldat$country=="United Kingdom"] <- 1
oldat$host16[oldat$country=="Brazil"] <- 1

#Missing values

#Replacing 2 gdp with previous years
oldat$gdp16[is.na(oldat$gdp16)] <- oldat$gdp12[is.na(oldat$gdp16)]
#Ignoring missing 2000 gdp as this is not used for the model

#Replacing BMI with mean value
bmi.mean <- mean(oldat$bmi,na.rm = TRUE)
oldat$bmi[is.na(oldat$bmi)] <- bmi.mean

#Checking total figures are calculated correctly
sum(oldat$gold00) == unique(oldat$totgold00)
sum(oldat$tot00) == unique(oldat$totmedals00)

sum(oldat$gold04) == unique(oldat$totgold04)
sum(oldat$tot04) == unique(oldat$totmedals04)

sum(oldat$gold08) == unique(oldat$totgold08)
sum(oldat$tot08) == unique(oldat$totmedals08)

sum(oldat$gold12) == unique(oldat$totgold12)
sum(oldat$tot12) == unique(oldat$totmedals12)

sum(oldat$gold16) == unique(oldat$totgold16)
sum(oldat$tot16) == unique(oldat$totmedals16)
#Total and gold medals are not consistent for 2016

sum(oldat$gold16)
unique(oldat$totgold16)

sum(oldat$tot16)
unique(oldat$totmedals16)
#As the sum's are greater than the total medals the total medals will be updated to match the sum

#Updating values
oldat$totgold16 <- sum(oldat$gold16)
oldat$totmedals16 <- sum(oldat$tot16)

#Casting binary variables as factors
oldat$soviet <- as.factor(oldat$soviet)
oldat$comm <- as.factor(oldat$comm)
oldat$muslim <- as.factor(oldat$muslim)
oldat$oneparty <- as.factor(oldat$oneparty)
oldat$host00 <- as.factor(oldat$host00)
oldat$host04 <- as.factor(oldat$host04)
oldat$host08 <- as.factor(oldat$host08)
oldat$host12 <- as.factor(oldat$host12)
oldat$host16 <- as.factor(oldat$host16)

#Adding in continent/region
#https://cloford.com/resources/codes/index.htm
continents_lookup <- read_csv("continent_lookup.csv")

#Joining to data
oldat.cont <- oldat %>%
  left_join(continents_lookup,by="country.code")

#Tidying names
names(oldat.cont) <- tolower(names(oldat.cont))

#Checking entries
unique(oldat.cont$continent)
unique(oldat.cont$region)

#Manually assigning missing variables
#https://en.wikipedia.org/wiki/Regions_of_Europe
#https://en.wikipedia.org/wiki/Geography_of_Asia#Regions

oldat.cont$continent[oldat.cont$country=="Hong Kong"] <- "Asia"
oldat.cont$region[oldat.cont$country=="Hong Kong"] <- "East Asia"

oldat.cont$continent[oldat.cont$country=="Kosovo"] <- "Europe"
oldat.cont$region[oldat.cont$country=="Kosovo"] <- "South East Europe"

oldat.cont$continent[oldat.cont$country=="Montenegro"] <- "Europe"
oldat.cont$region[oldat.cont$country=="Montenegro"] <- "South East Europe"

oldat.cont$continent[oldat.cont$country=="Serbia"] <- "Europe"
oldat.cont$region[oldat.cont$country=="Serbia"] <- "South East Europe"

oldat.cont$continent[oldat.cont$country=="Romania"] <- "Europe"
oldat.cont$region[oldat.cont$country=="Romania"] <- "South East Europe"

#2004 data
oldat04 <- oldat.cont %>%
  select(country, country.code,continent,region
         ,gdp = gdp04,pop = pop04,soviet,comm,muslim,oneparty
         ,gold = gold04,total = tot04,total_gold = totgold04,total_medals = totmedals04
         ,bmi,altitude,athletes = athletes04,host = host04
         #Pulling back previous event medal wins as a predictor
         ,prev_gold = gold00, prev_medals = tot00
         ,prev_gold_total = totgold00, prev_medals_total = totmedals00) %>%
  mutate(year = 2004
         #Share of medals won in the event
         ,gold_share = gold / total_gold
         ,total_share = total / total_medals
         #Share of medals won in the previous event
         ,prev_gold_share = prev_gold / prev_gold_total
         ,prev_total_share = prev_medals / prev_medals_total
         )

#2008 data
oldat08 <- oldat.cont %>%
  select(country, country.code,continent,region
         ,gdp = gdp08,pop = pop08,soviet,comm,muslim,oneparty
         ,gold = gold08,total = tot08,total_gold = totgold08,total_medals = totmedals08
         ,bmi,altitude,athletes = athletes08,host = host08
         #Pulling back previous event medal wins as a predictor
         ,prev_gold = gold04, prev_medals = tot04
         ,prev_gold_total = totgold04, prev_medals_total = totmedals04) %>%
  mutate(year = 2008
         #Share of medals won in the event
         ,gold_share = gold / total_gold
         ,total_share = total / total_medals
         #Share of medals won in the previous event
         ,prev_gold_share = prev_gold / prev_gold_total
         ,prev_total_share = prev_medals / prev_medals_total
  )

#2012 data
oldat12 <- oldat.cont %>%
  select(country, country.code,continent,region
         ,gdp = gdp12,pop = pop12,soviet,comm,muslim,oneparty
         ,gold = gold12,total = tot12,total_gold = totgold12,total_medals = totmedals12
         ,bmi,altitude,athletes = athletes12,host = host12
         #Pulling back previous event medal wins as a predictor
         ,prev_gold = gold08, prev_medals = tot08
         ,prev_gold_total = totgold08, prev_medals_total = totmedals08) %>%
  mutate(year = 2012
         #Share of medals won in the event
         ,gold_share = gold / total_gold
         ,total_share = total / total_medals
         #Share of medals won in the previous event
         ,prev_gold_share = prev_gold / prev_gold_total
         ,prev_total_share = prev_medals / prev_medals_total
  )

#2016 data
oldat16 <- oldat.cont %>%
  select(country, country.code,continent,region
         ,gdp = gdp16,pop = pop16,soviet,comm,muslim,oneparty
         ,gold = gold16,total = tot16,total_gold = totgold16,total_medals = totmedals16
         ,bmi,altitude,athletes = athletes16,host = host16
         #Pulling back previous event medal wins as a predictor
         ,prev_gold = gold12, prev_medals = tot12
         ,prev_gold_total = totgold12, prev_medals_total = totmedals12) %>%
  mutate(year = 2016
         #Share of medals won in the event
         ,gold_share = gold / total_gold
         ,total_share = total / total_medals
         #Share of medals won in the previous event
         ,prev_gold_share = prev_gold / prev_gold_total
         ,prev_total_share = prev_medals / prev_medals_total
  )


#Training
training <- oldat04 %>%
  union(oldat08) %>%
  union(oldat12)

test <- oldat16

############################
#Exploring training dataset#
############################

#Distribution of medals
h <- ggplot(training,aes(total))
h <- h + geom_histogram(binwidth = 5,fill="#2c96d4")
h <- h + ggtitle("Total medal histogram")

g <- ggplot(training,aes(gold))
g <- g + geom_histogram(binwidth = 5,fill="#E3B612")
g <- g + ggtitle("Gold medal histogram")

grid.arrange(h,g,nrow=2)

#Checking total medals by year
total_medals <- training %>%
  union(test) %>%
  select(year,total_medals) %>%
  distinct

total_gold <- training %>%
  union(test) %>%
  select(year,total_gold) %>%
  distinct

#Visualising total medals by year

#Total
t <- ggplot(total_medals,aes(year,total_medals))
t <- t + geom_line(color="#2c96d4")
t <- t + expand_limits(y = 0)
t <- t + ggtitle("Total medals won by year")
t <- t + scale_x_continuous(breaks=seq(2000,2016,4))

#Gold
m <- ggplot(total_gold,aes(year,total_gold))
m <- m + geom_line(color="#E3B612")
m <- m + expand_limits(y = 0)
m <- m + ggtitle("Gold medals won by year")
m <- m + scale_x_continuous(breaks=seq(2000,2016,4))

grid.arrange(t,m,nrow=2)

#Checking for outliers
o <- ggplot(training,aes(total,gold))
o <- o + geom_jitter(aes(color=year))
o <- o + ggtitle("Total medals vs. gold medals won")
o


#Correlation between continuous variables

#Number
training.corr <- training %>%
  select(gdp,pop,bmi,altitude,athletes,prev_gold,prev_medals,gold,total)
M <- cor(training.corr);
corrplot(M, method="number",tl.col = "black")

#Comparing distributions
training.subset <- training.corr %>%
  select(gdp,pop,bmi,altitude,athletes,prev_medals,total)

pairs(training.subset)

ggpairs(training.subset, upper=list(continuous=wrap("points", alpha=0.4, color="#d73027")),
        lower="blank", axisLabels="none")


#Exploring factors
r <- ggplot(training,aes(region,log(total)))
r <- r + geom_boxplot()
r <- r + ggtitle("Region")
r <- r + theme(axis.text.x = element_text(angle = 45, hjust = 1))
r

f <- ggplot(training,aes(continent,log(total)))
f <- f + geom_boxplot()
f <- f + ggtitle("Continent")

h <- ggplot(training,aes(host,log(total)))
h <- h + geom_boxplot()
h <- h + ggtitle("Host")

s <- ggplot(training,aes(soviet,log(total)))
s <- s + geom_boxplot()
s <- s + ggtitle("Soviet")

c <- ggplot(training,aes(comm,log(total)))
c <- c + geom_boxplot()
c <- c + ggtitle("Communist")

m <- ggplot(training,aes(muslim,log(total)))
m <- m + geom_boxplot()
m <- m + ggtitle("Muslim")

o <- ggplot(training,aes(oneparty,log(total)))
o <- o + geom_boxplot()
o <- o + ggtitle("One party")

grid.arrange(f,h,s,c,m,o,nrow=3)

#################
#Building models#
#################

#Defining error functions
rmse <- function(error)
{
  sqrt(mean(error^2))
}

mae <- function(error)
{
  mean(abs(error))
}

##############
#Linear model#
##############

#Filtering dataset
training.no.factors <- training %>%
  select(gdp,pop,athletes,prev_medals,total,bmi,altitude)

#Building 'full' model
lm.cont.full <- lm(total ~ .,data=training.no.factors)
summary(lm.cont.full)

#Removing variables via stepwise
lm.cont.step <- stepAIC(lm.cont.full, direction = "both", 
                      trace = FALSE)
summary(lm.cont.step)

#Testing adding in factors using ANCOVA

#Continent
lm.continent.separate <- lm(total~(gdp+pop+athletes+prev_medals)*continent,data=training)
lm.continent.parallel <- lm(total~gdp+pop+athletes+prev_medals+continent,data=training)
lm.continent.single <- lm(total~gdp+pop+athletes+prev_medals,data=training)

anova(lm.continent.separate,lm.continent.parallel,lm.continent.single)

#Host
lm.host.separate <- lm(total~(gdp+pop+athletes+prev_medals)*continent*host,data=training)
lm.host.parallel <- lm(total~(gdp+pop+athletes+prev_medals)*continent+host,data=training)
lm.host.single <- lm(total~(gdp+pop+athletes+prev_medals)*continent,data=training)

anova(lm.host.separate,lm.host.parallel,lm.host.single)

#Muslim
lm.muslim.separate <- lm(total~(gdp+pop+athletes+prev_medals)*continent*host*muslim,data=training)
lm.muslim.parallel <- lm(total~(gdp+pop+athletes+prev_medals)*continent*host+muslim,data=training)
lm.muslim.single <- lm(total~(gdp+pop+athletes+prev_medals)*continent*host,data=training)

anova(lm.muslim.separate,lm.muslim.parallel,lm.muslim.single)
#NOT INCLUDED

#One party
lm.oneparty.separate <- lm(total~(gdp+pop+athletes+prev_medals)*continent*host*oneparty,data=training)
lm.oneparty.parallel <- lm(total~(gdp+pop+athletes+prev_medals)*continent*host+oneparty,data=training)
lm.oneparty.single <- lm(total~(gdp+pop+athletes+prev_medals)*continent*host,data=training)

anova(lm.oneparty.separate,lm.oneparty.parallel,lm.oneparty.single)
#NOT INCLUDED

#Region
lm.region.separate <- lm(total~(gdp+pop+athletes+prev_medals)*continent*host*region,data=training)
lm.region.parallel <- lm(total~(gdp+pop+athletes+prev_medals)*continent*host+region,data=training)
lm.region.single <- lm(total~(gdp+pop+athletes+prev_medals)*continent*host,data=training)

anova(lm.region.separate,lm.region.parallel,lm.region.single)

#Checking if continent is still significant once region is added

#Region
lm.continent.test.separate <- lm(total~(gdp+pop+athletes+prev_medals)*region*host*continent,data=training)
lm.continent.test.parallel <- lm(total~(gdp+pop+athletes+prev_medals)*host*region+continent,data=training)
lm.continent.test.single <- lm(total~(gdp+pop+athletes+prev_medals)*region*host,data=training)

anova(lm.continent.test.separate,lm.continent.test.parallel,lm.continent.test.single)
#Not significant so removing

#Factor model:
lm.selected.model.factor <- lm.continent.test.single
summary(lm.selected.model.factor)

#Continuous model
lm.selected.model.cont <- lm.cont.step
summary(lm.selected.model.cont)

#Checking residuals
#Factor
lm.factor.df <- as.data.frame(cbind(observed = training$total,fitted = lm.selected.model.factor$fitted.values))

fr <- ggplot(lm.factor.df,aes(observed,fitted))
fr <- fr + geom_point()
fr
  
plot(lm.selected.model.factor,2,pch=16,cex=0.3)
plot(fitted(lm.selected.model.factor),rstandard(lm.selected.model.factor),
     xlab="Fitted Values",ylab="Standardized residuals",pch=16,cex=0.3)

#Continuous
lm.cont.df <- as.data.frame(cbind(observed = training$total,fitted = lm.selected.model.cont$fitted.values))

cr <- ggplot(lm.cont.df,aes(observed,fitted))
cr <- cr + geom_point()
cr

plot(lm.selected.model.cont,2,pch=16,cex=0.3)
plot(fitted(lm.selected.model.cont),rstandard(lm.selected.model.cont),
     xlab="Fitted Values",ylab="Standardized residuals",pch=16,cex=0.3)


#Checking predictive errors

#Factor
lm.predict.factor <- predict.lm(lm.selected.model.factor,newdata=test)
lm.observed <- test$total
lm.error.factor <- lm.observed - lm.predict.factor
rmse(lm.error.factor)
#9.075863
mae(lm.error.factor)
#3.83849

#Continuous
lm.predict.cont <- predict.lm(lm.selected.model.cont,newdata=test)
lm.observed <- test$total
lm.error.cont <- lm.observed - lm.predict.cont
rmse(lm.error.cont)
#4.217769
mae(lm.error.cont)
#2.407363
#Model performs better without factors


#Setting negative values to 0

#Factor
lm.predict.factor.0 <- lm.predict.factor
lm.predict.factor.0[lm.predict.factor.0<0] <- 0
lm.error.factor.0 <- lm.observed - lm.predict.factor.0
rmse(lm.error.factor.0)
#8.285391
mae(lm.error.factor.0)
#3.692574


#Continuous
lm.predict.cont.0 <- lm.predict.cont
lm.predict.cont.0[lm.predict.cont.0<0] <- 0
lm.error.cont.0 <- lm.observed - lm.predict.cont.0
rmse(lm.error.cont.0)
#4.21176
mae(lm.error.cont.0)
#2.358985

#Testing a few what if models

#Checking host model
lm.host.separate <- lm(total~(gdp+pop+athletes+prev_medals)*host,data=training)
lm.host.parallel <- lm(total~gdp+pop+athletes+prev_medals+host,data=training)
lm.host.single <- lm(total~gdp+pop+athletes+prev_medals,data=training)

anova(lm.host.separate,lm.host.parallel,lm.host.single)

lm.predict.host.0 <- predict(lm.host.separate,newdata=test)
lm.predict.host.0[lm.predict.host.0<0] <- 0
lm.error.host.0 <- lm.observed - lm.predict.host.0
rmse(lm.error.host.0)
#3.934143
mae(lm.error.host.0)
#2.270669

#Checking continent model
lm.continent.separate <- lm(total~(gdp+pop+athletes+prev_medals)*continent,data=training)
lm.continent.parallel <- lm(total~gdp+pop+athletes+prev_medals+continent,data=training)
lm.continent.single <- lm(total~gdp+pop+athletes+prev_medals,data=training)

anova(lm.continent.separate,lm.continent.parallel,lm.continent.single)

#Summarising model
summary(lm.continent.separate)

#Residuals
lm.continent.separate.df <- as.data.frame(cbind(observed = training$total,fitted = lm.continent.separate$fitted.values))

cs <- ggplot(lm.continent.separate.df,aes(observed,fitted))
cs <- cs + geom_point()
cs

plot(lm.continent.separate,2,pch=16,cex=0.3)
plot(fitted(lm.continent.separate),rstandard(lm.continent.separate),
     xlab="Fitted Values",ylab="Standardized residuals",pch=16,cex=0.3)

lm.continent.separate.predict <- predict(lm.continent.separate,newdata=test)
lm.continent.separate.predict[lm.continent.separate.predict<0] <- 0
lm.continent.separate.error <- lm.observed - lm.continent.separate.predict

rmse(lm.continent.separate.error)
#3.508848
mae(lm.continent.separate.error)
#2.13978

#Checking just interaction between continent and previous medals as others were not significant
lm.continent.prev_medals <- lm(total~gdp+pop+athletes+prev_medals*continent,data=training)

lm.continent.prev_medals.predict <- predict(lm.continent.prev_medals,newdata=test)
lm.continent.prev_medals.predict[lm.continent.prev_medals.predict<0] <- 0
lm.continent.prev_medals.error <- lm.observed - lm.continent.prev_medals.predict

rmse(lm.continent.prev_medals.error)
#4.357019
mae(lm.continent.prev_medals.error)
#2.470097


#Checking region model
lm.region.separate <- lm(total~(gdp+pop+athletes+prev_medals)*region,data=training)
lm.region.parallel <- lm(total~gdp+pop+athletes+prev_medals+region,data=training)
lm.region.single <- lm(total~gdp+pop+athletes+prev_medals,data=training)

anova(lm.region.separate,lm.region.parallel,lm.region.single)

lm.predict.region.0 <- predict(lm.region.separate,newdata=test)
lm.predict.region.0[lm.predict.region.0<0] <- 0
lm.error.region.0 <- lm.observed - lm.predict.region.0
rmse(lm.error.region.0)
#8.49363
mae(lm.error.region.0)
#3.789409

#Checking a model with continent and host
lm.continent.host.separate <- lm(total~(gdp+pop+athletes+prev_medals)*continent*host,data=training)
lm.continent.host.parallel <- lm(total~gdp+pop+athletes+prev_medals+continent+host,data=training)
lm.continent.host.single <- lm(total~gdp+pop+athletes+prev_medals,data=training)

anova(lm.continent.host.separate,lm.continent.host.parallel,lm.continent.host.single)

lm.predict.continent.host.0 <- predict(lm.continent.host.separate,newdata=test)
lm.predict.continent.host.0[lm.predict.continent.host.0<0] <- 0
lm.error.continent.host.0 <- lm.observed - lm.predict.continent.host.0
rmse(lm.error.continent.host.0)
#3.809549
mae(lm.error.continent.host.0)
#2.276744

#Testing a transformed continuous lm model

#Building 'full' model
lm.cont.sqrt.full <- lm(sqrt(total) ~ sqrt(gdp) + sqrt(pop) + sqrt(athletes) + sqrt(prev_medals),data=training.no.factors)
summary(lm.cont.sqrt.full)

#Removing variables via stepwise
lm.cont.sqrt.step <- stepAIC(lm.cont.sqrt.full, direction = "both", 
                        trace = FALSE)
summary(lm.cont.sqrt.step)

#Checking residuals
lm.sqrt.df <- as.data.frame(cbind(observed = training$total,fitted = lm.cont.sqrt.step$fitted.values))

sr <- ggplot(lm.sqrt.df,aes(observed,fitted))
sr <- sr + geom_point()
sr

plot(lm.cont.sqrt.step,2,pch=16,cex=0.3)
plot(fitted(lm.cont.sqrt.step),rstandard(lm.cont.sqrt.step),
     xlab="Fitted Values",ylab="Standardized residuals",pch=16,cex=0.3)


#Checking errors
lm.predict.sqrt.0 <- predict(lm.cont.sqrt.step,newdata=test)^2
lm.predict.sqrt.0[lm.predict.sqrt.0<0] <- 0
lm.error.sqrt.0 <- lm.observed - lm.predict.sqrt.0
rmse(lm.error.sqrt.0)
#4.039199
mae(lm.error.sqrt.0)
#2.309372
#Error lower than continuous

#Adding in continents
lm.sqrt.continent.separate <- lm(sqrt(total) ~ (sqrt(gdp) + sqrt(pop) + sqrt(athletes) + sqrt(prev_medals))*continent,data=training)
lm.sqrt.continent.parallel <- lm(sqrt(total) ~ sqrt(gdp) + sqrt(pop) + sqrt(athletes) + sqrt(prev_medals)+continent,data=training)
lm.sqrt.continent.single <- lm(sqrt(total) ~ sqrt(gdp) + sqrt(pop) + sqrt(athletes) + sqrt(prev_medals),data=training)

anova(lm.sqrt.continent.separate,lm.sqrt.continent.parallel,lm.sqrt.continent.single)

#Checking residuals
lm.sqrt.continent.separate.df <- as.data.frame(cbind(observed = training$total,fitted = lm.sqrt.continent.separate$fitted.values^2))

ss <- ggplot(lm.sqrt.continent.separate.df,aes(observed,fitted))
ss <- ss + geom_point()
ss

plot(lm.sqrt.continent.separate,2,pch=16,cex=0.3)
plot(fitted(lm.sqrt.continent.separate),rstandard(lm.sqrt.continent.separate),
     xlab="Fitted Values",ylab="Standardized residuals",pch=16,cex=0.3)

lm.sqrt.continent.predict.0 <- predict(lm.sqrt.continent.separate,newdata=test)^2
lm.sqrt.continent.predict.0[lm.sqrt.continent.predict.0<0] <- 0
lm.sqrt.continent.error.0 <- lm.observed - lm.sqrt.continent.predict.0
rmse(lm.sqrt.continent.error.0)
#3.677231
mae(lm.sqrt.continent.error.0)
#2.18335

#Adding in host
lm.sqrt.host.separate <- lm(sqrt(total) ~ (sqrt(gdp) + sqrt(pop) + sqrt(athletes) + sqrt(prev_medals))*host,data=training)
lm.sqrt.host.parallel <- lm(sqrt(total) ~ sqrt(gdp) + sqrt(pop) + sqrt(athletes) + sqrt(prev_medals)+host,data=training)
lm.sqrt.host.single <- lm(sqrt(total) ~ sqrt(gdp) + sqrt(pop) + sqrt(athletes) + sqrt(prev_medals),data=training)

anova(lm.sqrt.host.separate,lm.sqrt.host.parallel,lm.sqrt.host.single)
#No difference

#Adding in continents and host
lm.sqrt.continent.host.separate <- lm(sqrt(total) ~ (sqrt(gdp) + sqrt(pop) + sqrt(athletes) + sqrt(prev_medals))*continent*host,data=training)
lm.sqrt.continent.host.parallel <- lm(sqrt(total) ~ sqrt(gdp) + sqrt(pop) + sqrt(athletes) + sqrt(prev_medals)+continent+host,data=training)
lm.sqrt.continent.host.single <- lm(sqrt(total) ~ sqrt(gdp) + sqrt(pop) + sqrt(athletes) + sqrt(prev_medals),data=training)

anova(lm.sqrt.continent.host.separate,lm.sqrt.continent.host.parallel,lm.sqrt.continent.host.single)

lm.sqrt.continent.host.predict.0 <- predict(lm.sqrt.continent.host.separate,newdata=test)^2
lm.sqrt.continent.host.predict.0[lm.sqrt.continent.host.predict.0<0] <- 0
lm.sqrt.continent.host.error.0 <- lm.observed - lm.sqrt.continent.host.predict.0
rmse(lm.sqrt.continent.host.error.0)
#3.769892

##########################
#Generalised Linear Model#
##########################

#Buillding full model
glm.cont.full <- glm(total ~ .,
            family=poisson, data=training.no.factors)
summary(glm.cont.full)

#Removing variables via stepwise
glm.cont.step <- stepAIC(glm.cont.full, direction = "both", 
                        trace = FALSE)
summary(glm.cont.step)

#Checking residuals
glm.cont.step.df <- as.data.frame(cbind(observed = training$total,fitted = glm.cont.step$fitted.values))

gr <- ggplot(lm.cont.df,aes(observed,fitted))
gr <- gr + geom_point()
gr

plot(glm.cont.step,2,pch=16,cex=0.3)
plot(fitted(glm.cont.step),rstandard(glm.cont.step),
     xlab="Fitted Values",ylab="Standardized residuals",pch=16,cex=0.3)

resp <- resid(glm.cont.step, type = "pearson")
resd <- resid(glm.cont.step, type = "deviance")

p1<- ggplot(glm.cont.step, aes(sample = resp)) + geom_point(stat = "qq", color = "#7fc97f") +
  ylab("Pearson residuals")
p2<- ggplot(glm.cont.step, aes(sample = resd)) + geom_point(stat = "qq", color = "#7fc97f") +
  ylab("Deviance residuals")
p3<- ggplot(glm.cont.step, aes(x = predict(glm.cont.step, type="link"), y =resd))+
  geom_point(col = "#7fc97f") +
  ylab("Deviance residuals") + xlab("Linear predictor")
grid.arrange(p1, p2, p3, nrow = 1)

#Prediction on test
glm.predict.cont <- predict(glm.cont.step,newdata=test,type = "response")
glm.observed <- test$total
glm.error.cont <- glm.observed - glm.predict.cont
rmse(glm.error.cont)
#5.670195
mae(glm.error.cont)
#3.774292
#Bias towards 0

#Checking if overdispersion present
ggplot(glm.cont.step, aes(x=log(fitted(glm.cont.step)), y=log((test$total-fitted(glm.cont.step))^2)))+
  geom_point(col="#f46d43") +
  geom_abline(slope=1, intercept=0, col="#a6d96a", size=1) +
  ylab(expression((y-hat(mu))^2)) + xlab(expression(hat(mu)))


#Estimating the dispersion parameter
X2 <- sum(resid(glm.cont.step, type = "pearson")^2)
dp <- X2 / glm.cont.step$df.res
dp

summary(glm.cont.step, dispersion = dp)

#Checking significant of regression coefficients using an f test
drop1(glm.cont.step, test = "F")
#GDP/altitude are no longer significant

#Building quasi poisson without GDP/altitude
glm.quasi <- glm(total ~ pop + athletes + prev_medals + bmi,
            family = quasipoisson(link = "log"), data = training)

glm.predict.quasi <- predict(glm.quasi,newdata=test,type = "response", dispersion = dp)
glm.observed <- test$total
glm.error.quasi <- glm.observed - glm.predict.quasi
rmse(glm.error.quasi)
#5.889037
mae(glm.error.quasi)
#3.777748

#Negative binomial
glm.nb.full <- glm.nb(total ~ .,
               data = training.no.factors)
summary(glm.nb.full)

#Removing variables via stepwise
glm.nb.step <- stepAIC(glm.nb.full, direction = "both", 
                         trace = FALSE)
summary(glm.nb.step)
#Altitude dropped

glm.predict.nb <- predict(glm.nb.step,newdata=test,type = "response")
glm.observed <- test$total
glm.error.nb <- glm.observed - glm.predict.nb
rmse(glm.error.nb)
#17.69776
mae(glm.error.nb)
#7.868943

#Fitting zero-inflated model
glm.zeroinfl.full <- zeroinfl(total ~ .,
                     dist="poisson", data=training.no.factors)
summary(glm.zeroinfl.full)

glm.zeroinfl.full.predict <- predict(glm.zeroinfl.full,newdata=test,type="response")
glm.observed <- test$total
glm.error.cont <- glm.observed - glm.zeroinfl.full.predict
rmse(glm.error.cont)
#4.820437
mae(glm.error.cont)
#3.051596

#Fitting a hurdle model
glm.hurdle.full <- hurdle(total ~ .,
                              dist="poisson", data=training.no.factors)
summary(glm.hurdle.full)

glm.hurdle.full.predict <- predict(glm.hurdle.full,newdata=test)
glm.observed <- test$total
glm.error.cont <- glm.observed - glm.hurdle.full.predict
rmse(glm.error.cont)
#4.81837
mae(glm.error.cont)
#3.048327

#################
#Benchmark model#
#################

benchmark.expected <- test$prev_medals
benchmark.observed <- test$total
benchmark.error <- benchmark.observed - benchmark.expected
rmse(benchmark.error)
#4.174437
mae(benchmark.error)
#2.333333


#####################
#Linear mixed models#
#####################

#Creating random intercept model
mixed <- lmer(total ~ year + (1|country), data=training)
summary(mixed)

predictions_2016 <- as.data.frame(mixed@beta[1]+ranef(mixed)$country + (2016 * mixed@beta[2]))

names(predictions_2016) <- "prediction"

#Checking values
min(predictions_2016$prediction)
max(predictions_2016$prediction)

#Checking error
mixed.expected <- predictions_2016$prediction
mixed.observed <- test$total
mixed.error <- mixed.observed - mixed.expected
rmse(mixed.error)
#4.908894
mae(mixed.error)
#2.881609

#Testing predictions using predict 2016
predictions_2016_alt <- predict(mixed,newdata=test)
head(predictions_2016)
head(predictions_2016_alt)
#Same

#Adding 2000 in as a test
training_mixed <- training %>%
  select(country,total,year)

#2000 data
oldat00 <- oldat.cont %>%
  select(country,total = tot00) %>%
  mutate(year = 2000)

training_test <- training_mixed %>%
  union(oldat00)

#Checking same model with additional data - does it improve significantly?
#Creating random intercept model
mixed_test <- lmer(total ~ year + (1|country), data=training_test)
summary(mixed_test)

predictions_2016 <- as.data.frame(mixed_test@beta[1]+ranef(mixed_test)$country + (2016 * mixed_test@beta[2]))

names(predictions_2016) <- "prediction"

mixed_test.expected <- predictions_2016$prediction
mixed_test.observed <- test$total
mixed_test.error <- mixed_test.observed - mixed_test.expected
rmse(mixed_test.error)
#5.590479
#Larger error by including 2000
mae(mixed_test.error)
#3.178107
#Reverting to previous training set to build random slope model

#Uncorrelated
mixed <- lmer(total ~ year + (1|country) + (0+year|country), data=training)
summary(mixed)

predictions_2016 <- predict(mixed,newdata=test)

mixed.expected <- predictions_2016
mixed.observed <- test$total
mixed.error <- mixed.observed - mixed.expected
rmse(mixed.error)
#4.909606

#Correlated
mixed <- lmer(total ~ year + (1|country) + (1+year|country), data=training)
summary(mixed)

predictions_2016 <- predict(mixed,newdata=test)

mixed.expected <- predictions_2016
mixed.observed <- test$total
mixed.error <- mixed.observed - mixed.expected
rmse(mixed.error)
#5.400881
#Models are much less predictive when a random slope is added

#Reverting to random intercept model, testing inclusion of continent as a hierarchical model
mixed <- lmer(total ~ year + continent + (1|country), data=training)
summary(mixed)

predictions_2016 <- predict(mixed,newdata=test)

mixed.expected <- predictions_2016
mixed.observed <- test$total
mixed.error <- mixed.observed - mixed.expected
rmse(mixed.error)
#4.910379
mae(mixed.error)
#2.885285
#No improvement

#Poisson
glmm <- glmer(total ~ year + (1|country), data=training,family=poisson)
summary(glmm)

predictions_2016 <- predict(glmm,newdata=test,type = "response")

glmm.expected <- predictions_2016
glmm.observed <- test$total
glmm.error <- glmm.observed - glmm.expected
rmse(glmm.error)
#5.005843
mae(glmm.error)
#2.881769
#No improvement
