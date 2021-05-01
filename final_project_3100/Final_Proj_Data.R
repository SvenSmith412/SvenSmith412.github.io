###Final Project Data
library(tidyverse)
library(readxl)
library(janitor)
library(tidyverse)
library(patchwork)
library(tidyr)
library(ggplot2)
library(skimr)
#install.packages("lmrTest")
library(lmrTest)
library(GGally)
library(dplyr)
library(broom)
library(MASS)
library(plotly)
#install.packages("gplots")
library(gplots)
library(fitdistrplus)
library(modelr)
#install.packages("report")
library(report)


d1 <- read_xlsx("./FinalProj_Data_1.xlsx")
d1 <- d1 %>% filter(Season != "Career")
d2 <- read_xlsx("./FinalProj_Data_2.xlsx")
d2 <- d2 %>% filter(Season != "Career")
d3 <- read_xlsx("./FinalProj_Data_3.xlsx")
d3 <- d3 %>% filter(Season != "Career")
d4 <- read_xlsx("./FinalProj_Data_4.xlsx")
d4 <- d4 %>% filter(Season != "Career")
d5 <- read_xlsx("./FinalProj_Data_5.xlsx")
d5 <- d5 %>% filter(Season != "Career")
d6 <- read_xlsx("./FinalProj_Data_6.xlsx")
d6 <- d6 %>% filter(Season != "Career")
d7 <- read_xlsx("./FinalProj_Data_7.xlsx")
d7 <- d7 %>% filter(Season != "Career")
d8 <- read_xlsx("./FinalProj_Data_8.xlsx")
d8 <- d8 %>% filter(Season != "Career")
d9 <- read_xlsx("./FinalProj_Data_9.xlsx")
d9 <- d9 %>% filter(Season != "Career")
d10 <- read_xlsx("./FinalProj_Data_10.xlsx")
d10 <- d10 %>% filter(Season != "Career")
d11 <- read_xlsx("./FinalProj_Data_11.xlsx")
d11 <- d11 %>% filter(Season != "Career")
d12 <- read_xlsx("./FinalProj_Data_12.xlsx")
d12 <- d12 %>% filter(Season != "Career")
d13 <- read_xlsx("./FinalProj_Data_13.xlsx")
d13 <- d13 %>% filter(Season != "Career")
d14 <- read_xlsx("./FinalProj_Data_14.xlsx")
d14 <- d14 %>% filter(Season != "Career")
d15 <- read_xlsx("./FinalProj_Data_15.xlsx")
d15 <- d15 %>% filter(Season != "Career")



# add a new column for each df containing player name
d1$Player <- "Tobias Harris"
d2$Player <- "Bradley Beal"
d3$Player <- "Jonas Valanciunas"
d4$Player <- "Ish Smith"
d5$Player <- "Danilo Gallinari"
d6$Player <- "Damian Lillard"
d7$Player <- "Nikola Vucevic"
d8$Player <- "Rajon Rondo"
d9$Player <- "Chris Paul"
d10$Player <- "Andre Drummond"
d11$Player <- "Jeff Teague"
d12$Player <- "DeMar DeRozan"
d13$Player <- "Ricky Rubio"
d14$Player <- "Gordon Hayward"
d15$Player <- "Kemba Walker"

Full_Data <- rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15)
names(Full_Data)

cols_to_change_numeric <- c("G","GS","MP","FG","FGA","FG%","3P","3PA","3P%","2P","2PA","2P%","eFG%","FT","FTA","FT%","ORB","DRB","TRB","AST","STL","BLK","TOV","PF","PTS")

Full_Data <- Full_Data %>%
  mutate(Season = str_split(Season,"-") %>% map_chr(1) %>% as.numeric())

Full_Data <- Full_Data %>% 
  as_tibble() %>% 
  mutate(across(cols_to_change_numeric,as.numeric))
  
summary(Full_Data)

names(Full_Data) <- names(Full_Data) %>% make_clean_names()

write.csv(Full_Data,"Final_Tidy_Data.csv")

ggplot(Full_Data,aes(x=season)) + geom_point(aes(y=x3p_percent),color="Blue",alpha=.25) +
  theme_minimal() + facet_wrap(~player) +
  theme(axis.text.x=element_text(angle=60,hjust=1)) +
  labs(y= "Three Point %")

ggplot(Full_Data,aes(x=season)) + geom_point(aes(y=x3p_percent),color="Blue",alpha=.25) +
  geom_point(aes(y=x3pa),color="Red") + geom_point(aes(y=x3p),color="Green") +
  theme_minimal() + facet_wrap(~player,scales = "free_y") +  geom_smooth(aes(y=x3p_percent),method="lm") +
  theme(axis.text.x=element_text(angle=60,hjust=1)) +
  labs(y= "3-Pointers Taken, Made, and %")

ggplot(Full_Data,aes(x=season)) + geom_point(aes(y=x3p_percent),color="Blue",alpha=.25) +
  geom_point(aes(y=x3pa),color="Red") + geom_point(aes(y=x3p),color="Green") +
  theme_minimal() +  geom_smooth(aes(y=x3p_percent),method="lm") +
  theme(axis.text.x=element_text(angle=60,hjust=1)) +
  labs(y= "3-Pointers Taken, Made, and %")

mod1 <- glm(data=Full_Data,
            formula = x3p_percent ~ season * player)
summary(mod1)
tidy(mod1)
sqrt(mean(residuals(mod1)^2))
#0.0720

mod2 <- glm(data=Full_Data,
            formula = x3p_percent ~ season * player * x3pa)
sqrt(mean(residuals(mod2)^2))
#0.0586
summary(mod2)
tidy(mod2)

report(mod2)

ggplot(mod1,aes(x=season)) + geom_point(aes(y=x3p_percent),color="Blue",alpha=.25) +
  theme_minimal() + facet_wrap(~player) +  geom_smooth(aes(y=x3p_percent),method="lm") +
  theme(axis.text.x=element_text(angle=60,hjust=1)) +
  labs(y= "Three Point %",x="Season")

ggplot(mod2,aes(x=season)) + geom_point(aes(y=x3p_percent),color="Blue",alpha=.25) +
  theme_minimal() + facet_wrap(~player) +  geom_smooth(aes(y=x3p_percent),method="lm") +
  theme(axis.text.x=element_text(angle=60,hjust=1)) +
  labs(y= "Three Point %",x="Season")

ggplot(mod2,aes(x=season)) + geom_point(aes(y=x3p_percent),color="Blue",alpha=.25) +
  geom_point(aes(y=x3pa),color="Red") +
  theme_minimal() + facet_wrap(~player,scales = "free_y") +  geom_smooth(aes(y=x3p_percent),method="lm") +
  theme(axis.text.x=element_text(angle=60,hjust=1)) +
  labs(y= "3-Pointers Taken and Made",x="Season")

#TukeyHSD(mod1) %>% plot()


ggplot(Full_Data,aes(x=season)) + geom_point(aes(y=fga,color="Red"),alpha=.5) +
  geom_point(aes(y=fg),color="Green",size=1.5) + theme_minimal() + geom_smooth(aes(y=fg),method="lm") +
  theme(axis.text.x=element_text(angle=60,hjust=1)) + facet_wrap(~player) + geom_smooth(aes(y=fga),method="lm")+
  theme(legend.position="none") +
  labs(y= "Field Goals Made and Attempted per Season",x="Season")

mod_fg_percent <- glm(data=Full_Data,
            formula = fg_percent ~ season * player * fga)
sqrt(mean(residuals(mod_fg_percent)^2))
summary(mod_fg_percent)

add_predictions(Full_Data,mod1) %>% 
  ggplot(aes(x=season)) + geom_point(aes(y=x3p_percent),alpha=.5) +
  geom_point(aes(y=x3p_percent),color="Green",size=1.5) +
  geom_point(aes(y=pred),color="Red",size=2,alpha=.5) +
  facet_wrap(~player) +
  labs(y= "Three Point %",x="Season")

add_predictions(Full_Data,mod2) %>% 
  ggplot(aes(x=season)) + geom_point(aes(y=x3p_percent),alpha=.5) +
  geom_point(aes(y=x3p_percent),color="Green",size=1.5) +
  geom_point(aes(y=pred),color="Red",size=2,alpha=.5) +
  facet_wrap(~player) +
  labs(y= "Three Point %",x="Season")


#Adding Predictions####

df1 <- Full_Data %>% 
  add_predictions(mod2) 
df1[,c("x3p_percent","pred")] %>% head()

# Make a new dataframe with the predictor values we want to assess

players <- rep(unique(Full_Data$player),each=4)
length(players)
seasons <- rep(c(2021,2022,2023,2024),15)
seasons <- as.numeric(seasons)
class(seasons)
x3pa <- Full_Data

threepointattempt <- Full_Data %>% 
  dplyr::group_by(player) %>% 
  summarize(mean3pa = mean(x3pa,na.rm = TRUE))
threepa <- rep(threepointattempt$mean3pa,each=4)


newdf = data.frame(season = c(2021,2022,2023,2024),
                   player = players,
                   x3pa = threepa)

add_predictions(newdf,mod2) %>% 
  ggplot(aes(x=season,y=pred)) +
  geom_point() + geom_smooth(method="lm") + theme(axis.text.x=element_text(angle=60,hjust=1)) +
  facet_wrap(~player,scales = "free") + 
  labs(y= "Three Point % Predictions",x="Future Seasons")
                    
# anything specified in the model needs to be here with exact matching column names

Full_Data$player %>% unique()



# making predictions
#pred = predict(mod2, newdata = newdf)

# combining hypothetical input data with hypothetical predictions into one new data frame
#hyp_preds <- data.frame(disp = newdf$x3p_percent,
                    #    pred = pred)

# Add new column showing whether a data point is real or hypothetical
#df1$PredictionType <- "Real"
#hyp_preds$PredictionType <- "Hypothetical"

# joining our real data and hypothetical data (with model predictions)
#fullpreds <- full_join(df1,hyp_preds)

#fullpreds

#ggplot(fullpreds,aes(x=disp,y=pred,color=PredictionType)) +
#  geom_point() +
 # geom_point(aes(y=mpg),color="Black") +
  #theme_minimal()



#gather_predictions(Full_Data,mod2) %>% 
 # ggplot(aes(x=season)) + geom_point(aes(y=x3p_percent,color="Blue"),alpha=.4) +
  #geom_point(aes(y=pred),color="Green",alpha=.6) + 
  #geom_smooth(aes(y=x3p_percent),method="lm") +
  #facet_wrap(~model,data=hyp_data1)


# 
mod3 <- glm(data=Full_Data,
            formula = x3p_percent ~ player * mp + season)
summary(mod3)
sqrt(mean(residuals(mod3)^2))
#0.0698

ggplot(mod3,aes(x=season)) + geom_point(aes(y=mp),color="Blue",alpha=.25) +
  theme_minimal() + facet_wrap(~player) +  geom_smooth(aes(y=mp),method="lm") +
  theme(axis.text.x=element_text(angle=60,hjust=1)) +
  labs(y= "Minutes per Game", x="Season")

mod4<- glm(data=Full_Data,
                formula = x3p_percent ~ player * pos * season)
summary(mod4)

sqrt(mean(residuals(mod4)^2))
#0.0716
#####
saveRDS(Full_Data, "./full_cleaned_project_data.RDS")

finished

