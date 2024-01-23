# R Project Script - Jordan Wells

# set the working directory to where the folder was saved on your machine (replace with the dots)
setwd("...")


StudentComparisons <- read_sav("Comparison_Students.sav")
StarStudents <- read_sav("STAR_Students.sav")
K_3 <- read_sav("STAR_K-3_Schools.sav")
HighSchool <- read_sav("STAR_High_Schools.sav")

## packages
library(tidyr)
library(dplyr)
library(ggplot2)

### removes not needed data
newStuedntCompare <- subset(StudentComparisons, select = -c(birthday,birthmonth))
newStuedntCompare <- newStuedntCompare %>% drop_na

## specifies students that were in star for 4 years, in small classes all 4 yrs and have data for grades k - high school and have taken act/sat -- base line for whether star was effective
TrueStarStudents <- StarStudents[ which (StarStudents$yearsstar == 4 & StarStudents$yearssmall == 4 & StarStudents$flagg1 == 1 & StarStudents$flagg2 == 1 & StarStudents$flagg3 == 1 & StarStudents$flagg4 == 1 & StarStudents$flagg5 == 1 & StarStudents$flagg6 == 1 & StarStudents$flagg7 == 1 & StarStudents$flagg8 == 1 & StarStudents$flaggk == 1 & StarStudents$flagsatact == 1 & StarStudents$flagprt4 == 1 & StarStudents$flagidn8 == 1 & StarStudents$flagprt8 == 1 & StarStudents$flaghscourse == 1), ]
TrueStarStudents <- subset(TrueStarStudents, select = -c(gkpresent))

## specifies STAR students of 4 yrs without any small classes
regularStudents <- StarStudents
regularStudents <- StarStudents[ which (StarStudents$yearssmall == 0 & StarStudents$yearsstar == 4 ) ,]

## removes not needed data
K_3 <- subset(K_3, select = -c(GKOTHRAC,GKASIAN, GKNATVAM, GKHSPANC, G1OTHRAC,G1ASIAN, G1NATVAM, G1HSPANC, G2OTHRAC,G2ASIAN, G2NATVAM, G2HSPANC, G3OTHRAC,G3ASIAN, G3NATVAM, G3HSPANC))

## specifies graduation requirements = state requirements  
newHighschool <- HighSchool[ which (HighSchool$MINRQMNT == 1 ), ]

############################################################
## used for comparison of test scores for STAR vs regular###
############################################################

## sets conditions for both cases: teacher experience is 10 or more years, child absence is less than 40 days, no special education status
test1 <- TrueStarStudents[which (TrueStarStudents$gktyears  == 17 & TrueStarStudents$gkabsent < 40 & TrueStarStudents$gkspeced == 2 ) ,] 
test2 <- regularStudents[which ( regularStudents$gktyears == 17  & regularStudents$gkabsent < 40 & regularStudents$gkspeced == 2 ) ,] 

## finds the results on average of students who's teachers have equivalent experience and students of similar absence rates who belong to class sizes less than  20
summarize( test1, mean(test1$gktmathss))
summarize( test2, mean(test2$gktmathss))

summarize( test1, mean(test1$gktreadss))
summarize( test2, mean(test2$gktreadss))

#########################
##visual representation##
#########################

plotdata <- data.frame( 
  TrueStarStudents$gkclasssize , 
  TrueStarStudents$gktmathss
)
plotdata2 <- data.frame( 
  regularStudents$gkclasssize , 
  regularStudents$gktmathss
)

plotdata2 <- regularStudents %>% group_by(gkclasssize) %>% 
  summarize ( average = mean(gktmathss) )

plotdata <- TrueStarStudents %>% group_by(gkclasssize) %>% 
  summarize ( average = mean(gktmathss) )

plotdata3 <- TrueStarStudents %>% group_by(gkclasssize) %>% 
  summarize ( average = mean(gktreadss) )

plotdata4 <- regularStudents %>% group_by(gkclasssize) %>% 
  summarize ( average = mean(gktreadss) )

plotdata <- na.omit(plotdata)
plotdata2 <- na.omit(plotdata2)
plotdata3 <- na.omit(plotdata3)
plotdata4 <- na.omit(plotdata4)

## as indicated by the charts, the average test scores of smaller classes are higher than those of larger classes

plot <- ggplot() + 
  geom_line(data = plotdata, aes(x = gkclasssize, y= average), color = "red") + geom_line(data = plotdata2, aes(x = gkclasssize, y= average), color = "blue") + geom_line(data = plotdata3, aes(x = gkclasssize, y= average), color = "green") + geom_line(data = plotdata4, aes(x = gkclasssize, y= average), color = "yellow") 
plot <- plot + labs( x = "class size" , y = "avearge test score")

plot <- ggplot() + 
  geom_line(data = plotdata, aes(x = gkclasssize, y = average), color = "red") + 
  geom_line(data = plotdata2, aes(x = gkclasssize, y = average), color = "red") + 
  geom_line(data = plotdata3, aes(x = gkclasssize, y = average), color = "blue") + 
  geom_line(data = plotdata4, aes(x = gkclasssize, y = average), color = "blue") +
  geom_text(data = plotdata %>% filter(gkclasssize == max(gkclasssize)), 
            aes(label = "Math", x = max(gkclasssize), y = max(average)),
            color = "red", vjust = 6.5, hjust = -5) +  
  geom_text(data = plotdata3 %>% filter(gkclasssize == max(gkclasssize)), 
            aes(label = "Reading", x = max(gkclasssize), y = max(average)),
            color = "blue", vjust = 2.5, hjust = -3) +  
  labs(x = "Class Size", y = "Average Test Score")

plot

################################################
##regressions for class size & math test scores#
################################################

combined_data <- bind_rows(
  mutate(plotdata2, dataset = "plotdata2"),
  mutate(plotdata, dataset = "plotdata")
)

combined_model <- lm(average ~ gkclasssize + dataset, data = combined_data)
summary( combined_model )

#regression for teacher experience & average test scores
plotdata2 <- regularStudents %>% group_by(gktyears) %>% 
  summarize ( average = mean(gktmathss) )

plotdata <- TrueStarStudents %>% group_by(gktyears) %>% 
  summarize ( average = mean(gktmathss) )

combined_data <- bind_rows(
  mutate(plotdata2, dataset = "plotdata2"),
  mutate(plotdata, dataset = "plotdata")
)

combined_model <- lm(average ~ gktyears + dataset, data = combined_data)
summary( combined_model )



#as seen by the p-values of both regression, class size is a better indicator of test score compared to teacher experience.
