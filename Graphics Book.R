#R Graphics

install.packages("gcookbook")

#Chapter 1
library(ggplot2)
library(gcookbook)
library(RColorBrewer)
library(tidyverse)


#qplot is the base plot() indeed
qplot(mtcars$mpg,mtcars$wt)
qplot(pressure$temperature, pressure$pressure, geom = "line")
ggplot(pressure, aes(temperature, pressure)) + geom_point() + geom_line()

# Convert the x variable to a factor, so that it is treated as discrete
# Do not forget the stat = identity
# qplot is enough for bar graph individually
qplot(factor(BOD$Time), BOD$demand, geom = "bar", stat = "identity")
ggplot(BOD, aes(x = factor(Time), y = demand)) + geom_bar(stat = "identity")


#Histogram
ggplot(mtcars, aes(mpg)) + geom_histogram(binwidth = .5)

#Boxplot
ggplot(ToothGrowth, aes(supp, y = len)) + geom_boxplot() 
ggplot(ToothGrowth, aes(interaction(supp , dose), y = len)) + geom_boxplot()

#curve
randomfun <- function(xvar){
  1/(1 + exp(-xvar + 100))
}
# This sets the x range from 0 to 20
ggplot(data.frame(x = c(0,200)), aes(x = x)) + stat_function(fun = randomfun, geom = "line")
ggplot(data.frame(c(0,200)), aes(c(1,200))) + stat_function(fun = randomfun, geom = "line")




#Chapter 2
#In bar graphs be aware of counts or values (Very important) 
#because for values we should type stat = "identity" or we simply can use geom_col()
#geom_bar() uses stat_count() by default, so in geom_bar there is no y aesthetic
#geom_col is for values, geom_bar is for counts
#by using  factor with continuous x we omit the empty column in our graphs
ggplot(BOD, aes(x=Time, y=demand)) + geom_bar(stat="identity")
ggplot(BOD, aes(x=factor(Time), y=demand)) + geom_bar(stat="identity")


#outline with COLOUR and color with FILL
ggplot(pg_mean, aes(x = group, y = weight)) + 
  geom_bar(stat = "identity", fill = "lightblue3", colour = "black")


#Map the second categorical variable in FILL and use position = DODGE for bars next to each other
ggplot(cabbage_exp, aes(Date, Weight, fill = Cultivar)) + 
  geom_bar(stat = "identity", position = "dodge")
#without DODGE they are on each other (Stacked bar plot)
ggplot(cabbage_exp, aes(Date, Weight, fill = Cultivar)) + 
  geom_bar(stat = "identity")


#some beautiful colors
ggplot(cabbage_exp, aes(Date , Weight, fill = Cultivar)) + 
  geom_bar(position = "dodge", stat = "identity", colour = "black") + 
  scale_fill_brewer(palette = "pastel1")


#making a bar graph with counts (=no y column) just nothing or stat = "bin"
ggplot(diamonds, aes(x=cut)) + geom_bar()

#Bar graph of counts on a continuous axis, also known as a histogram
#Note that setting occurs outside of aes(), while mapping occurs within aes():
#Option 1 - you can use the aesthetic to reflect some properties of your data. 
#Option 2 - you can choose a certain value for an aesthetic. 
#For example, make the colour blue for ALL points or make the shape a square for ALL points. 
#This is called SETTING an aesthetic and the keyword here is ALL.
#colors
ggplot(uspopchange, aes(x=Abb, y=Change, fill=Region)) + 
  geom_bar(stat="identity")

ggplot(uspopchange, aes(x=reorder(Abb, Change), y=Change, fill=Region)) +
  geom_bar(stat="identity", colour="black") +
  scale_fill_manual(values=c("#669933", "#FFCC66","526261", "667865")) +
  xlab("State")

#Different color with positive and negative values
#We use logical values in the FILL 
csub <- subset(climate, Source=="Berkeley" & Year >= 1900) #or use filter
csub$pos <- csub$Anomaly10y >= 0
head(csub)
ggplot(csub, aes(x = Year, y = Anomaly10y, fill = pos)) +
  geom_bar(stat = "identity")

#It is better to remove the legend because it is TRUE and FALSE,Therefore with guide = FALSE
#WIDTH for narrower(or Wider) and SIZE for size of outline respectively
ggplot(csub, aes(x = Year, y = Anomaly10y, fill = pos)) + 
  geom_bar(stat = "identity", colour = "black", size = 0.5) + 
  scale_fill_manual(values = c("#CCEEFF", "#FFDDDD"), guide = F)


#Space between the bars of the dodge with POSITION_DODGE()
#make width smaller and set the value for position_dodge to be larger than width
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity", width=0.5, position=position_dodge(0.7))

#for Stacking bar to be appropriate with the legend: use DESC
library(dplyr) # Needed for desc()
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar, order=desc(Cultivar))) +
  geom_bar(stat="identity")



#100% stacked bar graph
#just position = "fill"
ggplot(cabbage_exp, aes(Date, Weight, fill = Cultivar)) + 
  geom_bar(stat = "identity", position = "fill")


#Add labels
ggplot(cabbage_exp, aes(interaction(Date, Cultivar), y = Weight)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = Weight), vjust = 1.5, colour = "white")

#position_dodge for the text makes the texts to be parallel to each other than on the other
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=Weight), vjust=1.5, colour="white",
            position=position_dodge(.9), size=3)


tophit <- tophitters2001[1:25, ]
tophit
#character vector are ordered alphabetically. 
#If it was a factor type, it would use the order which is defined in the factor levels. 
#In this case, we want name to be sorted by a different variable, avg.
ggplot(tophit, aes(x=avg, y=reorder(name, avg))) +
  geom_point(size=3)


#use rotation (x and y axis are now replaced)
ggplot(tophit, aes(x=reorder(name, avg), y=avg)) +
  geom_point(size=3) + # Use a larger dot
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(colour="grey60", linetype="dashed"))


#Unfortunately,
#the reorder() function will only order factor levels by one other variable;
#to order the factor levels by two variables, we must do it manually
nameorder <- tophit$name[order(tophit$lg, tophit$avg)]
# Turn name into a factor, with levels in the order of nameorder
tophit$name <- factor(tophit$name, levels=nameorder)

#geom_segment() : lines up to that point
ggplot(tophit, aes(x= avg, y = name)) + 
  geom_segment(aes(yend = name), xend = 0, colour = "grey50") + #yend is in aes()
  geom_point(aes(colour = lg), size = 3) +
  scale_color_brewer(palette = "Set1", limits = c("NL", "AL"), guide = F) +#guide = F b/c use facet 
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  facet_grid(lg ~ ., scales="free_y", space="free_y")
  





#Chapter3
#Line graph is using more when there is 2 continous axix (or one with order discrete)
#When the x variable is a factor, you must also use aes(group=1) to ensure that ggplot() knows
#that the data points belong together and should be connected with a line
#if you don't mention the group = 1, it throws an error
BOD1 <- BOD # Make a copy of the data
BOD1$Time <- factor(BOD1$Time)
ggplot(BOD1, aes(x=Time, y=demand, group=1)) + geom_line()

#for including 0 : Wheather expand_limits() or ylim(0, max(demand))
ggplot(BOD1, aes(Time, demand, group = 1)) + geom_line() + expand_limits(0)


#map another discrete variable into COLOUR or LINETYPE (*in bar plot it was FILL)
#If the x variable is a factor, you must also tell ggplot() to group by colour variable
#in Overall when the x is factor use the group whether with colour or 1
#Colour and 1 for group are different
ggplot(tg, aes(factor(dose), y = length, colour = supp, group = supp)) + geom_line()

#line and point together
ggplot(tg, aes(x=dose, y=length, colour=supp)) + geom_line() +
  geom_point(size=4, shape=21)

#dodge the overlap points and lines
ggplot(tg, aes(dose, length, shape = supp)) + #shape used here
  geom_line(position = position_dodge(0.2)) + geom_point(position = position_dodge(.2))

ggplot(tg, aes(dose, length, colour = supp)) + geom_line() + 
  scale_color_brewer(palette = "Set1")

ggplot(tg, aes(dose, length, colour = supp)) + 
  geom_line(linetype = "dashed") + 
  geom_point(shape = 25, size = 7, fill = "white", colour = "darkred")

ggplot(tg, aes(dose, length, colour = supp)) + 
  geom_line(position = position_dodge(.2)) + 
  geom_point(shape = 21, size = 3, position = position_dodge(.2)) + 
  scale_fill_manual(values = c("black", "white"))

# Convert the sunspot.year data set into a data frame for this example
sunspotyear <- data.frame(
  Year = as.numeric(time(sunspot.year)),
  Sunspots = as.numeric(sunspot.year)
)
head(sunspotyear)

#geom_area()
ggplot(sunspotyear, aes(x=Year, y=Sunspots)) + geom_area()

ggplot(sunspotyear, aes(x = Year, y=Sunspots)) + 
  geom_area(colour="tan2", fill="white", alpha =.2)
colors()

#Stacked area graph: use FILL in the aes
#ggplot requires the data to be in long format(often in converse of the stacked area graph)
ggplot(uspopage, aes(x= Year, y=Thousands, fill = AgeGroup)) + geom_area()


#reverse the order of legends with BREAKS and REV
ggplot(uspopage, aes(Year, Thousands, fill = AgeGroup)) + 
  geom_area(colour = "black", size = .2, alpha = .3) + 
  scale_fill_brewer(palette = "Blues", breaks = rev(levels(uspopage$AgeGroup)))


#confidence interval with geom_ribbon()
clim <- climate %>% filter(Source == "Berkeley") %>% select(Year, Anomaly10y, Unc10y)
head(clim)
ggplot(clim, aes(Year, Anomaly10y)) + 
  geom_ribbon(aes(ymin = Anomaly10y - Unc10y, ymax = Anomaly10y + Unc10y), alpha = .4) + 
  geom_line()

#Or with 2 geom_line()
ggplot(clim, aes(x=Year, y=Anomaly10y)) +
  geom_line(aes(y=Anomaly10y-Unc10y), colour="grey50", linetype="dotted") +
  geom_line(aes(y=Anomaly10y+Unc10y), colour="grey50", linetype="dotted") +
  geom_line()







#Chapter 5
#Scatter plots

#Scatter plots are used to display the relationship between two continuous variables
ggplot(heightweight, aes(ageYear, heightIn)) + geom_point(shape = 21, size = 1.5)

#shape 19 is better than 16 in overall(b/c formats like PNG)
#group by COLOUR or SHAPE the categorical variable(factor or character)
ggplot(heightweight, aes(ageYear, heightIn, colour = sex)) + geom_point()


ggplot(heightweight, aes(ageYear, heightIn, colour = sex, shape = sex)) + 
  geom_point() + 
  scale_shape_manual(values = c(6,4)) + 
  scale_color_brewer(palette = "Set1")

#Overall change of shapes in geom_point()
#Some of the point shapes:
#(1-14) have just an outline
#some (15-20) are solid 
#and some (21-25) have an outline and fill that can be controlled separately.
#For shapes 21-25, the outline is controlled by colour and the fill is controlled by fill.

#for 21-25 shapes we can use SHAPE for one variable and FILL for another (2 Categorical Variable)
hw <- heightweight %>% mutate(weighttt = weightLb > 100)
head(hw)

ggplot(hw, aes(x=ageYear, y=heightIn, shape=sex, fill=weighttt)) +
  geom_point(size=2.5) +
  scale_shape_manual(values=c(21, 24)) +
  scale_fill_manual(values=c(NA, "black"),
                    guide=guide_legend(override.aes=list(shape=21)))

#Map the continuous variable to SIZE or COLOUR
ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=weightLb)) + geom_point()

ggplot(heightweight, aes(x=ageYear, y=heightIn, size=weightLb)) + geom_point()

#So when you use the shape > 20 then use the FILL in aes
ggplot(heightweight, aes(ageYear, heightIn, fill = weightLb)) + 
  geom_point(size = 2.5, shape = 21) + 
  scale_fill_gradient(low = "black", high = "white")
#Using guide_legend() will result in a discrete legend instead of a colorbar
ggplot(heightweight, aes(x=weightLb, y=heightIn, fill=ageYear)) +
  geom_point(shape=21, size=2.5) +
  scale_fill_gradient(low="black", high="white", breaks=12:17,
                      guide=guide_legend())
#third and fourth variable can be categorical and continuous
ggplot(heightweight, aes(ageYear, heightIn, color = sex, size = weightLb )) + 
  geom_point(alpha= .5) +
  scale_color_brewer(palette = "Set1")


#bin: overplotting solution
sp <- ggplot(diamonds, aes(x=carat, y=price))
sp + stat_bin2d()  

sp + stat_bin2d(bins = 50) + 
  scale_fill_gradient(low = "lightblue", high = "red", limits = c(0,6000))

library(hexbin)
sp + stat_binhex() +
  scale_fill_gradient(low="lightblue", high="red",
                      limits=c(0, 8000))

#jitter: overplotting solution
sp1 <- ggplot(ChickWeight, aes(x=Time, y=weight))
sp1 + geom_point(position="jitter")

#boxplot
sp1 + geom_boxplot(aes(group=Time))


#regression
#To add a linear regression line to a scatter plot, add stat_smooth() and tell it to use
#method=lm.
sp <- ggplot(heightweight, aes(x=ageYear, y=heightIn))

sp + geom_point() +
  geom_smooth(method = "lm", level = .99)

#Don't forget the geom_point()
#Without CI
sp + geom_point() + 
  geom_smooth(method = "lm", se = F)

#You can use colour and size for geom_smooth and also default of method is loess
sp + geom_point(colour="grey60") + stat_smooth(colour = "green", size = 1)


#Logistic Regression
library(MASS)
biopsy$classn[biopsy$class=="benign"] <- 0
biopsy$classn[biopsy$class=="malignant"] <- 1

head(biopsy)
ggplot(biopsy, aes(V1, classn)) + 
  geom_point(size = 1.5, shape =21, position = "jitter", alpha =.2) + 
  geom_smooth(method = "glm" , method.args = list(family = "binomial"))

sps <- ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex)) +
  geom_point() +
  scale_colour_brewer(palette="Set1")
sps + geom_smooth(se = F)


#for extrapolation: use lm and fullrange = TRUE
sps + geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
