#-------------------
# Intro to R script
#-------------------
# Libraries I need to use for this project
# I already downloaded these and don't need to do that again
library(dplyr)
library(ggplot2)
library(gridExtra)

# Start up a new R Project (or open an existing one)
# Now R is looking there
# I can import the data
compensation <- read.csv("compensation.csv")

# -----------------------------------------
# Checking the data 
# -----------------------------------------
dim(compensation) # The dimensions of the data (i.e., the number of rows and columns) 
names(compensation)
head(compensation) # First 6 lines
str(compensation) # The structure of the data (very useful)
tail(compensation)

# some dplyr functions for checking data
tbl_df(compensation)
glimpse(compensation)

# -----------------------------------------
# Subsetting with dplyr
# -----------------------------------------

# select is all about columns
select(compensation, Root, Fruit)
select(compensation, -Grazing)

# slice to get rows 
slice(compensation, 2)
slice(compensation, 2:5)
slice(compensation, c(2,5))

# piping
magicNumber <- compensation %>%
  select(Fruit) %>%
  slice(8)
magicNumber

# filter
fruit80 <- filter(compensation, Fruit > 80)
select(fruit80, Root)

fruit80_RootVals <- compensation %>%
  filter(Fruit>80) %>%
  select(Root)

filter(compensation, Grazing != "Ungrazed")

# mutate adds columns
compensation <- mutate(compensation, RFratio = Root/Fruit)
head(compensation)

# sorting using arrange
arrange(compensation, Grazing, Fruit)
arrange(compensation, Grazing, desc(Fruit)) # descending order

# summarisation
compensation %>% 
  summarise(
    meanFruit = mean(Fruit),
    sdFruit = sd(Fruit))

# grouping via piping
summaryFruit <- compensation %>% 
  group_by(Grazing) %>%
  summarise(
    meanFruit = mean(Fruit),
    sdFruit = sd(Fruit))

# not piping
summarise(
  group_by(compensation, Grazing),
  meanFruit = mean(Fruit),
  sdFruit = sd(Fruit)) 

# string together several functions
# Ratio column in raw data is lost
compensation %>%
  mutate(FRrat=Fruit/Root) %>%
  group_by(Grazing) %>%
  summarise(
    meanFRrat = mean(FRrat))

# no pipe.  Ratio column retained
compensation <- mutate(compensation, FRrat = Fruit/Root)
compensation %>%
  group_by(Grazing) %>%
  summarise(
    meanFRrat = mean(FRrat),
    sdFRrat = sd(FRrat))

#--------------------------------
# Plotting with ggplot
#-----------------------------

## First plot the data - the basic layers
ggplot(compensation, aes(x=Root, y=Fruit)) +
  geom_point()

# Enhance the plot - add colour, alpha, labels
# get rid of grey and make the axis font bigger
# (using rel(4))
ggplot(compensation, aes(x=Root, y=Fruit)) +
  
  # this customises the point colours etc
  geom_point(colour = "blue", alpha = 0.5, size=5) +
  
  # these are the axis lables
  labs(x="Root Width (mm)", y="Fruit (kg)") +
  
  # this is a quick theme adjustment
  theme_bw(base_size=15)

# adjust the colours AND shapes 
# axis limits
# try a different theme
# use specific base_size for axis label sizes
ggplot(compensation, aes(x=Root, y=Fruit, colour = Grazing, shape = Grazing))+
  geom_point(size=5) +
  scale_colour_manual(values = c("brown",'green'))+
  # ylim(0,125) + xlim(0,10)+
  scale_x_continuous(breaks = c(2:10))+
  labs(x="Root Width (mm)", y="Fruit (kg)") +
  scale_colour_manual(values = c("brown","green")) +
  scale_x_continuous(breaks = 2:10) +
  theme_bw(base_size = 15)

# -----------------------------------------
# SNIPPET OF CODE TO ADJUST THEME - Customisation Central
# -----------------------------------------

# get rid of all theme background,
# and modify axis text size and colour

ggplot(compensation, aes(x=Root, y = Fruit, colour = Grazing)) +
  geom_point(size = 5, alpha = 0.5) +
  ylab("Fruit (kg)") + xlab("Root (mm)") +
  scale_colour_manual(values = c("cornflowerblue","darkgreen")) +
  scale_x_continuous(breaks = 2:15) +
  ggtitle("a") +
  theme(
    # get rid of grid and colour background
    # fill with nothing, black border
    panel.background = element_rect(fill = NA, colour = 'black'),
    panel.grid = element_blank(),
    
    # adjust the x & y axis details
    axis.title.x=element_text(size=rel(2), angle = -45, colour = 'red'),
    axis.title.y=element_text(size=20, colour = 'cornflowerblue'),
    
    # adjust the axis tick label size, rotation, and position
    axis.text.x = element_text(size = rel(2), angle = 45, vjust = -1),
    
    # get rid of the legend title (make it blank)
    legend.title=element_blank(),
    
    # get rid of boxes around points in legend
    legend.key=element_rect(fill=NA),
    
    # title justify left
    plot.title = element_text(hjust = 0))

# -------------------------------
# more than one plot on the page
#--------------------------------

## First plot the data - the basic layers
p1 <- ggplot(compensation, aes(x=Root, y=Fruit)) +
  geom_point(size = 10, colour = 'cornflowerblue', alpha = 0.8)+
  ylab("Fruit Production (kg)")+
  xlab("Root Stock Width (mm)")+
  theme_bw(base_size = 15)

# how to manage groups
p2 <- ggplot(compensation, aes(x=Root, y=Fruit, colour = Grazing)) +
  geom_point(size = 10, alpha = 0.8)+
  ylab("Fruit Production (kg)")+
  xlab("Root Stock Width (mm)")+
  scale_colour_manual(values = c('brown', 'green'))+
  theme_bw(base_size = 15)

# we can use gridExtra to make a figure of figures.
# the function is grid.arrange() which takes
# figures and column/row design

grid.arrange(p1, p2, ncol = 2)

# -----------------------------------------
# histograms and facets
# -----------------------------------------
ggplot(compensation, aes(x=Fruit)) +
  geom_histogram(binwidth = 20, fill = 'red', alpha = 0.5) +
  facet_wrap(~Grazing, ncol = 1)+
  theme_bw()


ggplot(compensation, aes(x=Fruit)) +
  geom_histogram(binwidth = 20, fill = 'red', alpha = 0.5) +
  facet_grid(Grazing~)+
  theme_bw()

# -----------------------------------------
# box-whisker
# fill in the geom makes one colour change for all boxes
# fill in the aes makes colours correspond to levels of a factor
# -----------------------------------------
ggplot(compensation, aes(x=Grazing, y = Fruit, fill = Grazing)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(values = c('steelblue','deeppink'))+
  theme_bw(base_size = 15)

# -----------------------------------------
# points with errorbars
# -----------------------------------------

# step 1 - get all summary stats needed
# mean and SE
sumDat <- compensation %>%
  group_by(Grazing) %>%
  summarise(
    meanFruit = mean(Fruit),
    seFruit = sd(Fruit/sqrt(n())))

# Step 2 - 4 lines of code for a magnificent figure
ggplot(sumDat, aes(x = Grazing, y = meanFruit, colour = Grazing))+
  geom_point(size = 5)+
  geom_errorbar(aes(ymin = meanFruit - seFruit, ymax = meanFruit + seFruit), width = 0.1)+
  theme_bw(base_size = 20)

# -----------------------------------------
# points, se, lines
# -----------------------------------------
sumDat<-summarise(group_by(compensation, Grazing),
                  meanFruit = mean(Fruit),
                  seFruit = sd(Fruit)/sqrt(n()))

# or using piping to do the same thing....
sumDat <- compensation %>%
  group_by(Grazing) %>%
  summarise(
    meanFruit = mean(Fruit),
    seFruit = sd(Fruit)/sqrt(n()))


ggplot(sumDat, aes(x = Grazing, y = meanFruit, group = 1))+
  geom_point(size = 5, aes(colour = Grazing))+
  geom_line()+
  geom_errorbar(aes(ymin = meanFruit - seFruit, ymax = meanFruit + seFruit), width = 0.1)+
  
  # we can use expression() and paste() to make fancy labels
  # using ?plotmath
  ylab(expression(paste("Mean Fruit Production (", kg^2, ")")))+
  xlab(expression(paste("Grazing ", (over(Cows,ha)))))+
  
  # customise the colours
  scale_colour_manual(values = c('darkgrey','darkgreen'))+
  theme_bw(base_size = 15)

# -----------------------------------------
# BIG error bar and points 
# Multiple groups
# Bovine diets
# -----------------------------------------
growth <- read.csv("growth.csv")

# Get summary data to get means and standard errors
sumGrow <- 
  growth %>%
  group_by(supplement, diet) %>%
  summarise(meanG = mean(gain),
            seG = sqrt(var(gain)/n()))

# ggplot using sumGrow not growth
ggplot(sumGrow, aes(x = supplement, y = gain, col = diet, group = diet)) +
  geom_point(size = 2) +
  geom_line() +
  # add the error bars
  # max = the mean + standard error
  # min = mean - standard error
  geom_errorbar(aes(ymin = meanG - seG, ymax = meanG + seG)) +
  theme_bw()

# -----------------------------------------
# barplot with errorbars (avoid if you can! Barcharts suck!)
# -----------------------------------------

# step 1 - get all summary stats needed
sumDat<-summarise(group_by(compensation, Grazing),
                  meanFruit = mean(Fruit),
                  sdFruit = sd(Fruit),
                  nFruit = length(Fruit),
                  lwr = meanFruit - sdFruit,
                  upr = meanFruit + sdFruit)


# step 2 create object limits for bar aesthetics
limits<-aes(ymin = lwr, ymax = upr)

# step 3: build layers and use limits from above
# stat ="identity" means USE THE MEAN WE CALCUATED... 
# don't do any maths for me
ggplot(sumDat, aes(x=Grazing, y=meanFruit, fill=Grazing))+
  geom_bar(stat = 'identity') +
  geom_errorbar(limits, width = 0, size = 2)+
  # geom_point(data = compensation, 
  # aes(x = Grazing, y = Fruit), size = 5, alpha = 0.3)
  # now we mak it pretty in our own special way
  scale_fill_manual(values = c("chocolate4","darkgreen"))+
  theme_bw()

# -----------------------------------------
# Emojis
# -----------------------------------------

library(devtools)
install_github('dill/emoGG@new-ggplot2')

library(emoGG)
emoji_search('turtle')

ggplot(compensation, aes(x=Root, y = Fruit))+
  geom_emoji(emoji = '1f422')



