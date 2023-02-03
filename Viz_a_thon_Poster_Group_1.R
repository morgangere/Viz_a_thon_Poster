###########################################################
#
# Author: Morgan Gere
# Purpose: Viz-a-thon
# Uses: BuoyData_2_2.xlsx
#
##########################################################

# Reading in the data
buoy <- read.csv("C:/Users/Morga/OneDrive/Desktop/ISchool/IST719_Information_Visualization/Viz_a_thon/Viz_a_thon_Data.csv"
                 ,header = T
                 ,stringsAsFactors = F)

# viewing the data
View(buoy)

############################### Data exploration
#looking at the dimensions of the data
dim(buoy)
#checking the column names
colnames(buoy)
# looking at the structure of the data set
str(buoy)
# looking at the first 6 rows
head(buoy)




#################################### Data Cleaning

# Checking for NA's in the data
table(is.na(buoy))
#finding which columns have the NA Values
names(which(colSums(is.na(buoy))>0))

##exploring the ph na values
#first finding how many
table(is.na(buoy$pH))
#finding the mean without the na
ph.avg <- mean(buoy$pH,na.rm = T)
#finding the range
range(buoy$pH,na.rm = T)
# replacing NA values with the mean
buoy$pH[is.na(buoy$pH)] <- ph.avg
#checking the pH
table(is.na(buoy$pH))

## Checking the number of na
table(is.na(buoy$Dox_mg_L))
#checking the mean of Dox
dox.avg <- mean(buoy$Dox_mg_L,na.rm = T)
#finding the range
range(buoy$Dox_mg_L,na.rm = T)
#replacing NA values with the mean
buoy$Dox_mg_L[is.na(buoy$Dox_mg_L)] <- dox.avg
#checking the pH
table(is.na(buoy$Dox_mg_L))

## Checking the number of na
table(is.na(buoy$Tn_Ntu))

## Checking the number of na
table(is.na(buoy$Chl_ug_L))

#removing the rows with NA values
buoy <- na.omit(buoy)

## Checking the number of na
table(is.na(buoy$Tn_Ntu))

## Checking the number of na
table(is.na(buoy$Chl_ug_L))

#changing the datetime for use later
buoy$Date <- as.POSIXct(strptime(buoy$ï..DATE_TIME, '%m/%d/%Y %H:%M',tz='GMT'))

# creating a year column
buoy$year <- strptime(buoy$ï..DATE_TIME, '%m/%d/%Y %H:%M',tz='GMT')$year+1900
## Checking the number of na
table(is.na(buoy$Date))

###################################### One dimension plots

#when the data was taken
ggplot(buoy)+aes(Date,fill = factor(year))+geom_histogram()+
  scale_x_datetime(breaks = "month")+
  theme(axis.text.x = element_text(angle = 90))



# finding the density of cholrophyll-a
d <- density(buoy$Chl_ug_L)
plot(d)
# coloring the visualization of density 
polygon(d,col = "orange")

## ggplot density of cholophyll-a
ggplot(buoy)+ aes(x = Chl_ug_L,fill = "oragne") + geom_density()

#one dimension plot of oxygen distribution

plot(sort(buoy$Dox_mg_L,decreasing=T))

# Boxplot
boxplot(sort(buoy$DEPTH_m))

plot(sort(buoy$DEPTH_m,decreasing = T))



############################################## Two dimension plots
#################################### Trophic Classifications based on Chlorophyll-a over time
View(buoy)

# aggregating chlorophyll by each measurement
buoy.mean.chl<-aggregate(buoy$Chl_ug_L,list(buoy$Date),mean)
#changing column names
colnames(buoy.mean.chl)<- c("Date","Chl_ug_L")

#creating trophic column type
buoy.mean.chl$type <- ifelse(buoy.mean.chl$Chl_ug_L < 2.6, "Hipotrophic",
                             ifelse(buoy.mean.chl$Chl_ug_L > 2.6 & buoy.mean.chl$Chl_ug_L < 7.3, "Mesotrophic",
                                    ifelse(buoy.mean.chl$Chl_ug_L >7.3 & buoy.mean.chl$Chl_ug_L < 56,"Eutrophic","Hypertrophic")))

# plotting trophic classifications based on chlorphyll-a
ggplot(buoy.mean.chl)+aes(y=Chl_ug_L,x=Date,color=type)+geom_point()+
  scale_x_datetime(breaks = "month")+
  theme(axis.text.x = element_text(angle = 90))

################################################ Ploatting oxygen vs time and depth
buoy$Dox_mg_L
# 0-2 dead zone
# 5 required for warrm water fish
# 7 required for cold water fish

# aggregating chlorophyll by each measurement
buoy.mean.oxy<-aggregate(buoy$Dox_mg_L,list(buoy$Date),mean)
#changing column names
colnames(buoy.mean.oxy)<- c("Date","Dox_mg_L")

#creating trophic column type
buoy.mean.oxy$type <- ifelse(buoy.mean.oxy$Dox_mg_L < 2, "Deadzone",
                             ifelse(buoy.mean.oxy$Dox_mg_L > 2 & buoy.mean.oxy$Dox_mg_L < 5, "Non-fish-supporting",
                                    ifelse(buoy.mean.oxy$Dox_mg_L >5 & buoy.mean.oxy$Dox_mg_L < 7,"Supports-warm-water-fish","Supports-both-warm-and-cold-water-fish")))

# plotting trophic classifications based on chlorphyll-a
ggplot(buoy.mean.oxy)+aes(y=Dox_mg_L,x=Date,col=type)+geom_point()+
  scale_x_datetime(breaks = "month")+
  theme(axis.text.x = element_text(angle = 90))





















