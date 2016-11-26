library(ggplot2)
library(ggmap)
library(gridExtra)
library(lubridate)
library(RColorBrewer)

blue_pal <- brewer.pal(5,'Blues')
red_pal <- brewer.pal(5, 'YlOrRd')

df <- read.csv('train.csv', stringsAsFactors = FALSE)
d <- df[1:1000,]
d <- droplevels(d)
df <- droplevels(df)
d$Hour <- as.factor(hour(as.POSIXlt(d$Dates)))
df$Hour <- as.factor(hour(as.POSIXlt(df$Dates, tz="UTC")))

#Investigate crimes based on time of day
hourlyDF <- setNames(aggregate(df, list(df$Hour), FUN = "length"), c("Hour", "Count"))
g <- ggplot(data = hourlyDF, aes(x = Hour, y = Count)) + geom_bar(stat="identity", position="dodge", color = blue_pal[3], fill = blue_pal[3]) +
    ggtitle('San Francisco Crimes by Hour')
plot(g)

#Investigate what crimes are the most common
tmp <- setNames(aggregate(df$Category, list(df$Category), FUN = "length"), c('Crime', 'Count'))
print(tmp[order(tmp$Count, decreasing = TRUE),])

violentDF <- df[df$Category %in% c("ROBBERY", "ASSAULT", "SEX OFFENSES FORCIBLE", "KIDNAPPING"),]
theftDF <- df[df$Category %in% c("LARCENY/THEFT", "VEHICLE THEFT", "BURGLARY"),]
violentDF <- droplevels(violentDF)
theftDF <- droplevels(theftDF)

#Bar graph of violent crime by hour
violentHourlyDF <- setNames(aggregate(violentDF, list(violentDF$Hour), FUN = "length"), c("Hour", "Count"))
g <- ggplot(data = violentHourlyDF, aes(x = Hour, y = Count)) + geom_bar(stat="identity", position="dodge", color = red_pal[4], fill = red_pal[4]) +
  ggtitle('San Francisco Crimes by Hour')
plot(g)

#Plot all violent crimes together
png('output/All_Violent_Crime.png', units = 'in', width = 11, height = 8, res = 600)
sf_map <- suppressMessages(get_map(location = c(-122.4431871, 37.7605004), zoom = 12, maptype = "roadmap"))
g <- ggmap(sf_map) + geom_point(data = violentDF, aes(X, Y, colour = Category), alpha = 0.5, size = .5) +
  scale_x_continuous(limits = c(-122.55, -122.355), expand = c(0, 0)) +
  scale_y_continuous(limits = c(37.705, 37.84), expand = c(0,0)) + labs(x = 'Longitude', y = 'Latitude') +
  ggtitle('Crime in San Francisco')
plot(g)
dev.off()


#Split out the crimes into seperate maps
png('output/Violent_Crime_Multiplot.png', units = 'in', width = 11, height = 8, res = 600)
sf_map <- suppressMessages(get_map(location = c(-122.4431871, 37.7605004), zoom = 12, maptype = "roadmap"))
g <- ggmap(sf_map) + geom_point(data = violentDF, aes(X, Y, colour = Category), alpha = 0.5, size = .5) +
  scale_x_continuous(limits = c(-122.55, -122.355), expand = c(0, 0)) +
  scale_y_continuous(limits = c(37.705, 37.84), expand = c(0,0)) + labs(x = 'Longitude', y = 'Latitude') +
  facet_wrap(~Category) + ggtitle('Crime in San Francisco')
plot(g)
dev.off()

#Heatmapping crimes on separate maps
png('output/Violent_Crime_Multiheatmap.png', units = 'in', width = 11, height = 8, res = 600)
sf_map <- suppressMessages(get_map(location = c(-122.4431871, 37.7605004), zoom = 12, maptype = "roadmap"))
g <- ggmap(sf_map) + geom_tile(data = violentDF, aes(X, Y, colour = Category), alpha = 0.5, size = .5) +
  scale_x_continuous(limits = c(-122.55, -122.355), expand = c(0, 0)) +
  scale_y_continuous(limits = c(37.705, 37.84), expand = c(0,0)) + labs(x = 'Longitude', y = 'Latitude') +
  facet_wrap(~Category) + ggtitle('Crime in San Francisco')
plot(g)
dev.off()