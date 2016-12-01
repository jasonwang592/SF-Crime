library(ggplot2)
library(ggmap)
library(gridExtra)
library(lubridate)
library(RColorBrewer)
source('faceted_plotting.R')

sf.long <- c(-122.55, -122.355)
sf.lat <- c(37.705, 37.84)

blue_pal <- brewer.pal(5,'Blues')
red_pal <- brewer.pal(5, 'YlOrRd')

df <- read.csv('data/train.csv')
df$Hour <- as.factor(hour(as.POSIXlt(df$Dates, tz="UTC")))
df$Year <- factor(year(df$Dates))
df$DayOfWeek <- factor(df$DayOfWeek, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday",
                                              "Friday", "Saturday"))
df$Month <- month.abb[month(df$Dates)]
df$Month <- factor(df$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                                        "Oct", "Nov", "Dec"))

# Investigate crimes based on time of day
hourlyDF <- setNames(aggregate(df, list(df$Hour), FUN = "length"), c("Hour", "Count"))
g <- ggplot(data = hourlyDF, aes(x = Hour, y = Count)) + geom_bar(stat="identity", position="dodge", color = blue_pal[3], fill = blue_pal[3]) +
    ggtitle('San Francisco Crimes by Hour')
plot(g)

# Investigate what crimes are the most common
tmp <- setNames(aggregate(df$Category, list(df$Category), FUN = "length"), c('Crime', 'Count'))
print(tmp[order(tmp$Count, decreasing = TRUE),])


# Analysis from this point on is less generalized. Data is split and analyzed based on whether the
# crime is violent or non-violent.

violentDF <- df[df$Category %in% c("ROBBERY", "ASSAULT", "SEX OFFENSES FORCIBLE", "KIDNAPPING"),]
theftDF <- df[df$Category %in% c("LARCENY/THEFT", "VEHICLE THEFT", "BURGLARY"),]
violentDF <- droplevels(violentDF)
theftDF <- droplevels(theftDF)

# Bar graph of violent crime by hour
png('output/Hourly_Violent_Crime_Bar.png', units = 'in', width = 11, height = 8, res = 400)
g <- aggregate_bar(violentDF, "Hour")
plot(g)

# Bar graph of violent crime by day
png('output/Daily_Violent_Crime_Bar.png', units = 'in', width = 11, height = 8, res = 400)
g <- aggregate_bar(violentDF,"DayOfWeek")
plot(g)

# Bar graph of violent crime by month
png('output/Montly_Violent_Crime_Bar.png', units = 'in', width = 11, height = 8, res = 400)
g <- aggregate_bar(violentDF,"Month")
plot(g)

# Bar graph of violent crime by month broken down by Category
png(paste0('output/Monthly_Violent_Crime_By_Category_Bar.png'), units = 'in', width = 11, height = 8, res = 400)
g <- faceted_aggregate_bar(violentDF, "Month")
dev.off()

# Plot all violent crimes together
png('output/All_Violent_Crime.png', units = 'in', width = 11, height = 8, res = 600)
sf_map <- suppressMessages(get_map(location = c(mean(sf.long), mean(sf.lat)), zoom = 12, maptype = "roadmap"))
g <- ggmap(sf_map) + geom_point(data = violentDF, aes(X, Y, colour = Category), alpha = 0.5, size = .5) +
  labs(x = 'Longitude', y = 'Latitude') + ggtitle('Crime in San Francisco')
plot(g)
dev.off()

# Plot violent crimes gps points by year
for (i in levels(violentDF$Year)) {
  png(paste0('output/Yearly_Violent_Crime_Points/Year_', i, '.png'), units = 'in', width = 11, height = 8, res = 400)
  sf_map <- suppressMessages(get_map(location = c(mean(sf.long), mean(sf.lat)), zoom = 12, maptype = "roadmap"))
  g <- ggmap(sf_map) + geom_point(data = violentDF[violentDF$Year == i,], aes(X, Y, colour = Category), alpha = 0.5, size = .5) +
    scale_x_continuous(limits = sf.long, expand = c(0, 0)) + scale_y_continuous(limits = sf.lat, expand = c(0, 0)) +
    labs(x = 'Longitude', y = 'Latitude') + ggtitle(paste0('Violent Crime in San Francisco, Year ', i))
  plot(g)
  dev.off()
}

# Plot line graph for violent crimes by year
violentYearlyDF <- setNames(aggregate(data = violentDF, .~ Year + Category, FUN = "length"), c("Year", "Category", "Count"))[,1:3]
png(paste0('output/Violent_Crime_Trend_By_Year.png'), units = 'in', width = 11, height = 8, res = 400)
g <- ggplot(data = violentYearlyDF, aes(x = Year, y = Count, group = Category, shape = Category, colour = Category)) + geom_line() + geom_point()
plot(g)
dev.off()

# Split out the crimes into seperate maps
png('output/Violent_Crime_Multiplot.png', units = 'in', width = 11, height = 8, res = 600)
sf_map <- suppressMessages(get_map(location = c(mean(sf.long), mean(sf.lat)), zoom = 12, maptype = "roadmap"))
g <- ggmap(sf_map) + geom_point(data = violentDF, aes(X, Y, colour = Category), alpha = 0.5, size = .5) +
  labs(x = 'Longitude', y = 'Latitude') + facet_wrap(~Category) + ggtitle('Crime in San Francisco')
plot(g)
dev.off()

# Heatmapping crimes on separate maps
png('output/Violent_Crime_Multiheatmap_All.png', units = 'in', width = 11, height = 8, res = 400)
g <- faceted_heatmap(violentDF, 'Crime in San Francisco')
plot(g)
dev.off()

# Heatmapping crimes on separate maps by hour
for (i in levels(violentDF$Hour)) {
  png(paste0('output/Hourly_Violent_Crime_Heatmap_/Hour_', i, '.png'), units = 'in', width = 11, height = 8, res = 400)
  g <- faceted_heatmap(violentDF[violentDF$Hour == i,], paste0('Crime in San Francisco, Hour ', i))
  plot(g)
  dev.off()
}

