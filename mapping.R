##user: KArthik Ravi
##Place: Hackathon clt, discovery science lab
## date: 03/24/2018

##model: Time series analysis
##model name: Price watching Cuckoo

setwd("")

# Turn off scientific notation
options(scipen = "999") 
# Ensure strings come in as character types
options(stringsAsFactors = FALSE)

library(ggplot2)
library(ggmap)
library(maptools)
library(ggthemes)
library(rgeos)
library(broom)
library(dplyr)
library(plyr)
library(grid)
library(gridExtra)
library(reshape2)
library(scales)


##color palettes
# Define one that we will use for plots
plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 18,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=8),
    axis.text = element_text(size=8),
    axis.title.x = element_text(hjust=1),
    axis.title.y = element_text(hjust=1),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"))
}

# And another that we will use for maps
mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 18,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    strip.text = element_text(size=12),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey80", color = "white"),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"))
}

# Define some palettes
palette_9_colors <- c("#0DA3A0","#2999A9","#458FB2","#6285BB","#7E7CC4","#9A72CD","#B768D6","#D35EDF","#F055E9")
palette_8_colors <- c("#0DA3A0","#2D97AA","#4D8CB4","#6E81BF","#8E76C9","#AF6BD4","#CF60DE","#F055E9")
palette_7_colors <- c("#2D97AA","#4D8CB4","#6E81BF","#8E76C9","#AF6BD4","#CF60DE","#F055E9")
palette_1_colors <- c("#0DA3A0")

# Read in a csv of home sale transactions directly from github.
sf <- read.csv("https://raw.githubusercontent.com/simonkassel/Visualizing_SF_home_prices_R/master/Data/SF_home_sales_demo_data.csv")

# We will need to consider Sale Year as a categorical variable so we convert it from a numeric variable to a factor
sf$SaleYr <- as.factor(sf$SaleYr)

URL <- "https://github.com/simonkassel/Visualizing_SF_home_prices_R/raw/master/Data/SF_neighborhoods.zip"
# Download the shapefile to your working directory and unzip it.
download.file(URL, "SF_neighborhoods.zip")
unzip("SF_neighborhoods.zip")
# Read it into R as a spatial polygons data frame & plot
neighb <- readShapePoly("SF_neighborhoods")
plot(neighb)

home_value_hist <- ggplot(sf, aes(SalePrice)) + 
  geom_histogram(fill=palette_1_colors) +
  xlab("Sale Price($)") + ylab("Count") +
  scale_fill_manual(values=palette_1_colors) +
  plotTheme() + 
  labs(x="Sale Price($)", y="Count", title="Distribution of San Francisco home prices",
       subtitle="Nominal prices (2009 - 2015)", 
       caption=" ") +
  scale_x_continuous(labels = comma) + scale_y_continuous(labels = comma)
# Plotting it:
home_value_hist
# And saving it to the working directory:
ggsave("plot1_histogram.png", home_value_hist, width = 8, height = 4, device = "png")

#outiners
sf <- sf[which(sf$SalePrice < mean(sf$SalePrice) + (2.5 * sd(sf$SalePrice))), ]





######violin plot
home_value_violin <- ggplot(sf, aes(x=SaleYr, y=SalePrice, fill=SaleYr)) + 
  geom_violin(color = "grey50") +
  xlab("Sale Price($)") + ylab("Count") +
  scale_fill_manual(values=palette_7_colors) +
  stat_summary(fun.y=mean, geom="point", size=2, colour="white") +
  plotTheme() + theme(legend.position="none") +
  scale_y_continuous(labels = comma) +
  labs(x="Year",y="Sale Price($)",title="Distribution of San Francisco home prices",
       subtitle="Nominal prices (2009 - 2015); Sale price means visualized as points",
       caption=" ")
home_value_violin
ggsave("plot2_violin.png", home_value_violin, width = 8, height = 4, device = "png")






##Maps
# Define the bounding box
bbox <- neighb@bbox

# Manipulate these values slightly so that we get some padding on our basemap between the edge of the data and the edge of the map
sf_bbox <- c(left = bbox[1, 1] - .01, bottom = bbox[2, 1] - .005, 
             right = bbox[1, 2] + .01, top = bbox[2, 2] + .005)
# Download the basemap
basemap <- get_stamenmap(
  bbox = sf_bbox,
  zoom = 13,
  maptype = "toner-lite")

# Map it
bmMap <- ggmap(basemap) + mapTheme() + 
  labs(title="San Francisco basemap")
bmMap




###small multiple plots

prices_mapped_by_year <- ggmap(basemap) + 
  geom_point(data = sf, aes(x = long, y = lat, color = SalePrice), 
             size = .25, alpha = 0.6) +
  facet_wrap(~SaleYr, scales = "fixed", ncol = 4) +
  coord_map() +
  mapTheme() + theme(legend.position = c(.85, .25)) +
  scale_color_gradientn("Sale Price", 
                        colors = palette_8_colors,
                        labels = scales::dollar_format(prefix = "$")) +
  labs(title="Distribution of San Francisco home prices",
       subtitle="Nominal prices (2009 - 2015)",
       caption=" ")
prices_mapped_by_year



#####subset maps

prices_mapped_2009_2015 <- ggmap(basemap) + 
  geom_point(data = subset(sf, sf$SaleYr == 2015 | sf$SaleYr == 2009), 
             aes(x = long, y = lat, color = SalePrice), 
             size = 1, alpha = 0.75) +
  facet_wrap(~SaleYr, scales = "fixed", ncol = 1) +
  coord_map() +
  mapTheme() +
  scale_color_gradientn("Sale Price", 
                        colors = palette_8_colors,
                        labels = scales::dollar_format(prefix = "$")) +
  labs(title="Distribution of San Francisco home prices",
       subtitle="Nominal prices (2009 & 2015)",
       caption=" ")
prices_mapped_2009_2015




#####
#Subset sales for the "Inner Mission neighborhood"
missionSales <- sf[which(sf$Neighborhood == "Inner Mission"), ]

#Create a new basemap at the appropriate scale
centroid_lon <- median(missionSales$long)
centroid_lat <- median(missionSales$lat)
missionBasemap <- get_map(location = c(lon = centroid_lon, lat = centroid_lat), 
                          source = "stamen",maptype = "toner-lite", zoom = 15)

#Create a facet map by year
mission_mapped_by_year <- ggmap(missionBasemap) + 
  geom_point(data = missionSales, aes(x = long, y = lat, color = SalePrice), 
             size = 2) +
  facet_wrap(~SaleYr, scales = "fixed", ncol = 4) +
  coord_map() +
  mapTheme() + theme(legend.position = c(.85, .25)) +
  scale_color_gradientn("Sale Price", 
                        colors = palette_8_colors,
                        labels = scales::dollar_format(prefix = "$")) +
  labs(title="Distribution of Mission District home prices",
       subtitle="Nominal prices (2009 - 2015)",
       caption=" ")
mission_mapped_by_year




####
# We'll start by summarizing our existing sales data frame
sf.summarized <- ddply(sf, c("Neighborhood", "SaleYr"), summarise, 
                       medianPrice = median(SalePrice),
                       saleCount = length(SaleYr),
                       sdPrice = sd(SalePrice),
                       minusSd = medianPrice - sdPrice,
                       plusSD = medianPrice + sdPrice,
                       .progress = "text")
head(sf.summarized, 10)

# Now we'll calculate the average annual home sale count for each neighborhood
yearly_sales <- ddply(sf.summarized, ~Neighborhood, summarise, 
                      avg.yearly.sales = mean(saleCount))

# Then we will use a left join to join the average sale count data frame to the original summarized data frame. 
sf.summarized <- left_join(sf.summarized, yearly_sales, by = "Neighborhood")

# Next, we'll calculate the % change in neighborhood median home value. In order to do this we will need to reshape the data 
# again using the dcast function in the package reshape2. 
medByYear <- dcast(sf.summarized, Neighborhood ~ SaleYr, value.var = "medianPrice")
# Check out the reshaped data frame
head(medByYear)

# We now have a data frame in which each row is a neighborhood, each column is a year and the values are the corresponding median prices. 
# Now we can easily calculate the % change from 2009 to 2015.
medByYear$pctChange <- (medByYear$`2015` - medByYear$`2009`) / medByYear$`2009`
# And join it back to our sf.summarized dataset
sf.summarized <- left_join(sf.summarized, medByYear[,c("Neighborhood", "pctChange")], 
                           by = "Neighborhood")

# Some neighborhoods have very low annual sale counts. 
# We will remove these neighborhoods from the dataset by converting them to NA.
sf.summarized$pctChange <- ifelse(sf.summarized$avg.yearly.sales < 10, NA, 
                                  sf.summarized$pctChange)

# Remember the neighborhood shapefile we imported at the beginning? We must now convert it to a format that ggplot understands.
neighb.tidy <- tidy(neighb, region = c('nbrhood'))
# Look at the resulting data frame to see how it has been transformed
head(neighb.tidy)

# Create an identical field to 'id' but with a name that will allow us to join the data frame to our summarized price data
neighb.tidy$Neighborhood <- neighb.tidy$id

# Now we're going to join these data frames together so that when we map the neighborhood polygons we can symbolize them using the summary
# stats we created
sf.summarized_tidy <- join(sf.summarized, neighb.tidy, by = "Neighborhood", match = "all")





###neighbour maps
neighb_map <- ggmap(basemap) +
  geom_polygon(data = sf.summarized_tidy, 
               aes(x = long, y = lat, group = group, fill = medianPrice), 
               colour = "white", alpha = 0.75, size = 0.25) + 
  scale_fill_gradientn("Neighborhood \nMedian \nSale Price", 
                       colors = palette_8_colors,
                       labels = scales::dollar_format(prefix = "$")) +
  mapTheme() + theme(legend.position = c(.85, .25)) + coord_map() +
  facet_wrap(~SaleYr, nrow = 2) +
  labs(title="Median home price by neighborhood, San Francisco ",
       subtitle="Nominal prices (2009 - 2015)",
       caption=" ")
neighb_map





##percentage changes
# Plot the percent change in neighborhood median home value over time
change_map <- ggmap(basemap) +
  geom_polygon(data = sf.summarized_tidy[which(sf.summarized_tidy$SaleYr == 2015), ], 
               aes(x = long, y = lat, group = group, fill = pctChange), 
               colour = "white", alpha = 0.75, size = 0.25) + 
  coord_map() +
  scale_fill_gradientn("% Change", colors = palette_8_colors,
                       labels = scales::percent_format()) +
  mapTheme() + 
  theme(legend.position = "bottom", 
        legend.direction = "horizontal", 
        legend.key.width = unit(.5, "in")) +
  labs(title="Percent change in median home prices, San Francisco",
       subtitle="Nominal prices (2009 - 2015)",
       caption=" ")
change_map


##data wrangling

# Lets look at the top 8 most appreciating neighborhoods.
topPctChange <- unique(sf.summarized$pctChange) %>% sort(decreasing = TRUE) %>% head(8)

# Well use these percentages to subset our neighborhoods data frame
sfForTimeSeries <- sf.summarized[which(sf.summarized$pctChange %in% topPctChange), ] 




##time series analysis
time.series <- ggplot(sfForTimeSeries, aes(x = SaleYr, group=Neighborhood)) +
  geom_line(aes(y = medianPrice)) +
  geom_ribbon(aes(ymin = minusSd, ymax = plusSD, fill = Neighborhood), alpha = 0.75) +
  facet_wrap(~Neighborhood, scales = "fixed", nrow = 4) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
  ylab("Neighborhood median home price") + xlab(NULL) +
  plotTheme() +
  theme(
    legend.position = "none",
    panel.spacing.y = unit(1, "lines")
  ) +
  scale_fill_manual(values=palette_8_colors) +
  labs(title="Time series for highest growth neighborhoods, San Francisco",
       subtitle="Nominal prices (2009-2015)",
       caption=" ")
time.series


##merging time series and maps
sampleNeighborhoods <- ggmap(basemap) + 
  geom_polygon(data = neighb.tidy, aes(x = long, y = lat, group = group), 
               colour = NA, fill="black", alpha = 1) +
  geom_polygon(data = neighb.tidy[which(neighb.tidy$Neighborhood %in% sfForTimeSeries$Neighborhood), ], 
               aes(x = long, y = lat, group = group, fill = Neighborhood), 
               colour = "black") +
  coord_map() +
  scale_fill_manual(values=palette_9_colors)+
  mapTheme() + 
  theme(legend.position = "bottom", 
        legend.direction = "horizontal", 
        legend.key.size = unit(0.15, "in"),
        legend.title=element_blank()) +
  guides(fill=guide_legend(ncol = 2))
sampleNeighborhoods





##  Merging
blank <- grid.rect(gp=gpar(col="white"))

# Next, we will create an ordered list of the plots we want to use, including 'blank' 
gs <- list(time.series, blank, sampleNeighborhoods)
# Finally, we will denote a matrix that will dictate the layout of the plots. Each number
# in the matrix refers to a position in the list of plots.
lay <- rbind(c(1,1,2),
             c(1,1,2),
             c(1,1,3),
             c(1,1,3),
             c(1,1,2),
             c(1,1,2))

# Arrange the plot
grid.arrange(grobs = gs, layout_matrix = lay)





##final plot description and information visualization
# First we create a data frame of just 2009 and remove neighborhoods 
# that have an NA value for pctChange due to on insufficient sale volume
sf.2009 <- sf.summarized[which(sf.summarized$SaleYr == 2009), ] %>% na.omit()

# Then create the scatterplot using neighborhood name labels instead of points
change_scatterplot <- ggplot(sf.2009, aes(x = pctChange, y = medianPrice, label = Neighborhood)) + 
  geom_label(data = sf.2009[which(!sf.2009$pctChange %in% topPctChange),], 
             aes(label = Neighborhood), fill = "grey20", size = 2, color = "white") +
  geom_label(data = sf.2009[which(sf.2009$pctChange %in% topPctChange),], 
             aes(label = Neighborhood, fill = Neighborhood), size = 2, color = "white") +
  scale_fill_manual(values=palette_9_colors) +
  geom_smooth(method = "lm", se = FALSE) +
  plotTheme() + theme(legend.position = "none") +
  scale_y_continuous(labels = dollar_format(prefix = "$")) + 
  scale_x_continuous(labels = percent, limits = c(min(sf.2009$pctChange - .04), max(sf.2009$pctChange + .025)) ) +
  labs(x="% Change", y="Median Sale Price (2009)",
       title="Change in home price as a function of initial price",
       subtitle="Median price; Change between 2009 - 2015",
       caption=" ")
change_scatterplot
