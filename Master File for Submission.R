# Clear workspace
rm(list = ls())

# Load packages
suppressPackageStartupMessages(library(dplyr)) #This will help us do simple manipulations
suppressPackageStartupMessages(library(jsonlite)) #This will allow us to handle JSON data
suppressPackageStartupMessages(library(tibble)) #This will give us a df that is friendly with dplyr
suppressPackageStartupMessages(library(stringr)) #Use this to filter
suppressPackageStartupMessages(library(tm)) #This will help us with text mining
suppressPackageStartupMessages(library(rattle)) #Helpful tool to explore and visualize data
suppressPackageStartupMessages(library(RGtk2)) #Needed for rattle package
suppressPackageStartupMessages(library(rvest)) #Pull data from www
suppressPackageStartupMessages(library(sqldf)) #Use SQL code in R
suppressPackageStartupMessages(library(ggplot2)) #Make charts
suppressPackageStartupMessages(library(scales)) #Stats & visualization


# Import in yelp in json file
yelp <- stream_in(file("yelp_academic_dataset_business.json"))

# Flatten yelp file
yelp_flat <- flatten(yelp)

# Create tbl that can function with dplyr
yelp_tbl <- as_data_frame(yelp_flat)

# Replace "attributes." and "hours." in front of colnames
# with "a." and "h." respectively
colnames(yelp_tbl) <- gsub("attributes.","a.",paste(colnames(yelp_tbl)))
colnames(yelp_tbl) <- gsub("hours.","h.", paste(colnames(yelp_tbl)))

# Export clean dataset into csv file
write.csv(yelp_tbl, "yelp_df.csv", row.names = FALSE)

# Subsets yelp.tbl by category returns a dataframe with only US and Canadian businesses
cat.subset<-function(category=""){                                                          #Create cat.subset function
  url<-"https://www.ups.com/worldshiphelp/WS16/ENU/AppHelp/Codes/State_Province_Codes.htm"  #Link for State/Province Codes
  pg<- read_html(url)                                                                       #read in from url
  tb<-html_table(pg,fill = TRUE,header = TRUE)                                              #Save tables
  states<-tb[[1]]$Code                                                                      #Add US codes to list
  states<-c(states,tb[[2]]$Code)                                                            #Combine US States to Canadian Provinces
  df<-yelp_tbl[grep(category,yelp_tbl$categories),]                                         #Subset by Category selected
  i<-1
  results<-integer()
  while (i <= length(states)) {                                                             #Search for states/provinces
    results<- c(results, grep(paste("^",states[i],"$", sep=""),df$state))                   #Save row number if exact match for states found 
    i<-i+1
  }
  df<-df[results,]                                                                          #Subset with only row in US or Canada
  return(as.data.frame(df))                                                                 #Return df
}

df<-cat.subset("Gas Stations")

#Count max amount of individual categories listed in categories column for a single row
max(na.omit(count.fields(textConnection(yelp_tbl$categories), sep = ",")))

#Print unique values in each column
for (i in 1:ncol(yelp_tbl)) {
  print(unique(yelp_tbl[i]))
}

#Create df with count gas stations names that are the same
dfsql <- sqldf("select name, count(name)
               from df
               group by name
               order by count(name) desc")

#Creates percent column and calculates percent for companies
dfsql$percent<-(dfsql$`count(name)`/sum(dfsql$`count(name)`))*100

#Start for loop. If count is less than 8 keep column is NA if count is > 8 then keep is 1
i<-1
total<-0
count<-0
for (i in 1:nrow(dfsql)) {
  if(dfsql$`count(name)`[i]< 8){
    total<-total+dfsql$percent[i]
    count<-count+dfsql$`count(name)`[i]
    dfsql$keep[i]<-NA
  } else {
    dfsql$keep[i]<- 1
  }
}
pie<-na.omit(dfsql)          #Take out rows with NA 

#Insert name:Other, count(name):687, percent: 47.379310, keep:1 
pie[nrow(pie) + 1,] <- list(as.character("Other"),as.integer("687"), as.numeric("47.379310"), as.integer("1")) #Insert Row

write.csv(df,"Gas_Stations.csv")  
write.csv(pie, "pie.csv")

pie<-read.csv("pie.csv")

#Plot pie chart with 23 Gas Station names and 1 "Other" 
pie(pie$percent,pie$name,radius=1,main="Names of Gas Stations",init.angle=180) 

#Return bar chart outlining star distribution of data
star.plot<-qplot(stars, data = df, geom = "bar", fill = I("red"), color = I("black"), 
                 alpha = I(0.25), main = "Yelp Star Distribution for Gas Stations", 
                 xlab = "Stars", ylab = "Count") 

#Create df with number of gas stations in each city ranked with most at the top
city.count <- sqldf("select state, city,count(name)
                    from df
                    group by state,city
                    order by count(name) desc")

top20cities<-head(city.count,20) #Subset top 20 cities
top20cities$state<-NULL          #NULL state column

city.plot<-qplot(top20cities$`count(name)`,top20cities$city,main = "Count by City")

#Plot Top 20 Cities with most gas stations and the number in each city; Also some cleaning of graph
top20.plot<-ggplot(top20cities, aes(y=top20cities$`count(name)`, x=as.factor(top20cities$city))) + geom_bar(stat="identity", position="stack", alpha=0.5) + xlab("City") + ylab("Count") +  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Top 20 Gas Station Count by City")

ggsave(filename = "Top 20 Cities for Gas Stations Bar Chart.pdf",plot = top20.plot)      #Save top20.plot as pdf file
ggsave(filename = "Gas Station Star Distribution.pdf",plot = star.plot)                  #Save star.plot as pdf file
png(file = "Names of Gas Stations.png",width = 1020,height = 824)                        #Save pie chart
pie(pie$percent,pie$name,radius=1,main="Names of Gas Stations",init.angle=180)           #in png file
dev.off()










#Create dataframe df from function above
df <- cat.subset("Restaurants")

#Get NA counts of every columns for data relating to restaurants
df %>% summarise_all(funs(sum(is.na(.))))

#Grab only columns of interest pertaining to restaurants, these were selected because
#these columns actually had substantial data in them
df <- select(df, business_id, name, address,city, state, postal_code, latitude, longitude, 
             stars, review_count, is_open, categories, a.BusinessAcceptsCreditCards, a.GoodForKids, a.OutdoorSeating, a.RestaurantsAttire, a.RestaurantsPriceRange2, a.RestaurantsTakeOut) 



#I am running a SQL command inside of R to grab the restaurant name and the count of times they 
#show up in the data set. Just grab the top 100 and talk about how all the major chains populate it.
#Include a picture of the table or a portion of it if you like
dfsql <- sqldf("select name, count(name) as 'Count'
               from df
               group by name
               order by count(name) desc
               limit 100")

#Run a line of code to get the count of the reviews for 
#top 100 most reviewed distinct restaurants
#This returns 8503 out of 56839 = only about 15%..bulk of reviews are distributed widely among
#restaurants
sum(dfsql$Count)




#Return bar chart outlining star distribution of data for only Restaurants
g <- qplot(stars, data = df, geom = "bar", fill = I("red"), color = I("black"), 
           alpha = I(0.25), main = "Yelp Star Distribution in Restaurant Category", 
           xlab = "Stars", ylab = "Count") 
g    

ggsave(filename = "Star_Dist_Restaurants.png", plot = g, width = 6, height = 4,
       dpi = 600)

#Return bar chart outlining star distribution of entire  Yelp 2018 data set
#Discuss how there are some differences in restaurant rating distrubtion 
#versus the entire yelp dataset distribution of stars
#The entire distribution is slightly more skewed towards higher ratings, where restaurant
#reviews seem slightly more balanced
g1 <- qplot(stars, data = yelp_tbl, geom = "bar", fill = I("red"), color = I("black"), 
            alpha = I(0.25), main = "Yelp Star Distribution in all Categories", 
            xlab = "Stars", ylab = "Count") 
g1

ggsave(filename = "Star_Dist_All_Cats.png", plot = g1, width = 6, height = 4,
       dpi = 600)

#Average stars of open businesses =  3.453939 vs closed businesses = 3.415991
#Discuss how the number of stars doesn't seem to have an impact
#on whether a business closes or not since the avg. stars are the essentially the same
mean(df$stars[df$is_open == 1])
mean(df$stars[df$is_open == 0])


#Another sql command that shows me which cities have the highest prevalence in 
#our data set. I will be creating a geographic zoom in by city. Where it will be
#broken down via zip code, star rating, and city. I only cover Las Vegas since
#it is the largest US city from the data set and break down the distribution by zip code
#size is dependent on stars of restaurants. 

dfsql2 <- sqldf("select distinct city, count(city) as 'Count'
                from df
                group by city
                order by count(city) desc
                limit 100")



# Limit analysis to just top American city (Las Vegas) with most restaraunt reviews
VegasFilter <- dplyr::filter(df, city == "Las Vegas" & postal_code != "" & latitude < 38) #This grabs only vegas & not nulls for postal code


#Analysis below will return the min and max of the latitude and longitude coordinates for Las Vegas
#needed to properly graph the visuals.
max(VegasFilter$longitude) #-114.8963
min(VegasFilter$longitude) #-115.6796
max(VegasFilter$latitude)  #36.43031 
min(VegasFilter$latitude)  #35.60673

# Export clean dataset into csv file
write.csv(VegasFilter, "yelp_df_VegasFilter.csv", row.names = FALSE) #vegas file


#Plot code to try and pull all the info together. This is the graphic that will show a geographic
#breakdown of Las Vegas. Color coded by zip code, then broken out by size (dependent on yelp stars)
g2 <- qplot(longitude, latitude, data = VegasFilter, color = postal_code, size = stars,
            xlim = c(-115.6796,-114.8963), ylim = c(35.60673,36.43031), na.rm = TRUE)
g2 <- g2 + scale_color_hue(name = "Postal Code")
g2 <- g2 + scale_size("Stars", range = c(0.5,2.5))
g2 <- g2 + ggtitle("Map of Review Stars for Las Vegas")
g2 <- g2 + theme(axis.title = element_blank())
g2 <- g2 + theme(axis.text = element_blank())
g2 <- g2 + theme(axis.ticks = element_blank())
g2 <- g2 + theme(panel.grid = element_blank())
g2 <- g2 + theme(panel.border = element_rect(linetype = "solid", color = "black",
                                             fill = NA))
g2 <- g2 + theme(legend.text = element_text(size = 8))
g2 <- g2 + theme(legend.key.size = unit(0.12, "in"))
g2 <- g2 + theme(title = element_text(size = 12))
g2 <- g2 + theme(legend.spacing = unit(0, "in"))
g2 <- g2 + theme(legend.key = element_rect(fill = NA))
g2 <- g2 + theme(plot.title = element_text(face = "bold"))

g2

#ggsave(filename = "Vegas.pdf", plot = g2, width = 8, height = 6,
#units = "in")

ggsave(filename = "Vegas_Star_Dist.png", plot = g2, width = 6, height = 4,
       dpi = 600)










df <- cat.subset("Beauty & Spas")

#Examine the structure of the data frame
str(df)

#Attach the data frame
attach(df)

#Observe missing values
#We see here that most columns cannot be used since almost 90% of their values are empty - hence,
#we will be focusing on the following columns, "a.BusinessAcceptsCreditCards", "is_open", 
#"a.ByAppointmentOnly"

colSums(sapply(df, is.na))

# The percentage of data missing in the data frame
sum(is.na(df)) / (nrow(df) *ncol(df))




#Distribution of Stars Rating
hist(stars, main = "Distribution of Stars Rating")

#Observe average review count by star rating. we see that the average rises steadily by rating
#whether closes or not, but it peaks at 4.5 and experiences a sharp drop to 13.5 at 5
df %>% 
  group_by(stars) %>%
  summarize(AverageReviewCount = mean(review_count))


#Observe the percentage spread across star ratings and the open businesses
#We see that there's an even spread amongst open and closed businesses across all star ratings
#We also observe that the number of establishments rises significantly as we 
#climb the star ratings
df %>% 
  group_by(stars, is_open) %>%
  summarize(Count = n())%>%
  mutate(Percent = (Count/sum(Count)) *100)


#Distribution of star ratings faceted by open status. We observe a steady rise among closed businesses
#, among open businesses there is a steady rise that steadies between 3.5 and 4.5, then a 
#significant rise to 5
p1 <- qplot(stars, data = df, geom = "bar", fill = I("blue"), alpha = I(0.25), 
            xlab = "Stars", ylab = "Count", facets = .~is_open)
p1 <- p1 + ggtitle("Distribution of Beauty & Spa Establishments across Star Ratings")
p1 <- p1 + theme(panel.grid.major = element_blank())
p1 <- p1 + theme(panel.grid.minor = element_blank())
print(p1)
ggsave(filename = "S1.png", plot = p1, width = 6, height = 4,dpi = 600)

#We observe the distribution of the review count. Majority of them are within 0 and 100,
#and for the most part there is a steady decline
p2 <- qplot(review_count, data = df, geom = "histogram", fill = I("green"), alpha = I(0.25), 
            xlab = "Number of Reviews", ylab = "Count", log = "x")
p2 <- p2 + ggtitle("Distribution of Beauty & Spa Establishments by Number of Reviews")
p2 <- p2 + theme(panel.grid.major = element_blank())
p2 <- p2 + theme(panel.grid.minor = element_blank())
print(p2)
ggsave(filename = "S2.png", plot = p2, width = 6, height = 4,dpi = 600)

#We observe the distribution of review count coded by whether the business accepts credit cards
#we observe the vast majority of the reviews were by businesses that accept credit cards.
#it's notable that as the number of reviews increase, the number of businesses 
#who don't accept credit cards reduce
p3 <- qplot(review_count, data = df, geom = "histogram", alpha = I(0.25), 
            xlab = "Number of Reviews", ylab = "Count", log = "x", fill = a.BusinessAcceptsCreditCards)
p3 <- p3 + ggtitle("Distribution of Beauty & Spa Establishments by Number of Reviews")
p3 <- p3 + theme(panel.grid.major = element_blank())
p3 <- p3 + theme(panel.grid.minor = element_blank())
p3 <- p3 + scale_fill_hue(name = "Business Accepts Credit Cards")
print(p3)
ggsave(filename = "S3.png", plot = p3, width = 6, height = 4,dpi = 600)

#We observe the all ratings except "5" have more businesses that are not appointment only than those
#that are. Clearly, the appointment only trait would be required to be regarded as "5"
p4 <- qplot(stars, data = df, geom = "histogram", alpha = I(0.25), 
             xlab = "Star Rating", ylab = "Count", log = "x", fill = a.ByAppointmentOnly)
p4 <- p4 + ggtitle("Distribution of Beauty & Spa Establishments by Star Rating and Appointment Status")
p4 <- p4 + theme(panel.grid.major = element_blank())
p4 <- p4 + theme(panel.grid.minor = element_blank())
p4 <- p4 + scale_fill_hue(name = "By Appointment Only")
print(p4)
ggsave(filename = "S4.png", plot = p4, width = 6, height = 4,dpi = 600)
