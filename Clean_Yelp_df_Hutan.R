# Clear workspace
rm(list = ls())

# Load packages
suppressPackageStartupMessages(library(dplyr)) #This will help us do simple manipulations
suppressPackageStartupMessages(library(jsonlite)) #This will allow us to handle JSON data
suppressPackageStartupMessages(library(tibble)) #This will give us a df that is friendly with dplyr
suppressPackageStartupMessages(library(stringr)) #Use this to filter
suppressPackageStartupMessages(library(rvest)) #For Scraping data from website

# Import in yelp in json file
yelp <- stream_in(file("yelp_academic_dataset_business.json"))

# Flatten yelp file
yelp_flat <- flatten(yelp)

# Create tbl that can function with dplyr
yelp_tbl <- as_data_frame(yelp_flat)

#How to remove attributes. and hours. in front of colnames
colnames(yelp_tbl) <- gsub("attributes.","",paste(colnames(yelp_tbl)))
colnames(yelp_tbl) <- gsub("hours.","", paste(colnames(yelp_tbl)))

# Export clean dataset into csv file
write.csv(yelp_tbl, "yelp_df.csv", row.names = FALSE) #full file


# Subsets yelp.tbl by category returns a dataframe
cat.subset<-function(category=""){
  url<-"https://www.ups.com/worldshiphelp/WS16/ENU/AppHelp/Codes/State_Province_Codes.htm"  
  pg<- read_html(url)
  tb<-html_table(pg,fill = TRUE,header = TRUE)
  states<-tb[[1]]$Code
  states<-c(states,tb[[2]]$Code)
  df<-yelp_tbl[grep(category,yelp_tbl$categories),]
  i<-1
  results<-integer()
  while (i <= length(states)) {
    results<- c(results, grep(paste("^",states[i],"$", sep=""),df$state))
    i<-i+1
  }
  df<-df[results,]
  return(as.data.frame(df))
}

#Create dataframe df from function above
df <- cat.subset("Restaurants")

#Get NA counts of every columns for data relating to restaurants
df %>% summarise_all(funs(sum(is.na(.))))

#Grab only columns of interest pertaining to restaurants, these were selected because
#these columns actually had substantial data in them
df <- select(df, business_id, name, address,city, state, postal_code, latitude, longitude, 
             stars, review_count, is_open, categories, BusinessAcceptsCreditCards, GoodForKids, OutdoorSeating, RestaurantsAttire, RestaurantsPriceRange2, RestaurantsTakeOut) 



#Load package to run SQL commands inside of R
suppressPackageStartupMessages(library(sqldf))

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


#Load graphing cpabilities
suppressPackageStartupMessages(library(ggplot2))



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
