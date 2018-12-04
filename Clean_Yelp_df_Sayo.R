# Clear workspace
rm(list = ls())

# Load packages
suppressPackageStartupMessages(library(dplyr)) #This will help us do simple manipulations
suppressPackageStartupMessages(library(jsonlite)) #This will allow us to handle JSON data
suppressPackageStartupMessages(library(tibble)) #This will give us a df that is friendly with dplyr
suppressPackageStartupMessages(library(stringr)) #Use this to filter
suppressPackageStartupMessages(library(tm)) #This will help us with text mining

library(ggplot2)
library(scales)

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



rm(list = ls())

yelp_tbl <- read.csv("yelp_df.csv", stringsAsFactors = FALSE, na.strings = c("","NA"))

load.libraries <- c('data.table', 'rvest', 'testthat', 'gridExtra', 'corrplot', 
                    'GGally', 'ggplot2', 'e1071', 'dplyr', "choroplethr","choroplethrMaps")
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)

#Subsetting to Beauty & Spas category
#df <- subset(dg, grepl("Beauty & Spas", categories))

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




#Convert character to factors 

#setDT(df)[,(cat_var) := lapply(.SD, as.factor), .SDcols = cat_var]


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


#Change is_open data to Open and Closed Businesses
df$is_open <- as.character(df$is_open)

wh0 <- which(df$is_open == '0')
wh1 <- which(df$is_open == '1')

df$is_open[wh0] <- 'Closed Business'
df$is_open[wh1] <- 'Open Business' 

df$is_open <- as.factor(df$is_open)

#Distribution of star ratings faceted by open status. We observe a steady rise among closed businesses
#, among open businesses there is a steady rise that steadies between 3.5 and 4.5, then a 
#significant rise to 5
p1 <- qplot(stars, data = df, geom = "bar", fill = I("black"), alpha = I(0.85),
            xlab = "Stars", ylab = "Count", facets = .~is_open)
p1 <- p1 + ggtitle("Distribution of Beauty & Spa Establishments across Star Ratings")
p1 <- p1 + theme(panel.grid.major = element_blank())
p1 <- p1 + theme(panel.grid.minor = element_blank())
p1 <- p1 + theme(panel.border = element_blank())
p1 <- p1 + theme(panel.background = element_blank())
print(p1)
ggsave(filename = "S5.png", plot = p1, width = 6, height = 4,dpi = 600)

#We observe the distribution of the review count. Majority of them are within 0 and 100,
#and for the most part there is a steady decline
p2 <- qplot(review_count, data = df, geom = "histogram", fill = I("black"), alpha = I(0.85),
            xlab = "Number of Reviews", ylab = "Count", log = "x")
p2 <- p2 + ggtitle("Beauty & Spa Establishments by Number of Reviews")
p2 <- p2 + theme(panel.grid.major = element_blank())
p2 <- p2 + theme(panel.grid.minor = element_blank())
p2 <- p2 + theme(panel.border = element_blank())
p2 <- p2 + theme(panel.background = element_blank())
print(p2)
ggsave(filename = "S2.png", plot = p2, width = 6, height = 4,dpi = 600)

#We observe the distribution of review count coded by whether the business accepts credit cards
#we observe the vast majority of the reviews were by businesses that accept credit cards.
#it's notable that as the number of reviews increase, the number of businesses 
#who don't accept credit cards reduce
p3 <- qplot(review_count, data = df, geom = "histogram", alpha = I(0.85),
            xlab = "Number of Reviews", ylab = "Count", log = "x", fill = a.BusinessAcceptsCreditCards)
p3 <- p3 + ggtitle("Beauty & Spa Establishments by Number of Reviews")
p3 <- p3 + theme(panel.grid.major = element_blank())
p3 <- p3 + theme(panel.grid.minor = element_blank())
p3 <- p3 + theme(panel.border = element_blank())
p3 <- p3 + theme(panel.background = element_blank())
p3 <- p3 + scale_fill_hue(name = "Business Accepts Credit Cards")
print(p3)
ggsave(filename = "S3.png", plot = p3, width = 6, height = 4,dpi = 600)

#We observe the all ratings except "5" have more businesses that are not appointment only than those
#that are. Clearly, the appointment only trait would be required to be regarded as "5"
p4 <- qplot(stars, data = df, geom = "histogram", binwidth = 0.5, alpha = I(1), color = I("black"),
             xlab = "Star Rating", ylab = "Count", fill = a.ByAppointmentOnly)
p4 <- p4 + ggtitle("Beauty & Spa Establishments by Star Rating & Appointment Type")
p4 <- p4 + theme(panel.grid.major = element_blank())
p4 <- p4 + theme(panel.grid.minor = element_blank())
p4 <- p4 + theme(panel.border = element_blank())
p4 <- p4 + theme(panel.background = element_blank())
p4 <- p4 + scale_fill_brewer(palette = "Set2", name = "By Appointment Only")
print(p4)
ggsave(filename = "S4.png", plot = p4, width = 6, height = 4,dpi = 600)
