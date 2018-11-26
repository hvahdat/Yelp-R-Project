# Clear workspace
rm(list = ls())

# Load packages
suppressPackageStartupMessages(library(dplyr)) #This will help us do simple manipulations
suppressPackageStartupMessages(library(jsonlite)) #This will allow us to handle JSON data
suppressPackageStartupMessages(library(tibble)) #This will give us a df that is friendly with dplyr
suppressPackageStartupMessages(library(stringr)) #Use this to filter
suppressPackageStartupMessages(library(tm)) #This will help us with text mining
suppressPackageStartupMessages(library(rvest)) #For Scraping data from website

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
