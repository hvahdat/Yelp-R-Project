# Load the package

suppressPackageStartupMessages(library(dplyr)) #This will help us do simple manipulations
suppressPackageStartupMessages(library(jsonlite)) #This will allow us to handle JSON data
suppressPackageStartupMessages(library(tibble)) #This will give us a df that is friendly with dplyr
suppressPackageStartupMessages(library(stringr)) #Use this to filter



#bring in yelp json
yelp <- stream_in(file("yelp_academic_dataset_business.json"))

#flatten yelp file
yelp_flat <- flatten(yelp)


#create tbl that can function with dplyr
yelp_tbl <- as_data_frame(yelp_flat)


#How to remove attributes. and hours. in front of colnames
colnames(yelp_tbl) <- gsub("attributes.","",paste(colnames(yelp_tbl)))
colnames(yelp_tbl) <- gsub("hours.","", paste(colnames(yelp_tbl)))

#Export CSV
write.csv(yelp_tbl, "yelp_df.csv", row.names = FALSE)



              





