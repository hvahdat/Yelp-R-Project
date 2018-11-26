# Clear workspace
rm(list = ls())

# Load packages
suppressPackageStartupMessages(library(dplyr)) #This will help us do simple manipulations
suppressPackageStartupMessages(library(jsonlite)) #This will allow us to handle JSON data
suppressPackageStartupMessages(library(tibble)) #This will give us a df that is friendly with dplyr
suppressPackageStartupMessages(library(stringr)) #Use this to filter
suppressPackageStartupMessages(library(tm)) #This will help us with text mining

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

# Subsets yelp.tbl by category returns a dataframe

cat.subset<-function(category=""){
    df<-yelp_tbl[grep(category,yelp_tbl$categories),]
 return(df)
   }







