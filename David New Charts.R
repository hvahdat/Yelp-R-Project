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


pie[nrow(pie) + 1,] <- list(as.character("Other"),as.integer("687"), as.numeric("47.379310"), as.integer("1")) #Insert row

pie(pie$`count(name)`,pie$name)
#Plot pie chart with 23 Gas Station names and 1 "Other" 
pie(pie$percent,pie$name,radius=1,main="Names of Gas Stations",init.angle=180) 

#Return bar chart outlining star distribution of data
star.plot<-qplot(stars, data = df, geom = "bar", fill = I("red"), color = I("black"), 
                 alpha = I(0.25), main = "Yelp Star Distribution for Gas Stations", 
                 xlab = "Stars", ylab = "Count") 
star.plot<- star.plot + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

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

top20.plot<-top20.plot +theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "Top 20 Cities for Gas Stations Bar Chart.pdf",plot = top20.plot)      #Save top20.plot as pdf file
ggsave(filename = "Gas Station Star Distribution.pdf",plot = star.plot)                  #Save star.plot as pdf file
png(file = "Names of Gas Stations.png",width = 1020,height = 824)                        #Save pie chart
pie(pie$percent,pie$name,radius=1,main="Names of Gas Stations",init.angle=180)           #in png file
dev.off()
