#Count max amount of categories listed for a single row
max(na.omit(count.fields(textConnection(yelp_tbl$categories), sep = ",")))

#Print unique values in each column
for (i in 1:ncol(yelp_tbl)) {
 print(unique(yelp_tbl[i]))
}

