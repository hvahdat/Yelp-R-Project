#Simple example that shows how to use SQL type commands in R

suppressPackageStartupMessages(library(sqldf))

dfsql <- sqldf("select name, count(name)
               from df
               group by name
               order by count(name) desc")