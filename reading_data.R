#if we don't know path, reading a csv file, separated by comma
path <- file.path("~", "Desktop", "swimming_pools.csv")
read.csv(path, stringsAsFactors = FALSE)

#if file separated by tab (tab-delimited format) in txt format
read.delim(path, stringsAsFactors = FALSE, header=FALSE)

#if file separated by tab, or slash, or different forms
read.table(path, sep = "\t", col.names = c("type", "calories", "sodium"))

#readr package: read.csv>read_csv, read.table>read_delim, read.delim>read.tsv
#data.table package

#readxl package
read_excel(path, sheet = 2)
#gdata package
#XLConnect package

#reading http file
read_csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_1478/datasets/swimming_pools.csv")
