#getwd(): showing directory, dir.create("testdir"): crete directory
#setwd("testdir"): set directory, 
#?functionname or ?':': to get help

#creating vector, list, matrix-rbind &cbind, factor
x<-c(1,2,3)
names(x)<-c("birinci","ikinci","ucuncu")
y<-list(1,TRUE,"isil")
z<-matrix(1:6,2,3)
g<-matrix(1:9, byrow = TRUE, nrow = 3)
rownames(g)<-c("birinci","ikinci","ucuncu")
rbind(x,z)
#rowSums, colSums
m<-factor(c("yes","no","no","yes")) 
temperature_vector<-c("Low", "Medium", "High")
factor_temperature_vector <- factor(temperature_vector, order = TRUE, levels = c("Low", "Medium", "High"))
#changing levels: levels(factor_survey_vector) <-c("Female", "Male")
#data frames:each column is an element and each row is the length of the elements
d<-data.frame(foo=1:4,bar=c(T,T,F,F))
#to get a column: matrix[,3] or matrix$nameofthecolumn
#to select subset: subset(planets_df, subset=diameter<1)
#ordering data: positions <-  order(planets_df$diameter), planets_df[positions,]

#is.na() and is.nan(): test whether data has na or nan
#read.table() separator is space, read.csv() separator is comma
#ls(): listing elements, rm(x,y): removing, rm(list=ls()): removing all

#x*y elementwise multiplication, x%*%y matrix multiplication
#seq(0,10,by=0.5), seq(5, 10, length=30), length(x),
#rep(0,times=40), rep(c(0,1,2),times=10), rep(c(0, 1, 2), each = 10)
#which(matrix>5) gives indices of matrix elements that are greater than 5

#logical: >, <=, ==, !=, A|B, A&B, !A, xor T,F>TRUE otherwise FALSE
#to get only TRUE: selection<-X>0, then x[selection]