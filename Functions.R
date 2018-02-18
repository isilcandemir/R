#if-else if-else form
if (x<0) {
  print ("x is a negative number")
} else if (x==0){
    print ("x is zero")
} else {
    print ("x is a positive number")
}

#while-break form
while (speed > 30) {
  print(paste("Your speed is", speed))
  if ( speed>80) {
    break
  }
  if (speed > 48) {
    print("Slow down big time!")
    speed <- speed - 11
  } else {
    print("Slow down!")
    speed <- speed - 6
  }
}

#while-break form
i<-1
while (i <= 10) {
  print(3 * i)
  if ( (3 * i) %% 8 == 0) {
    break
  }
  i <- i + 1
}

#for form
cities<-c("New York","Paris", "London","Tokyo","Rio de Janerio", "Cape Town")
for(i in 1:length(cities)){
  print(paste(cities[i],"is on position", i, "in the cities vector"))
}

#for form
ttt<-matrix(c("O",NA,"X",NA,"O","O","X",NA,"X"), byrow=TRUE, nrow=3)
for (i in 1:nrow(ttt)) {
  for (j in 1:ncol(ttt)) {
    print(paste("On row",i,"and column", j, "the board contains", ttt[i,j]))
  }
}

#writing a function
my_fun<-function(a,b){
  a+b
}
