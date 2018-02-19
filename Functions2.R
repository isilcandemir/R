#using lapply/sapply/vapply
names <- lapply(split_low, function(x){x[1]})

#regular expressions: grepl(pattern="a", x=animals) logical outcome,"^ a" or "a$"
#grep: gives indices
#sub: change the first variable: sub(pattern="a|i", replacement="o", x=animals)
#gsub: change every element

#Sys.Date(), Sys.time()
#%Y:1982, %y:82, %m:01, %d:13, %A:Wednesday, %a:Wed, %B:January, %b:Jan
#as.Date("1982-01-13"), as.Date("Jan-13-82", format = "%b-%d-%y")
#as.Date("13 January, 1982", format = "%d %B, %Y")
#format(Sys.Date(), format = "Today is a %A!")

#as.POSIXct(), %H or %I:(00-23), %M: minutes, %S: seconds
#%T: shorthand notation of %H:%M:%S, %p:AM/PM indicator 
#now <- Sys.time(), now + 3600: adding an hour, now - 3600*24:subtracting a day