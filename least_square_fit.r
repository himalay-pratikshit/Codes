#least square fit

dat=read.csv(file="values.csv",header=TRUE )
data= as.data.frame(dat)
print(data)

y_bar=mean(data$y)

n=as.numeric(nrow(data))
print(paste("N = " , n))

x_squ=as.numeric((data$x)^2)
xy=as.numeric(data$x * data$y)
xsum=as.numeric(sum(data$x))
ysum=as.numeric(sum(data$y))
x_squ_sum=as.numeric(sum(x_squ))
xy_sum=as.numeric(sum(xy))

delta=((n * x_squ_sum) - ((xsum)^2))

slope=((n * (xy_sum ))- ( xsum * ysum ))/delta
print(paste( "slope = ", slope))

intercept= ((ysum * x_squ_sum )- ( xsum * xy_sum))/ delta
print(paste( "intercept = ", intercept))
print("The best fit line using least square fit method --> ")
print(paste(" y =",slope,"x + ",intercept))

yofx=c((slope * data$x) + intercept)
yofxybarsq=c((yofx - y_bar)^2)
yybar=c(( data$y - y_bar)^2)
yofxybarsq_sum=as.numeric(sum(yofxybarsq))
yybar_sum=as.numeric(sum(yybar))

reg= ((yofxybarsq_sum)/ (yybar_sum))
print(paste("r^2 = ", reg))

plot( data$x, data$y, col="red")
lines(data$x, yofx, col = "green")
