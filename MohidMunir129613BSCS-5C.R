#TASK1
data("AirPassengers")
AirPassengers
apmat <- matrix(AirPassengers, nrow=12,byrow=TRUE)
colnames(apmat) <- c("Janauary","February","March","April","May","June","July","August","September","October","November","December")
rownames(apmat) <- c("1949","1950","1951","1952","1953","1954","1955","1956","1957","1958","1959","1960")
max(apmat)
mnthname= which(apmat == max(apmat), arr.ind=TRUE)
cx = colnames(apmat)[mnthname[,2]]
rx = rownames(apmat)[mnthname[,1]]
cat("The most profitable month is",cx ,"with",max(apmat),"passengers.")
apmat2 <- apply(t(apmat),2,cumsum)
max(apmat2,12)
cat("The most profitable year is",rx ,"with",max(apmat2,12),"passengers.")
ts.plot(AirPassengers)

#TASK2
p=8000
for(i in 1:12)
{
  for(j in 1:12)
  {
    apmat3=apmat*p
  }
  p=p*1.1
}
colnames(apmat3) <- c("Janauary","February","March","April","May","June","July","August","September","October","November","December")
rownames(apmat3) <- c("1949","1950","1951","1952","1953","1954","1955","1956","1957","1958","1959","1960")
max(apmat3)
mnthname= which(apmat3 == max(apmat3), arr.ind=TRUE)
cz = colnames(apmat3)[mnthname[,2]]
rz = rownames(apmat3)[mnthname[,1]]
cat("The month ",cz ,"is with most revenue",max(apmat3))
apmat4 <- apply(t(apmat3),2,cumsum)
max(apmat4,12)
cat("The year ",rz ,"with most revenue",max(apmat4,12))
sum(apmat3)
cat("Total revenue is",sum(apmat3))

#TASK 3
plot(decompose(AirPassengers))
