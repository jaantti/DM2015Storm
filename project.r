library("data.table")

## read head to get classes, makes reading large data quicker
smalldata <- read.table("D:\\org_dwh_dump-dJH2-73e4-mw78-8ja0.out\\org_dwh_dump-dJH2-73e4-mw78-8ja0.out", sep=",", header=TRUE, nrows = 10)
classes <- sapply(smalldata, class)
header <-  names(smalldata)
col.not.na <- colSums(is.na(smalldata)) != nrow(smalldata)

data <- read.table("D:\\org_dwh_dump-dJH2-73e4-mw78-8ja0.out\\org_dwh_dump-dJH2-73e4-mw78-8ja0.out", sep=",", header=TRUE, nrows = 1000)
data2<- read.table("D:\\org_dwh_dump-dJH2-73e4-mw78-8ja0.out\\org_dwh_dump-dJH2-73e4-mw78-8ja0.out", sep=",", header=TRUE, nrows = 1000000, colClasses=classes)
data3<- read.table("D:\\org_dwh_dump-dJH2-73e4-mw78-8ja0.out\\org_dwh_dump-dJH2-73e4-mw78-8ja0.out", sep=",", header=TRUE, nrows = 1000000, skip=1000000, colClasses=classes)
data4<- read.table("D:\\org_dwh_dump-dJH2-73e4-mw78-8ja0.out\\org_dwh_dump-dJH2-73e4-mw78-8ja0.out", sep=",", header=TRUE, nrows = 2000000, skip=2000000, colClasses=classes)


names(data3) <- header

row.has.na <- apply(data, 1, function(x){any(is.na(x))})

data.streamid7 <- data2[data2$stream_id==7,]
data.streamid11 <- data2[data2$stream_id==11,]
data.streamid19 <- data2[data2$stream_id==19,]
data.streamid31 <- data2[data2$stream_id==31,]

plot(data.streamid19$mt_4, type="l", xlim=c(0,26000))

xmax <- 6000
plot(1, xlim=c(0,5500), ylim=c(0,400), type="n")
lines(data.streamid7$mt_4)
lines(data.streamid7$mt_33, lty=2, col="blue")

plot(1, xlim=c(5000,5500), ylim=c(0,400), type="n")
lines(data.streamid11$mt_4)
lines(data.streamid11$mt_33, lty=2, col="blue")

plot(1, xlim=c(0,10000), ylim=c(0,400), type="n")
lines(data.streamid19$mt_4)
lines(data.streamid19$mt_33, lty=2, col="blue")

plot(1, xlim=c(0,10000), ylim=c(0,400), type="n")
lines(data.streamid31$mt_4)
lines(data.streamid31$mt_33, lty=2, col="blue")

min(data.streamid7$mt_4)
max(data.streamid7$mt_4)
min(data.streamid11$mt_4)
max(data.streamid11$mt_4)
min(data.streamid19$mt_4)
max(data.streamid19$mt_4)
min(data.streamid31$mt_4)
max(data.streamid31$mt_4)

r_avg <- function(data){
  newdata <- vector(mode="numeric", length=length(data)-1)
  for(i in 1:(length(data)-1)){
    newdata[i] = (data[i] + data[i+1])/2.0
  }
  return(newdata)
}

jupats <- r_avg(data.streamid11$mt_4)
#jupats <- convert_overflows(jupats)

plot(1, xlim=c(8200,8500), ylim=c(min(jupats),max(jupats)), type="n")
lines(jupats)
lines(data.streamid11$mt_4, col="blue",lty=2)

###############################
### Remove spikes
##############################
remove_spikes <- function(data){
  newdata <- vector(mode="numeric", length=length(data))
  spike <- 0
  for(i in 1:(length(data)-2)){
    if(spike == 1){
      spike <- 0
      if(data[i]-data[i+1] < -340 || data[i]-data[i+1] > 340){
        newdata[i] <- (data[i]+data[i+2])/2.0
        next
      }      
    }
    if(data[i]-data[i+1] < -340){
      spike <- 1
    }
    else if(data[i]-data[i+1] > 340){      
      spike <- 1    
    }
    newdata[i] <- data[i+1]
  }
  return(newdata)
}


###############################
### modifier-counting overflow conversion
##############################
convert_overflows2 <- function(data){
  upperlim <- 330
  lowerlim <- 30
  newdata <- vector(mode="numeric", length=length(data))
  newdata <- data
  modifier = 0
  for (i in 2:(length(data))){
    if(abs(data[i]-data[i-1]) > upperlim-lowerlim){
      if(data[i-1] > upperlim && data[i]< lowerlim){
        modifier <- modifier + 1
      }
      else if (data[i-1] < lowerlim && data[i] > upperlim){
        modifier <- modifier - 1
      }
    }
    newdata[i] = data[i] + modifier * 360

  }
  return (newdata)
}

jupats <- convert_overflows2(data.streamid11$mt_4)

#jupats <- remove_spikes(data.streamid11$mt_4)
#jupats <- convert_overflows2(jupats)


xmin = 10000#0
xmax = 11000#25000
ymin = -5000#min(jupats)
ymax = 5000#max(jupats)
plot(1, xlim=c(xmin,xmax), ylim=c(ymin,ymax), type="n")
abline(h=0,lty=2)
abline(h=360, lty=2)
lines(jupats)
lines(data.streamid11$mt_4, col="blue",lty=3)

cbind(jupats[3460:3480], data.streamid11$mt_4[3460:3480])

testdata = c(1,3,5,7,9,7,5,7,9,1,3,5,7,5,3,5,7,9,1,3,5,3,1,9,7,5,3,5,7,5,3,1,9,7,5,3,1,3,1,9,7,5,7)*40
jupats <- convert_overflows2(testdata)
lines(testdata)


###############################
### another overflow conversion
##############################
convert_overflows <- function(data){
  newdata <- vector(mode="numeric", length=length(data))
  newdata[1] <- data[1]
  modifier = 0
  for (i in 2:(length(data))){
    if(data[i]-data[i-1] > 300){
      data[i] = data[i]-360
    }
    else if (data[i]-data[i-1] < -300){
      data[i] = data[i]+360
    }
    newdata[i] = data[i] + modifier * 360
  }
  return (data)
}

#jupats <- r_avg(data.streamid11$mt_4)
jupats <- convert_overflows(data.streamid11$mt_4)
jupats2 <- convert_overflows(jupats)

plot(1, xlim=c(0,20000), ylim=c(min(jupats),max(jupats)), type="n")
lines(jupats)
lines(data.streamid11$mt_4, col="blue",lty=3)
lines(jupats2, col="blue",lty=3)


head(jupats, n= 30)
head(data.streamid11$mt_4, n= 30)
tail(jupats)
tail(data.streamid11$mt_4)

data.streamid11$mt_4[2000:2050]
jupats[2000:2050]


################################
table(data2$stream_id)
table(data2$operation_uid)
table(data2$platform_deployment_id)
table(data2$t)
plot(data2$t)



#################################
data.streamid7 <- data3[data3$stream_id==7,]
data.streamid11 <- data3[data3$stream_id==11,]
data.streamid15 <- data3[data3$stream_id==15,]
data.streamid23 <- data3[data3$stream_id==23,]

################################
## simplified plotting function
################################
evilplot <- function(xdata, data, xmin=0, xmax=length(data)){
  jupats <- convert_overflows2(data)
  #xmin = 0
  #xmax = length(jupats)
  ymin = min(jupats[xmin:xmax])
  ymax = max(jupats[xmin:xmax])
  plot(1, xlim=c(xmin,xmax), ylim=c(ymin,ymax), type="n")
  abline(h=0,lty=2)
  abline(h=360, lty=2)
  #lines(data.streamid7$mt_4, col="blue",lty=3)
  lines(jupats)
  lines(data, col="blue",lty=3)
}

#################################
## eraldusjoonte kordaja funktsioon
#################################
verticalliner <- function(interval,length,lty=2,col="blue"){
  i <- 0
  while (i < length){
    i <- i + interval
    abline(v = i, lty=lty, col=col)
  }
}


#################################
## huvitavate olude identifikaator
#################################
## 

findinteresting <- function(datarow,radius,uppermod=4, lowermod=0.25){
  rowmean <- mean(datarow)
  part <- c()
  interestinglist <- c()
  while (i < 2:length(datarow)){
    if(i<= radius || i+radius > length(datarow)){
      next()
    }
    part <- datarow[(i-radius):(i+radius)]
    if(mean(part) > rowmean*3 || mean(part) < rowmean*0.33){
      interestinglist <- c(interestinglist, i)
      points(i,mean(part))
    }
  }

  return (interestinglist)
}


##################################################

data.na.omit <- data.streamid7[!is.na(data.streamid7$mt_4),]
evilplot(as.numeric(data.na.omit$t), data.na.omit$mt_4,1000,2000)
#findinteresting(data.na.omit$mt_4,radius=72)
verticalliner(144, nrow(data.na.omit))

data.na.omit <- data.streamid11[!is.na(data.streamid11$mt_4),]
evilplot(as.numeric(data.na.omit$t), data.na.omit$mt_4,125000,126000)
findinteresting(data.na.omit$mt_4,radius=72)
verticalliner(144, nrow(data.na.omit))

data.na.omit <- data.streamid7[!is.na(data.streamid7$mt_33),]
evilplot(as.numeric(data.na.omit$t), data.na.omit$mt_33,1000,2000)
verticalliner(144, nrow(data.na.omit))

data.na.omit <- data.streamid11[!is.na(data.streamid11$mt_33),]
evilplot(as.numeric(data.na.omit$t), data.na.omit$mt_33,1000,2000)
verticalliner(144, nrow(data.na.omit))


##################################################
## Beauforti skaala järgi on 24.5 m/s torm, tavakasutuses varieeruv
## meil miljoni sisse ei pruugi selliseid jääda

tormituuled <- data.streamid7[data.streamid7$mt_33 > 20,]

#plot(xlim=c(10000,11000),data.streamid7$mt_4, type="l")
plot(as.numeric(tormituuled$t),tormituuled$mt_33, type="p")
#plot(xlim=c(52130,52190),as.numeric(tormituuled$t),tormituuled$mt_33, type="p")


tormituuled <- data.streamid11[data.streamid11$mt_33 > 20,]

#plot(xlim=c(10000,11000),data.streamid7$mt_4, type="l")
plot(as.numeric(tormituuled$t),tormituuled$mt_33, type="p")
#plot(xlim=c(52130,52190),as.numeric(tormituuled$t),tormituuled$mt_33, type="p")


###########################
## dig deep and don't look back, this function is too terrible for moral eyes
###########################

filedigger <- function (filename, chunksize, totalsize){
  smalldata <- read.table(filename, sep=",", header=TRUE, nrows = 10)
  classes <- sapply(smalldata, class)
  stormdata <- list()
  stormcount <- 0
  starttime <- proc.time()
  chunkstart <- 0
  while (chunkstart <= totalsize - chunksize){
    print(c("Starting ",chunkstart, "row,",(proc.time()-starttime)[3], "s,",stormcount,"hits"))
    datachunk <- data.frame(rownames = names(smalldata))
          
    datachunk <- read.csv(filename, sep=",", header=TRUE, nrows = chunksize, skip=chunkstart, colClasses=classes)
    names(datachunk) <-  names(smalldata)
    chunkstart <- chunkstart + chunksize
    for (i in 1:nrow(datachunk)){
      try(
        if(datachunk$mt_33[i] > 20){
          stormdata <-c(stormdata, datachunk[i,])
          stormcount <- stormcount + 1
        }
      )
    }
  }
  return(stormdata)
}
#########################
# Jesus Christ, be careful, this will run a terrifyingly long time!
#########################

filename <- "D:\\org_dwh_dump-dJH2-73e4-mw78-8ja0.out\\org_dwh_dump-dJH2-73e4-mw78-8ja0.out"
stormdata <- filedigger(filename=filename, chunksize=10000, totalsize = 1000000)




###########################
## this one is not as bad
###########################

simplefiledigger <- function (filename, has.header=FALSE){
  #smalldata <- read.table(filename, sep=",", header=has.header, nrows = 10)
  #classes <- sapply(smalldata, class)
  starttime <- proc.time()
  datachunk <- read.table(filename, sep=",", header=has.header)
  names(datachunk) <-  names(smalldata)
  datachunk$t_formed <- as.POSIXct(datachunk$t, format="%Y-%m-%d %H:%M:%S", tz="GMT")
  years.table <- table(strftime(as.POSIXct(datachunk$t_formed, tz="GMT"), format="%Y.%m"))
  stormdata <- datachunk[(datachunk$mt_33 > 20) & !is.na(datachunk$mt_33),col.not.na]
  print(c("Ending ",filename, ", ",(proc.time()-starttime)[3], "s,",nrow(stormdata),"hits"))
  return(list(stormdata,years.table))
}

########################
# if we have smaller files
########################
library(data.table)

path="D:/org_dwh_dump-dJH2-73e4-mw78-8ja0.out/"
firstfile=2
lastfile=51
filenames <- list.files(path=path)
filenames <- filenames[firstfile:lastfile]


colnames <- c(header[col.not.na],"t_formed")
stormdata <- list()
stormdata <- vector("list", length(colnames))
names(stormdata) <- colnames

years <- data.frame(Var1=numeric(0),Freq=numeric(0))

hasheader <- TRUE
for (onefile in filenames){
  print(onefile)
  fileresults <- simplefiledigger(filename=paste(path,onefile,sep=""),has.header=hasheader)
  hasheader <- FALSE
  onefiledata <- fileresults[1]
  #stormdata <- c(stormdata,onefiledata)
  for(i in 1:length(colnames)){
    stormdata[[i]] <- rbind(stormdata[[i]],  onefiledata[[1]][i])
  }
  
  years.onefile <- as.data.frame(fileresults[2])
  years <- merge(years, years.onefile, all.x=TRUE,all.y=TRUE, by=c(1,2))
  years.table <- data.table(years)
  years <- as.data.frame(years.table[,list(Freq=sum(Freq)),by="Var1"])
}
#colnames <- names(smalldata[,col.not.na])
#stormdata[[1]][[2]][[1]]
#names(stormdata) <- header[col.not.na]
#head(as.data.frame(stormdata, check.rows = FALSE))

stormframe <- as.data.frame(stormdata, check.rows = FALSE)

##############
## Throw away everything before 1996 (either super stormy years or not actually data)
##############
head(stormframe)
stormframe$year <- year(as.POSIXct(stormframe$t_formed))
stormframe$month <- month(as.POSIXct(stormframe$t_formed))

## Throw away everything before 1996 (either super stormy years or not actually data)

stormframe <- stormframe[!stormframe$year<1996,]
years <- years[!years$year<1996,]

stormframe$unixtime <- as.numeric(as.POSIXct(stormframe$t_formed))
stormframe <- stormframe[order(stormframe$unixtime),]

## identify storms
storms <- data.frame(stormnumber=numeric(0),stormstart=numeric(0),stormend=numeric(0))
storm.number <- 1
stormstart <- stormframe[1,"unixtime"]
for (i in 1:(nrow(stormframe)-1)){
  thispoint <- stormframe[i,]
  if(abs(stormframe[i,"unixtime"] - stormframe[i+1,"unixtime"]) < 60*60*12){
    ## same storm
  }
  else{
    stormend <- stormframe[i,"unixtime"]
    thisstorm <- data.frame(stormnumber=storm.number, stormstart=stormstart, stormend=stormend)
    storm.number <- storm.number + 1
    stormstart <- stormframe[i+1,"unixtime"]
    storms <- rbind(storms,thisstorm)
  }
}


storms$length_hour <- (storms$stormend-storms$stormstart) / (60*60)
storms$startdate <- as.POSIXct(storms$stormstart, origin="1970-01-01", tz="GMT")

head(storms)






#################################

ym <- strsplit(as.character(years$Var1),split="\\.")
unixtimes <- as.numeric(as.POSIXct(gsub("\\.","-",paste(years$Var1,"-01",sep="")), format="%Y-%m-%d",tz="GMT"))
for (i in 1:length(ym)){
  years[i,"year"] <- as.numeric(ym[[i]][[1]])
  years[i,"month"] <- as.numeric(ym[[i]][[2]])
  years[i,"unixtime"] <- unixtimes[[i]]
}
years <- years[order(as.numeric(years$year),as.numeric(years$month)),]







##################
## Plot all measurement count by month
#################

plot(years$unixtime,years$Freq, xaxt="n", main="Measurement distribution by month",xlab="Date",ylab="Measurement count")
axis(1,at= years$unixtime, labels=years$Var1)

## 1000x400px seemed reasonable size for graphs ##



years$y <- 1:264 * (2013.42-1989.5)/264 + 1989.5
plot(years$y,years$Freq)


#randomstart <- runif(1,0,nrow(stormframe))
#hist(as.numeric(format(as.Date(stormframe$t, format="%Y-%m-%d %H:%M:%S"), "%Y")))














unixyearmonth <- as.numeric(as.POSIXct(gsub("\\.","-",paste(stormframe$year,"-",stormframe$month, "-01",sep="")), format="%Y-%m-%d",tz="GMT"))
stormframe$unixyearmonth <- unixyearmonth
stormframe$one <- 1
storms.table <- data.table(stormframe)
storms.bymonth <- as.data.frame(storms.table[,list(count=sum(one)),by="unixyearmonth"])

mergedvals <- merge(years, storms.bymonth, by.x="unixtime",by.y="unixyearmonth", all.x=TRUE,all.y=FALSE)
mergedvals <- mergedvals[c(1,2,3,8)]
names(mergedvals) <- c("unixtime","year.month","totalcount","stormcount")
mergedvals[is.na(mergedvals$stormcount),"stormcount"] <- 0
mergedvals$fraction <- mergedvals$stormcount/mergedvals$totalcount

#################
## storm distribution graph by months
#################
xlim <- c(min(mergedvals$unixtime),max(mergedvals$unixtime))
ylim <- c(min(mergedvals$fraction),max(mergedvals$fraction))
plot(1, type="n", xaxs="i", xaxt="n", main="Fraction of storm points per month",
     xlab="Date",ylab="Storms per month (%)",
     xlim=xlim, ylim=ylim)
points(mergedvals$unixtime,mergedvals$fraction,pch=20)
axis(1,at= mergedvals$unixtime, labels=mergedvals$year.month)
yearstarts <- as.numeric(as.POSIXct(gsub("\\.","-",paste(seq(from=1996,to=2012),"-01-01",sep="")), format="%Y-%m-%d",tz="GMT"))
abline(v=yearstarts,lty=3,col="gray")
abline(h=seq(from=0, to=0.2, by=0.002),lty=3,col="gray")




#######################
## counts of storm points
#######################
xlim <- c(min(mergedvals$unixtime),max(mergedvals$unixtime))
ylim <- c(min(mergedvals$stormcount),max(mergedvals$stormcount))
plot(1, type="n", xaxs="i", xaxt="n", main="Storm count by month",
     xlab="Date",ylab="Number of storm measurements",
     xlim=xlim, ylim=ylim)
points(mergedvals$unixtime,mergedvals$stormcount,pch=20)
axis(1,at= mergedvals$unixtime, labels=mergedvals$year.month)
yearstarts <- as.numeric(as.POSIXct(gsub("\\.","-",paste(seq(from=1996,to=2012),"-01-01",sep="")), format="%Y-%m-%d",tz="GMT"))
abline(v=yearstarts,lty=3,col="gray")
abline(h=seq(from=0, to=500, by=50),lty=3,col="gray")




#################
## storm distribution graph by year
#################

storms.table <- data.table(stormframe)
storms.byyear <- as.data.frame(storms.table[,list(count=sum(one)),by="year"])

years$one <- 1
years.table <- data.table(years)
allcount.byyear <- as.data.frame(years.table[,list(allcount=sum(Freq)),by="year"])

mergedvals <- merge(allcount.byyear, storms.byyear, by.x="year",by.y="year", all.x=TRUE,all.y=FALSE)
mergedvals <- mergedvals[c(1,3,7)]
names(mergedvals) <- c("year","totalcount","stormcount")
mergedvals[is.na(mergedvals$stormcount),"stormcount"] <- 0
mergedvals$year <- as.numeric(mergedvals$year)
class(mergedvals$year)="numeric"
mergedvals$fraction <- mergedvals$stormcount/mergedvals$totalcount*100

xlim <- c(min(mergedvals$year),max(mergedvals$year))
ylim <- c(min(mergedvals$fraction),max(mergedvals$fraction))
plot(1, type="n", xaxs="i", xaxt="n", main="Fraction of storm points per year",
     xlab="Date",ylab="Storms per year (%)",
     xlim=xlim, ylim=ylim)
lines(mergedvals$year,mergedvals$fraction)
points(mergedvals$year,mergedvals$fraction,pch=20)
axis(1,at= mergedvals$year, labels=mergedvals$year)
yearstarts <- as.numeric(as.POSIXct(gsub("\\.","-",paste(seq(from=1996,to=2012),"-01-01",sep="")), format="%Y-%m-%d",tz="GMT"))
#abline(v=yearstarts,lty=3,col="gray")
#abline(h=seq(from=0, to=0.2, by=0.002),lty=3,col="gray")










# 17 omadust

interestingdata <- data.frame()



interestingcols <- c(2,13,15,16)
col.count <- 17
files.count <- 4
filestart <- 0
#allinterestingcols <- seq(from = 0, by=col.count, to=col.count*files.count)+interestingcols

filecolstarts <- seq(from = filestart*col.count, by=col.count, to=col.count*files.count + filestart*col.count)

for (filecol in filecolstarts){
  thiscols <- interestingcols + filecol
  interestingdata <- rbind(interestingdata, as.data.frame(stormdata[thiscols])) 
}




interesting.small <- interestingdata[10000:50000,]

plot(xlim=c(0,5000),as.numeric(interesting.small$t), interesting.small$mt_33,type="p")

hist(as.numeric(format(as.Date(interesting.small$t, format="%Y-%m-%d %H:%M:%S"), "%Y")))

hist(as.numeric(format(as.Date(interestingdata$t, format="%Y-%m-%d %H:%M:%S"), "%Y")))

#https://www.overleaf.com/2764835xvxzxs