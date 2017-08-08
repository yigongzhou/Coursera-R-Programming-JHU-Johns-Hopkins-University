##pollutantmean

pollutantmean <- function(directory, pollutant, id = 1:332){
  
  # initial an empty dataframe to combine
  datall<- read.csv(paste("C:/Users/Yigong Zhou/Desktop/specdata","/001.csv",sep=""))
  datall<- datall[0,]
  
  # read different csv files
  for (i in id){
    if (i < 10) {
      datall <- rbind(datall,read.csv(paste(directory,"/00", i,".csv", sep="")))
    }
    if (i < 100 & i > 9){
      datall <- rbind(datall,read.csv(paste(directory,"/0", i,".csv", sep="")))  
    }
    if (i >99){
      datall <- rbind(datall,read.csv(paste(directory,"/", i,".csv", sep="")))  
    }  
  }
  
  # use colMeans to calculate mean, remanber to use na,rm to remove NAs
  colMeans(datall[pollutant],na.rm= TRUE)
}

#test pollutantmean
pollutantmean(directory="C:/Users/Yigong Zhou/Desktop/specdata", pollutant= "nitrate")

#test pollutantmean
pollutantmean(directory="C:/Users/Yigong Zhou/Desktop/specdata", pollutant="sulfate",id = 34)


##complete

complete <- function(directory, id= 1:332){
 
 #same as in pollutantmean()
 datall <- read.csv(paste("C:/Users/Yigong Zhou/Desktop/specdata","/001.csv",sep=""))
  datall<- datall[0,]
  for (i in id){
    if (i < 10) {
      datall <- rbind(datall,read.csv(paste(directory,"/00", i,".csv", sep="")))
    }
    if (i < 100 & i > 9){
      datall <- rbind(datall,read.csv(paste(directory,"/0", i,".csv", sep="")))  
    }
    if (i >99){
      datall <- rbind(datall,read.csv(paste(directory,"/", i,".csv", sep="")))  
    }  
  }
  
  #use complete.cases() to select rows with full records, dont forget ,
  datall<-datall[complete.cases(datall),]
  
  #initial a data frame for fianl report
  com <- data.frame(comid=vector(),nobs=numeric())
  
  #construct rows with numbers and use nrow() to count the total
  #then combine them together
  for (i in id){
    new<-c(i,nrow(datall[which(datall$ID==i),]))
    com<-rbind(com,new)
  }
  
  #report com 
  com
}

#test complete
cc <- complete("C:/Users/Yigong Zhou/Desktop/specdata",54)

#test complete
set.seed(42)
cc <- complete("C:/Users/Yigong Zhou/Desktop/specdata", 332:1)
use <- sample(332, 10)
print(cc[use, 2])


## corr
corr<- function(directory, threshold = 0){

  #same as in pollutantmean()
  datall <- read.csv(paste("C:/Users/Yigong Zhou/Desktop/specdata","/001.csv",sep=""))
  datall<- datall[0,]
  for (i in 1:332){
    if (i < 10) {
      datall <- rbind(datall,read.csv(paste("C:/Users/Yigong Zhou/Desktop/specdata","/00", i,".csv", sep="")))
    }
    if (i < 100 & i > 9){
      datall <- rbind(datall,read.csv(paste("C:/Users/Yigong Zhou/Desktop/specdata","/0", i,".csv", sep="")))  
    }
    if (i >99){
      datall <- rbind(datall,read.csv(paste("C:/Users/Yigong Zhou/Desktop/specdata","/", i,".csv", sep="")))  
    }  
  }
  datall<-datall[complete.cases(datall),]

  #use ave() to add a vecter indicate the count of other columns, very useful
  datall$count<-as.numeric(ave(datall$ID,datall$ID,FUN = length))
  
  #initial variables
  corvec<-numeric()
  uniqid<-numeric()
  
  # first, construct a vector to list all id that the count are higher than threshold
  k<-as.numeric(unique(datall[datall$count>threshold,]$ID))
  
  # use loop to calculate only ids that meeted the aboved requirement
  for (i in k){
    corvec[i]<-as.numeric(cor(datall[datall$ID==i,][,2:3])[2,1])
  }
  corvec
}

#test corr
cr <- corr("C:/Users/Yigong Zhou/Desktop/specdata/")                
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

#test corr
cr <- corr("C:/Users/Yigong Zhou/Desktop/specdata/", 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

#test corr
cr <- corr("C:/Users/Yigong Zhou/Desktop/specdata/", 2000)                
n <- length(cr)                
cr <- corr("C:/Users/Yigong Zhou/Desktop/specdata/", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
