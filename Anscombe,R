###################################
# Anscombe quartet
##################################

#empty R - workspace

rm(list=ls())

#load required Libs

require(dplyr)
require(ggplot2)
require(htmlTable)

#create a data set to work with

mydata=with(anscombe,data.frame(x=c(x1,x2,x3,x4), y=c(y1,y2,y3,y4), group=gl(4,nrow(anscombe))))

#show the data in table format

output<-matrix(nrow=11,ncol=8)

output[,1]<-mydata[mydata$group=="1",]$x
output[,2]<-mydata[mydata$group=="1",]$y
output[,3]<-mydata[mydata$group=="2",]$x
output[,4]<-mydata[mydata$group=="2",]$y
output[,5]<-mydata[mydata$group=="3",]$x
output[,6]<-mydata[mydata$group=="3",]$y
output[,7]<-mydata[mydata$group=="4",]$x
output[,8]<-mydata[mydata$group=="4",]$y

htmlTable(txtRound(output,2),align="r",
          header =  c("X", "Y","X", "Y", "X", "Y", "X", "Y"),
          cgroup =  c("1", "2","3","4"),
          n.cgroup = c(2,2,2,2), 
          tfoot="Anscombe's Quartet data series",
          css.cell = "padding-left: .5em; padding-right: .2em;")


# pressent summury data 

table<-as.data.frame(mydata %>%
  group_by(group) %>%
  summarize(mean(x), sd(x), mean(y), sd(y), cor(x,y)))

output<-matrix(nrow=5,ncol=4)
output[1,]<-table[,2]
output[2,]<-table[,3]
output[3,]<-table[,4]
output[4,]<-table[,5]
output[5,]<-table[,6]

htmlTable(txtRound(output,3),align="r",
          header =  c("1", "2","3", "4"),
          rnames = c("Mean X", "Standard Deviation X", "Mean Y", "Standard Deviation Y", "Correlation"),
          tfoot="Summary Statistics for Anscombe's Quartet",
          css.cell = "padding-left: .5em; padding-right: .2em;")
  
#build the plot  

theme_set(theme_bw(base_size=18))

ggplot(mydata, aes(x, y)) + 
  geom_point(size=5, color="red", fill="orange", shape=21) + 
  geom_smooth(method="lm", fill=NA, fullrange=TRUE) + 
  facet_wrap(~group, ncol=2)
