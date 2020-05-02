#Rank - Ranking of overall sales
#Name - The games name
#Platform - Platform of the games release (i.e. PC,PS4, etc.)
#Year - Year of the game's release
#Genre - Genre of the game
#Publisher - Publisher of the game
#NA_Sales - Sales in North America (in millions)
#EU_Sales - Sales in Europe (in millions)
#JP_Sales - Sales in Japan (in millions)
#Other_Sales - Sales in the rest of the world (in millions)
#Global_Sales - Total worldwide sales.

setwd("D:\\databases\\spain_stock_market")
getwd()
vg<-read.csv(file.choose())
head(vg)
vg$Rank <- factor(vg$Rank)
summary(vg)
str(vg)


library(ggplot2)
install.packages("ggplot2")

vg[which(vg$Year== NA),]

#-------------------SECTION 1 Whole DATA

#----------------------Count by Genre
#-----------------------How to display percent
t<-ggplot(data = vg)
?geom_bar()
t+ geom_bar(aes(x=Genre))
t+ geom_bar(aes(x=Genre,y=(..count..)/sum(..count..)))
rm(t)
total

#-----Count the number of games
length(which(vg$Publisher=="Nintendo"))


# ggplot(vg, aes(x=Genre)) + 
#   geom_bar(stat='identity') + 
#   geom_text(aes(label=scales::percent(pct)),
#             position = position_stack(vjust = .5))+
#   scale_y_continuous(labels = scales::percent)



#---------------------------------SECTION 2 TOP 100
1:10
#-------------------Bar Graph Count By Genre
top100<-head(vg,100)
g<- ggplot(top100)
g+ geom_bar(aes(x=Genre),stat = "count") 

g + geom_bar(aes(x=Publisher))


#------------------Stats before and after removing an outlier
mean(top100$Global_Sales)
mean(top100[!(top100$Name == "Wii Sports"),"Global_Sales"])

median(top100$Global_Sales)
median(top100[!(top100$Name == "Wii Sports"),"Global_Sales"])

quartiles<-quantile(top100$Global_Sales,c(0.25,0.75))
IqrTop100GlobalSales<-quartiles[2]-quartiles[1]
quartiles
rangeTop100GlobalSales<-max(top100$Global_Sales)-min(top100$Global_Sales)

#-------------------Co Relation among Sales

g+ geom_point(aes(x=Global_Sales,y=NA_Sales, colour=Publisher))
?geom_point()
g+ geom_point(aes(x=Global_Sales,y=EU_Sales, colour=Publisher))

g+ geom_point(aes(x=Global_Sales,y=Other_Sales, colour=Publisher))

g+ geom_point(aes(x=EU_Sales,y=NA_Sales,colour=Publisher,size=Global_Sales))

g+ geom_point(aes(x=EU_Sales,y=Other_Sales,colour=Publisher,size=Global_Sales))

g+ geom_point(aes(x=NA_Sales,y=Other_Sales,colour=Publisher,size=Global_Sales))

g+ geom_point(aes(x=JP_Sales,y=Other_Sales,colour=Publisher,size=Global_Sales))

g+ geom_point(aes(x=JP_Sales,y=NA_Sales,colour=Publisher,size=Global_Sales))

g+ geom_point(aes(x=JP_Sales,y=EU_Sales,colour=Publisher,size=Global_Sales))


g+ geom_point(aes(x=NA_Sales,y=EU_Sales,
                  colour=Platform,size=Global_Sales))

top100[top100$NA_Sales>20,]

#----------------Earning by Publisher
#Nintendo, Microsoft, Take-Two, Activision

salesnin<-top100[top100$Publisher=="Nintendo",
       c("NA_Sales","EU_Sales","JP_Sales","Global_Sales") ]
salesmicro<-top100[top100$Publisher=="Microsoft Game Studios",
                   c("NA_Sales","EU_Sales","JP_Sales","Global_Sales") ]
salestaket<-top100[top100$Publisher=="Take-Two Interactive",
                   c("NA_Sales","EU_Sales","JP_Sales","Global_Sales") ]
salesacti<-top100[top100$Publisher=="Activision",
                   c("NA_Sales","EU_Sales","JP_Sales","Global_Sales") ]
#------------
q<- ggplot(data = salesmicro,aes(x=NA_Sales,
                                 y=EU_Sales,
                                 size=Global_Sales))
q + geom_point()+facet_grid(.~JP_Sales)

q<- ggplot(data = salesacti,aes(x=NA_Sales,
                                 y=EU_Sales,
                                 size=Global_Sales))
q + geom_point()+facet_grid(.~JP_Sales)

q<- ggplot(data = salesnin,aes(x=NA_Sales,
                                 y=EU_Sales,
                                 size=Global_Sales))
q + geom_point()+facet_grid(.~JP_Sales)

q<- ggplot(data = salestaket,aes(x=NA_Sales,
                                 y=EU_Sales,
                                 size=Global_Sales))
q + geom_point()+facet_grid(.~JP_Sales)
#--------------------------------------------

mean(salesacti$JP_Sales)
mean(salesmicro$JP_Sales)
mean(salesnin$JP_Sales)
mean(salestaket$JP_Sales)

median(salesacti$JP_Sales)
salesacti$JP_Sales

top100[top100$JP_Sales==0.65,]


summary(top100[top100$JP_Sales>3.5,"Genre"])
#--------------------------Platforms Detailing

g+ geom_bar(aes(x=Platform,fill=Publisher))

top100$Publisher=="Nintendo"

g + geom_point(aes(x=NA_Sales,y=Global_Sales))+
  facet_grid(Publisher~Platform)


#----------------------Little Depth---------------------------------

top100[(top100$NA_Sales> 20 & top100$NA_Sales<30),]
top100[(top100$Publisher=="Take-Two Interactive"),]

top100[(top100$Other_Sales>5),]
str(top100)
#-----------Single Digit Global Sales
nrow(top100[(top100$Global_Sales<10),])


#--------------------Standardising the sales

top100$stdNA<- (top100$NA_Sales/sum(top100$NA_Sales))
top100$stdEU<- (top100$EU_Sales/sum(top100$EU_Sales))
top100$stdJP<- (top100$JP_Sales/sum(top100$JP_Sales))
top100$stdGlobal<- (top100$Global_Sales/sum(top100$Global_Sales))
typeof(top100$stdNA)
standarisedSales<-c()

r <- ggplot(data=top100,aes(x=stdGlobal,
                            y=stdNA,colour=Publisher))
r+geom_point() 
#------------------------Extra section
nin<- vg[vg$Publisher=="Nintendo",]
h<- ggplot(data = nin)
g + geom_point(aes(x=Year,y=Global_Sales,colour=Publisher))

top100[top100$Genre=="Action",]
NinGen<-top100[(top100$Publisher=="Nintendo","Genre"]
summary(NinGen)
top100[(top100$Publisher=="Nintendo" & top100$Genre=="Platform"),]

top100[(top100$Publisher=="Nintendo" & top100$Genre=="Role-Playing"),]

top100[(top100$Publisher=="Nintendo" & top100$Genre=="Racing"),]

top100[(top100$Publisher=="Nintendo" & top100$Genre=="Misc"),]

top100[(top100$Publisher=="Nintendo" & top100$Genre=="Puzzle"),]

top100[(top100$Publisher=="Nintendo" & top100$Rank==c(1:10)),]

h+ geom_point(aes(x=Year,y=Global_Sales))

top100[top100$Genre=="Shooter",]
nrow(top100[top100$Genre=="Shooter",])
top100gen<-top100$Genre
summary(top100gen)
shooterPublish<- top100[top100$Genre=="Shooter","Publisher"]
summary(shooterPublish)

