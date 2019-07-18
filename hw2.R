#Data Visualization
# vgacutan

library(ggplot2)
library(plyr) # i use this to group the data by state and calculate the correlation of the variable so I can display the correllation (cors) numbers in the plot.
data(midwest)
data(diamonds)

#1. Professional Employment by State
midwest$AdultProf = as.integer((midwest$percprof / 100 ) * midwest$popadults)
mw = midwest[c('state', 'county', 'poptotal', 'AdultProf')]
sumPopTotal = aggregate(mw$poptotal, mw["state"], function(mw) sum(mw))
sumPopAdTotal = aggregate(mw$AdultProf, mw["state"], function(mw) sum(mw))
mdf = cbind(sumPopTotal, sumPopAdTotal[2])
names(mdf) = c("state", "sumPopTotal", "AdultProfTotal")
mdf = transform(mdf, percentAdltProf = (AdultProfTotal / sumPopTotal) * 100)

plt = ggplot(data=mdf, aes(x=state, y=percentAdltProf, fill=state)) + 
       geom_bar(colour="black", fill ="#009E73", width=.8, stat="identity") + 
       xlab("state") + ylab("Percent of Adult with Professional Employment") +
       ggtitle("State Vs Percent of Adult with Professional Employment")
print(mdf)
print(plt)


SumpopTotal = aggregate(midwest$poptotal, midwest["state"], function(midwest) sum(midwest,na.rm=T))
GrndPopTotal = sum(midwest$poptotal)
res = cbind(SumpopTotal,GrndPopTotal,mdf[4])
names(res) = c("state", "SumpopTotal", "GrndPopTotal", "AvgAdultPercprof")
res=transform(res, PopsPrcnt = (SumpopTotal / GrndPopTotal)*100)
resplt = ggplot(data=res, aes(x=state, y=PopsPrcnt, fill=state)) + 
          geom_bar(colour="black", fill ="#0072B2", width=.8, stat="identity") + 
          xlab("state") + ylab("Percent of Percent total Pops") +
        ggtitle("Percent of State Total Pops in Midwest")
print(res)
print(resplt)

#2. School and College Education by State
library(plyr) # i use this to group the data by state and calculate the correlation of the variable so I can display the correllation (cors) numbers in the plot.
p <- ggplot(midwest, aes(perchsd, percollege, fill= state)) + geom_point() + geom_smooth(method = "lm") + facet_grid(.~state) + labs(title="percentage of people with a High School diploma (perchsd) Vs \n percentage of college educated population (percollege) Vs state")
cors <- ddply(midwest, c("state"), summarise, cor = round(cor(perchsd, percollege), 2))
plt = p + geom_text(data=cors, aes(label=paste("r=", cor, sep="")), x=70, y=40)
print(plt)

#3 Comparison of Visualization Techniques
n100 = rnorm(100, mean = 2 )
n1000 = rnorm(1000, mean = 5 )
df=data.frame(n100)
df1 = data.frame(n1000)

p3plt1 = ggplot(df, aes(x=factor(0), y=n100)) + geom_boxplot() + ggtitle("Q3 Box Plot rnorm n = 100")
print(p3plt1)
print(p3plt2)

#4. Random Scatterplots
# call fileSave(n) function where n >= 1000 to get a more readable output.
# I use the same random numbers for both x,y since we are not really getting the correllation 
# but getting the impact of the file sizes with the increase in the value of random numbers.  
# I'm still learning R, I admit this are not the best optimized codes.
fileSave = function(n, l = 20){
  df = NULL
  df1 = NULL
  df2 = NULL
  df3 = NULL
  df4 = NULL
  if(n<1000){
    print("Minimum of 1,000 random uniform numbers to get a descent plot")
  } else {
    for (i in seq(100, n, length = l)){
      x = runif(i)
      y = runif(i)
      z = data.frame(x,y)
      plttitle = paste("xyScatter_jpg", i)
      p = ggplot(z, aes(x=x, y=y)) + geom_point() + labs(title = plttitle)
      ggsave(filename = file.path(getwd(), paste("XYSctrPlot_", as.integer(i) , ".jpg", sep = "")), p)
    }
    for (i in seq(100, n, length = l)){
      x = runif(i)
      y = runif(i)
      z = data.frame(x,y)
      plttitle = paste("xyScatter_png", i)
      p = ggplot(z, aes(x=x, y=y)) + geom_point()
      ggsave(filename = file.path(getwd(), paste("XYSctrPlot_", as.integer(i) , ".png", sep = "")), p)
    }
    for (i in seq(100, n, length = l)){
      x = runif(i)
      y = runif(i)
      z = data.frame(x,y)
      plttitle = paste("xyScatter_pdf", i)
      p = ggplot(z, aes(x=x, y=y)) + geom_point() + labs(title = plttitle)
      ggsave(filename = file.path(getwd(), paste("XYSctrPlot_", as.integer(i) , ".pdf", sep = "")), p)
    }
    for (i in seq(100, n, length = l)){
      x = runif(i)
      y = runif(i)
      z = data.frame(x,y)
      plttitle = paste("xyScatter_ps", i)
      p = ggplot(z, aes(x=x, y=y)) + geom_point() + labs(title = plttitle)
      ggsave(filename = file.path(getwd(), paste("XYSctrPlot_", as.integer(i) , ".ps", sep = "")), p)
    }
    for (i in seq(100, n, length = l)){
      data = file.info(file.path(getwd(),paste("XYSctrPlot_", as.integer(i) , ".png", sep = "")))$size
      df1 = rbind(df1, c(i,data))
      
    }
    for (i in seq(100, n, length = l)){
      data1 = file.info(file.path(getwd(),paste("XYSctrPlot_", as.integer(i) , ".pdf", sep = "")))$size
      df2 = rbind(df2, c(i,data1))
      
    }
    for (i in seq(100, n, length = l)){
      data2 = file.info(file.path(getwd(),paste("XYSctrPlot_", as.integer(i) , ".jpg", sep = "")))$size
      df3 = rbind(df3, c(i,data2))
      
    }
    for (i in seq(100, n, length = l)){
      data3 = file.info(file.path(getwd(),paste("XYSctrPlot_", as.integer(i) , ".ps", sep = "")))$size
      df4 = rbind(df4, c(i,data3))
    }
    df1 = transform(data.frame(df1), type = "png")
    df2 = transform(data.frame(df2), type = "pdf")
    df3 = transform(data.frame(df3), type = "jpg")
    df4 = transform(data.frame(df4), type = "ps")
    df = rbind(df1, df2, df3, df4)
    names(df) = c("N", "fileSize", "fileType")
    dfPlt1 =  ggplot(df, aes(x=N, y=fileSize, color = fileType)) + geom_point() + geom_smooth(method = "loess", se=F) + facet_grid(fileType~.) + labs(title="Range of N Vs File type (bytes)")
    
    print(dfPlt1)
  }
  
}

fileSave(100000)


#5. Diamonds
pr = ggplot(data = diamonds, aes(x = price))
pr = pr +  geom_histogram(binwidth = 150)
pr = pr + xlab('Price')
pr = pr + ylab('count') + labs(title="Diamond Price Distribution", subtitle = "Diamonds dataset")
print(pr)

lpr = ggplot(data = diamonds, aes(x = log10(price)))
lpr = lpr + geom_histogram()
lpr = lpr + xlab('log(Price)')
lpr = lpr + ylab('count') + labs(title="Diamond log(Price) Distribution", subtitle = "Diamonds dataset")
print(lpr)

summary(diamonds$price, digits = 10)

c = ggplot(data = diamonds, aes(x = carat))
c = c +  geom_histogram(binwidth = 0.05)
c = c + xlab('carat')
c = c + ylab('count') + labs(title="Diamond Carat Distribution", subtitle = "Diamonds dataset")
print(c)

lc = ggplot(data = diamonds, aes(x = log10(carat)))
lc = lc + geom_histogram()
lc = lc + xlab('log(carat)')
lc = lc + ylab('count') + labs(title="Diamond log(Carat) Distribution", subtitle = "Diamonds dataset")
print(lc)

summary(diamonds$carat, digits = 10)

lcol = ggplot(data = diamonds, aes(x = color))
lcol = lcol + geom_bar()
lcol = lcol + xlab('color')
lcol = lcol + ylab('count') + labs(title="Diamond color Distribution", subtitle = "Diamonds dataset")
print(lcol)

summary(diamonds$color)


library(plyr) # i use this to group the data by state and calculate the correlation of the variable so I can display the correllation (cors) numbers in the plot.
d <- ggplot(diamonds, aes(carat, price, fill= color)) + geom_point() + geom_smooth(method = "lm") + facet_wrap(~color, scales = "free_y") + labs(title="Diamonds Carat Vs Price Vs Color")
cors <- ddply(diamonds, c("color"), summarise, cor = round(cor(carat, price), 2))
dplt = d + geom_text(data=cors, aes(label=paste("r=", cor, sep="")), x=4, y=20000)
print(dplt)

corplt = ggplot(diamonds, aes(x=carat, y = price)) + geom_point(alpha = 1/20) + facet_grid(color~., scales = "free_y") + labs(title="Price Vs Carat Vs Color", subtitle = "Diamonds dataset")
print(corplt)

z = ggplot(diamonds, aes(x = color, y = diamonds$price/diamonds$carat, fill = color)) 
z = z +  geom_violin() + scale_y_log10() 
z = z +  geom_boxplot(width = 0.2) + labs(title="Price Vs Carat Vs Color", subtitle = "Diamonds dataset")
print(z)

