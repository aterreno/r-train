dt <- read.csv("listings.csv")
length(dt)
dim(dt)
colnames(dt)
dt[1:16,]
dt$PropertyType=NULL
dt$Style=NULL
dt$CloseDate=NULL
cor(dt)
plot(cor(dt))
require(lattice)
levelplot(cor(dt))
install.packages("corrgram")
require(corrgram)
corrgram(cor(dt))
model1 <- lm(ClosePrice ~ SquareFeet, dt)
summary(model1)
model2 <- lm(ClosePrice ~ SquareFeet+TotalBathrooms, dt)
summary(model2)
model3 <- lm(ClosePrice ~ SquareFeet+TotalBathrooms+Age, dt)
summary(model3)
model4 <- lm(ClosePrice ~ SquareFeet+TotalBathrooms+Age+TotalBedrooms, dt)
summary(model4)
model5 <- lm(ClosePrice ~ SquareFeet+TotalBathrooms+Age+TotalBedrooms+Year, dt)
summary(model5)
dt$predict = predict(model5, newdata=dt)
dt[1:10,]
dt$error = (abs(dt$ClosePrice-dt$predict)/dt$ClosePrice)
dt$predict
dt$error[1:5]
dt[1:10, ]
plot(ecdf(dt$error[dt$error <5]))
grid(col='black')
plot(ecdf(dt$error[dt$error <5]), ylab='% time',  xlab='error')
(hist(dt$error))
(hist(dt$error[dt$error <1 ]))
plot(ecdf(dt$error[dt$error <1]))
table1<-table(dt$Polygon_id)
plot(as.data.frame(table1))
table1[as.data.frame(table1)$Freq>300]

neural <- nnet(ClosePrice ~ SquareFeet+TotalBathrooms+Age+TotalBedrooms+Year, dt, size=20)


