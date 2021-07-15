
#https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv
#https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv

###################################
# Get Data
###################################

rm(list=ls())
library(data.table)
library(dplyr)
library(openxlsx)
library(reshape2)
library(sqldf)
library(forecast)
library(keras)
library(randomForest)
library(TSA)
library(ggplot2)
library(fda)
library(igraph)

scale_ivn <- function(x){apply(x,2,rntransform)}
myinv<-function(A){
  A_svd<-fast.svd(A)
  if(length(A_svd$d)==1){
    A_inv<-A_svd$v%*%as.matrix(1/A_svd$d)%*%t(A_svd$u)
  }else{
    A_inv<-A_svd$v%*%diag(1/A_svd$d)%*%t(A_svd$u)
  }
  return(A_inv)
}

write.clip <- function(x){
  clip <- pipe('pbcopy','w')
  write.table(x, file=clip, sep = ',')   
  close(clip)
}
k <- paste(strsplit(paste(Sys.time()),'\\W')[[1]][1:5],collapse='')

#Get data from github
setwd('/Users/wenrurumon/Documents/postdoc/covid19')
raw <- lapply(c('us_raw.csv','global_raw.csv'),fread)
colnames(raw[[2]])[1:2] <- c('state','country')
raw[[2]]$state[raw[[2]]$country=='Taiwan*'] <- 'Taiwan'
raw[[2]]$country[raw[[2]]$country=='Taiwan*'] <- 'China'
raw[[2]] <- raw[[2]][,-3:-4]

write.csv(t(apply(raw[[2]][,-1:-2],2,function(x){tapply(x,raw[[2]]$country,sum)})),'globaldata.csv')

colnames(raw[[1]])[7:8] <- c('state','country')
raw[[1]] <- raw[[1]][,-c(1:6,9:11)]
raw <- do.call(rbind,raw)
raw <- raw %>% filter(!(state==''&country=='US'))

write.csv(t(apply(raw[,-1:-2],2,function(x){
  tapply(x,raw$country,sum)
})),'global.csv')

###################################
# Data Process 1
###################################

map <- data.table(id=1:nrow(raw),raw[,1:2])
X <- t(as.matrix(raw[,-1:-2]))
for(i in 2:(nrow(X))){X[i,] <- ifelse(X[i,]>=X[i-1,],X[i,],X[i-1,])}
# X <- apply(X,2,diff)
colnames(X) <- map$id

X.china <- X[,colnames(X)%in%filter(map,country=='China')$id,drop=F]
colnames(X.china) <- map$state[colnames(X)%in%filter(map,country=='China')$id]
X.us <- X[,colnames(X)%in%filter(map,country=='US')$id,drop=F]
colnames(X.us) <- map$state[colnames(X)%in%filter(map,country=='US')$id]
X.us <- t(apply(X.us,1,function(x){tapply(x,colnames(X.us),sum)}))

###################################
# Data Process 2
###################################

#P
P <- openxlsx::read.xlsx('US-population-est2019.xlsx')
colnames(P) <- c('state','p')
P$state <- substr(P$state,2,nchar(P$state))

#Y
Y <- melt(X.us)
colnames(Y) <- c('date','state','y')
Y$date <- as.Date(sapply(strsplit(paste(Y$date),'/'),function(x){
  paste0('20',x[3],'-',x[1],'-',x[2])
}))
Y$state <- paste(Y$state)
Y <- Y %>% filter(state%in%P$state)
Y$date <- paste(Y$date)

#Y2
Y2 <- melt(apply(X.us,2,diff))
colnames(Y2) <- c('date','state','y2')
Y2$date <- as.Date(sapply(strsplit(paste(Y2$date),'/'),function(x){
  paste0('20',x[3],'-',x[1],'-',x[2])
}))
Y2$state <- paste(Y2$state)
Y2 <- Y2 %>% filter(state%in%P$state)
Y2$date <- paste(Y2$date)

#X
X <- fread('us-covid-19-total-people-vaccinated-2021-07-09.csv')[,-2]
colnames(X) <- c('state','date','x')
X <- X %>% mutate(state=ifelse(state=='New York State','New York',state))
X <- X %>% filter(state%in%P$state)
X$date <- paste(X$date)

#Merge
Y <- sqldf('select y.state, y.date, y.y, x.x from Y y left join X x on x.state=y.state and x.date=y.date')
Y <- sqldf('select y.date, y.state, y.y, y.x, p.p from Y y left join P p on y.state=p.state') 

#Process X
X <- acast(Y %>%select(date,state,x),date~state,value.var='x')
X <- apply(X,2,function(x){
  x.na <- which(!is.na(x))
  x.na <- x[min(x.na):max(x.na)]
  x.map <- cbind(
    1:length(x.na),
    x.na,
    ((1:length(x.na))[!is.na(x.na)])[cumsum(!is.na(x.na))],  
    ((1:length(x.na))[!is.na(x.na)])[(sum(!is.na(x.na))-cumsum(!is.na(x.na[length(x.na):1]))+1)[length(x.na):1]]
  )
  x2 <- (x.na[x.map[,4]]-x.na[x.map[,3]])/(x.map[,4] - x.map[,3])*(x.map[,1]-x.map[,3])+x.na[x.map[,3]]
  x2 <- ifelse(is.na(x.na),x2,x.na)
  x[min(which(!is.na(x))):max(which(!is.na(x)))] <- x2
  x <- x[length(x):1]
  x[is.na(x)] <- x[max(which(!is.na(x)))]-(which(is.na(x))-length(x2))*round(median(diff(x2)))
  x[x<0] <- 0
  x <- x[length(x):1]
  for(i in 2:length(x)){
    if(x[i]<x[i-1]){
      x[i] <- max(x[i],x[i-1])
    }
  }
  x
})
X <- melt(X)
colnames(X) <- c('date','state','x2')
Y <- merge(Y,X,by=c('state','date')) %>% 
  select(state,date,y,x=x2,p) %>% 
  merge(Y2,by=c('date','state')) %>% 
  select(date,state,new=y2,accum=y,doses=x,population=p) %>%
  mutate(date=as.Date(date)) %>% arrange(state,date)
raw <- Y <-Y %>% filter(date<='2021-07-08')

temp <- Y[Y$state=='Missouri',]
temp.ttl <- sum(temp$new)
temp$new[temp$new==69494] <- mean(temp$new[abs(1:nrow(temp) - which(temp$new==69494))==1])
temp$new <- round(temp$new/sum(temp$new)*temp.ttl)
temp$accum <- cumsum(temp$new)
Y[Y$state=='Missouri',] <- temp

temp <- Y[Y$state=='Rhode Island',]
temp.ttl <- sum(temp$new)
temp2 <- rep(NA,length(temp$new))
for(i in 1:length(temp2)){
  temp2[i] <- mean(temp$new[max(1,i-7):min(i+7,length(temp2))])
}
temp$new <- round(temp2/sum(temp2)*temp.ttl)
temp$accum <- cumsum(temp$new)
Y[Y$state=='Rhode Island',] <- temp

raw <- Y

############################################

#inital data
sm <- function(x,mv=7){sapply(mv:length(x),function(i){mean(x[(i-6):i])})}
statei <- "Michigan"
states <- unique(raw$state)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

fets <- function(y,h=7,th=1){
  m <- ets(ts(y))
  f <- forecast(m,h=h)
  list(
    model=m,
    fit=ifelse(f$fit>th,th,f$fit),
    pred=ifelse(f$mean>th,th,f$mean)
  )
}

nls2 <- function(y,x=NULL){
  if(is.null(x)){x <- 1:length(y)}
  model1 <- lm(y~x)
  model2 <- try(
    model2 <- nls(y~a+b*(x)^d,start=list(a=coef(model1)[1],b=coef(model1)[2],d=1),
                  control=list(maxiter = 1000))
  )
  if(class(model2)=='try-error'){
    return(model1)
  } else {
    return(model2)
  }
}
predict.nls2 <- function(model,x=NULL){
  if(is.null(x)){return(predict(model))}
  if(length(coef(model))==2){
    return(coef(model)[1] + coef(model)[2] * x)
  }
  coef(model)[1] + coef(model)[2] * x ^ coef(model)[3]
}

fsp <- function(y,p=20,x=NULL,th=3,tpforce=NULL){
  if(is.null(x)){x <- 1:length(y)}
  y0 <- predict(lm(y~poly(1:length(y),p)))
  # plot.ts(y); lines(y0,col=2)
  tp <- c(1,
          which(sapply(2:(length(y0)-1),function(i){y0[i]==max(y0[-1:1+i])})|
                  sapply(2:(length(y0)-1),function(i){y0[i]==min(y0[-1:1+i])}))+1,
          length(y)) %>% combn(2) %>% t %>% as.data.frame %>% 
    select(from=V1,to=V2) %>% filter(to-from>th)
  if(!is.null(tpforce)){
    # tpforce <- c(1,144,178)
    tp <- filter(tp,from%in%tpforce&to%in%tpforce)
  }
  tp$weight <- apply(tp,1,function(i){
    1-summary(
      lm(
        y[i[1]:i[2]]~
          exp(predict(nls2(log(y[i[1]:i[2]]),
                           x[i[1]:i[2]])))
      )
    )$r.square
  })
  g <- graph_from_data_frame(tp,directed=T)
  tp <- as.numeric(names(shortest_paths(g,V(g)[1],V(g)[length(V(g))])$vpath[[1]]))
  tp <- t(sapply(2:length(tp),function(i){
    c(from=tp[i-1],to=tp[i])
  })) %>% as.data.frame 
  y.model <- lapply(1:nrow(tp),function(i){
    i1 <- tp$from[i]
    i2 <- tp$to[i]
    model <- nls2(log(y[i1:i2]),x[i1:i2])
    fit <- rep(NA,length(y))
    fit[i1:i2] <- predict(model)
    list(from=i1,to=i2,model=model,fit=fit)
  })
  tp$coef <- sapply(y.model,function(x){coef(x$model)[2]})
  y.fit0 <- y.fit <- sapply(y.model,function(x){x$fit})
  if(tp$coef[nrow(tp)]>0){
    i1 <- tp$from[length(tp$from)-1]
    i2 <- tp$to[length(tp$from)]
    y.fit0[,ncol(y.fit)-1] <- c(
      rep(NA,i1-1),
      predict.nls2(y.model[[length(y.model)-1]]$model,x[i1:i2])
    )
    y.fit0 <- y.fit0[,1:(ncol(y.fit)-1),drop=F]
  }
  plot.ts(y)
  lines(exp(rowMeans(y.fit,na.rm=T)),col=2);
  lines(exp(rowMeans(y.fit0,na.rm=T)),col=4);
  list(model=y.model,
       summary=tp,
       fit=exp(rowMeans(y.fit,na.rm=T)),
       fit0=exp(rowMeans(y.fit0,na.rm=T)))
}

bsp <- function(y.p,x.p){
  t1 <- lm(y.p[1:3]~I((x.p[1:3]-x.p[2])^2))
  t1 <- as.numeric(cbind(1,(x.p[1]:x.p[3]-x.p[2])^2) %*% cbind(coef(t1)))
  t2.0 <- cbind(1,x.p[3]:x.p[4]) %*% cbind(coef(lm(y.p[3:4]~x.p[3:4])))
  t2.1 <- (cbind(1,x.p[4]:x.p[5]) %*% cbind(coef(lm(y.p[4:5]~x.p[4:5]))))[-1]
  y2 <- c(t2.0,t2.1)
  x2 <- x.p[3]:x.p[5]
  x2 <- sapply(1:99,function(r){
    x2 <- predict(lm(y2~I((r/100)^x2)))
    (x2-min(x2))/(max(x2)-min(x2)) * (max(y2)-min(y2)) + min(y2)
  })
  t2 <- x2[,which.min(colMeans(abs(y2-x2)))]
  plot(x.p,y.p)
  lines(x.p[1]:x.p[3],t1,col=2)
  lines(x.p[3]:x.p[5],t2,col=2)
  lines(x.p[3]:x.p[5],c(t2.0,t2.1),col=4)
  c(t1,t2[-1])
}

##############################
# Modeling
##############################

# statei <- 'Michigan'
# statei <- "Missouri"
# statei <- 'Texas'
# statei <- 'Oregon'
# statei <- 'Florida'
# statei <- "Arkansas"
# statei <- "Alabama"

temp <- lapply(states,function(statei){
  # statei <- states[(i<-i+1)]
  print(statei)
  options(digits=10) 
  #Initial data
  Y <- raw %>% filter(state==statei&doses>0)
  p <- max(Y$population)
  x <- (sm(diff(c(0,Y$doses)))/p)
  y <- sm(Y$new)/(p-x*p)
  h2 <- 365*2
  #Fitting
  if(statei=='Arkansas'){
    tpforce <- c(1,131,178)
  } else {
    tpforce <- NULL
  }
  y.model <- fsp(y,20,x=cumsum(x),th=3,tpforce=tpforce)
  x0 <- forecast(cumsum(x),h=h2-length(y))$mean
  x0 <- ifelse(x0>0.9,0.9,x0)
  if(y.model$summary$coef[nrow(y.model$summary)]>0){
    model0 <- y.model$model[[length(y.model$model)-1]]$model
    model1 <- y.model$model[[length(y.model$model)]]$model
    fit0 <- y.model$fit0
    fit1 <- y.model$fit
  } else {
    model0 <- model1 <- y.model$model[[length(y.model$model)]]$model
    fit0 <- y.model$fit0
    fit1 <- y.model$fit
  }
  y0 <- exp(predict.nls2(model0,x0))
  y1 <- exp(predict.nls2(model1,x0))
  #Scenario
  rltx <- cbind(current=c(cumsum(x),x0))
  rlt <- cbind(original=c(fit0,y0),current=c(y,y1)) * p * (1-rltx[,ncol(rltx)])
  #12 0.1
  r.week <- 12
  r.vac <- 0.1
  x2 <- (1:length(x0))/r.week/7*r.vac
  x2 <- x0 + ifelse(x2>r.vac,r.vac,x2)
  x2 <- ifelse(x2>0.9,0.9,x2)
  y2 <- y1 - (x2-x0) * 
    ifelse(y1/(1-sum(y)-cumsum(y1)-x2)<0,1,y1/(1-sum(y)-cumsum(y1)-x2))  
  y2 <- ifelse(y2<0,1e-10,y2)
  if(mean(y0==y1)<1){
    y2.tp <- which(sapply(2:length(y2)-1,function(i){y2[i]==max(y2[-1:1+i])}))[1]+1
    x.p <- c(max(which(fit0==fit1)),length(y),length(y)+y2.tp)
    x.p <- c(x.p,max(x.p)+y2.tp,max(x.p)*2-min(x.p))
    y.p <- y[x.p]
    y.p[3] <- y2[y2.tp]
    y.p[4] <- y.p[2]
    y.p[5] <- y0[x.p[5]-length(y)]
    x.p[6] <- x.p[5]*2-x.p[4]
    y.p[6] <- y0[x.p[6]-length(y)]
    y.p <- y.p[-1]; x.p <- x.p[-1]
    y2 <- c(y,y0)
    y2[x.p[1]:x.p[5]] <- bsp(y.p,x.p)
  } else {
    y2 <- c(y,y2)
  }
  rltx <- cbind(rltx,scenario1=c(cumsum(x),x2))
  rlt <- cbind(rlt,scenario1=y2 * p * (1-rltx[,ncol(rltx)]))
  #8 0.1
  r.week <- 8
  r.vac <- 0.1
  x2 <- (1:length(x0))/r.week/7*r.vac
  x2 <- x0 + ifelse(x2>r.vac,r.vac,x2)
  x2 <- ifelse(x2>0.9,0.9,x2)
  y2 <- y1 - (x2-x0) * 
    ifelse(y1/(1-sum(y)-cumsum(y1)-x2)<0,1,y1/(1-sum(y)-cumsum(y1)-x2))  
  y2 <- ifelse(y2<0,1e-10,y2)
  if(mean(y0==y1)<1){
    y2.tp <- which(sapply(2:length(y2)-1,function(i){y2[i]==max(y2[-1:1+i])}))[1]+1
    x.p <- c(max(which(fit0==fit1)),length(y),length(y)+y2.tp)
    x.p <- c(x.p,max(x.p)+y2.tp,max(x.p)*2-min(x.p))
    y.p <- y[x.p]
    y.p[3] <- y2[y2.tp]
    y.p[4] <- y.p[2]
    y.p[5] <- y0[x.p[5]-length(y)]
    x.p[6] <- x.p[5]*2-x.p[4]
    y.p[6] <- y0[x.p[6]-length(y)]
    y.p <- y.p[-1]; x.p <- x.p[-1]
    y2 <- c(y,y0)
    y2[x.p[1]:x.p[5]] <- bsp(y.p,x.p)
  } else {
    y2 <- c(y,y2)
  }
  rltx <- cbind(rltx,scenario2=c(cumsum(x),x2))
  rlt <- cbind(rlt,scenario2=y2 * p * (1-rltx[,ncol(rltx)]))
  #8 0.2
  r.week <- 8
  r.vac <- 0.2
  x2 <- (1:length(x0))/r.week/7*r.vac
  x2 <- x0 + ifelse(x2>r.vac,r.vac,x2)
  x2 <- ifelse(x2>0.9,0.9,x2)
  y2 <- y1 - (x2-x0) * 
    ifelse(y1/(1-sum(y)-cumsum(y1)-x2)<0,1,y1/(1-sum(y)-cumsum(y1)-x2))  
  y2 <- ifelse(y2<0,1e-10,y2)
  if(mean(y0==y1)<1){
    y2.tp <- which(sapply(2:length(y2)-1,function(i){y2[i]==max(y2[-1:1+i])}))[1]+1
    x.p <- c(max(which(fit0==fit1)),length(y),length(y)+y2.tp)
    x.p <- c(x.p,max(x.p)+y2.tp,max(x.p)*2-min(x.p))
    y.p <- y[x.p]
    y.p[3] <- y2[y2.tp]
    y.p[4] <- y.p[2]
    y.p[5] <- y0[x.p[5]-length(y)]
    x.p[6] <- x.p[5]*2-x.p[4]
    y.p[6] <- y0[x.p[6]-length(y)]
    y.p <- y.p[-1]; x.p <- x.p[-1]
    y2 <- c(y,y0)
    y2[x.p[1]:x.p[5]] <- bsp(y.p,x.p)
  } else {
    y2 <- c(y,y2)
  }
  rltx <- cbind(rltx,scenario3=c(cumsum(x),x2))
  rlt <- cbind(rlt,scenario3=y2 * p * (1-rltx[,ncol(rltx)]))
  #Output
  p1 <- ggplot() + 
    geom_line(data=data.frame(date=min(Y$date)+0:(h2-1),rlt) %>% melt(id=1),
              aes(x=date,y=value,colour=variable))
  p2 <- ggplot() + 
    geom_line(data=data.frame(date=min(Y$date)+0:(h2-1),rltx) %>% melt(id=1),
              aes(x=date,y=value,colour=variable))
  multiplot(p1,p2,cols=2)
  print(statei)
  list(model=y.model$summary,
       rlty=data.frame(date=min(Y$date)+0:(h2-1),rlt/p),
       rltx=data.frame(date=min(Y$date)+0:(h2-1),rltx))
})
names(temp) <- states

rlti <- temp$Michigan
p1 <- ggplot() + geom_line(
  data=melt(rlti$rlty,id=1) %>% mutate(variable=toupper(variable)) %>%
    filter(date<='2021-12-01'),
  aes(x=date,y=value,colour=variable)
) + labs(x='Date',y='New Cases',colour='',title='Trend of New Cases - Michigan') + theme(legend.position='top',legend.text=element_text(size=5))
p2 <- ggplot() + geom_line(
  data=melt(rlti$rltx,id=1) %>% mutate(variable=toupper(variable)) %>%
    filter(date<='2021-12-01'),
  aes(x=date,y=value,colour=variable)
) + labs(x='Date',y='Accumulated Vaccinated People',colour='',title='Trend of Vaccinated People') + theme(legend.position='top',legend.text=element_text(size=5))

rlti <- temp$Texas
p3 <- ggplot() + geom_line(
  data=melt(rlti$rlty,id=1) %>% mutate(variable=toupper(variable)) %>%
    filter(date<='2021-12-01'),
  aes(x=date,y=value,colour=variable)
) + labs(x='Date',y='New Cases',colour='',title='Trend of New Cases - Texas') + theme(legend.position='top',legend.text=element_text(size=5))
p4 <- ggplot() + geom_line(
  data=melt(rlti$rltx,id=1) %>% mutate(variable=toupper(variable)) %>%
    filter(date<='2021-12-01'),
  aes(x=date,y=value,colour=variable)
) + labs(x='Date',y='Accumulated Vaccinated People',colour='',title='Trend of Vaccinated People') + theme(legend.position='top',legend.text=element_text(size=5))

rlti <- temp$Missouri
p5 <- ggplot() + geom_line(
  data=melt(rlti$rlty,id=1) %>% mutate(variable=toupper(variable)) %>%
    filter(date<='2021-12-01'),
  aes(x=date,y=value,colour=variable)
) + labs(x='Date',y='New Cases',colour='',title='Trend of New Cases - Missouri') + theme(legend.position='top',legend.text=element_text(size=5))
p6 <- ggplot() + geom_line(
  data=melt(rlti$rltx,id=1) %>% mutate(variable=toupper(variable)) %>%
    filter(date<='2021-12-01'),
  aes(x=date,y=value,colour=variable)
) + labs(x='Date',y='Accumulated Vaccinated People',colour='',title='Trend of Vaccinated People') + theme(legend.position='top',legend.text=element_text(size=5))

multiplot(p5,p6,p1,p2,p3,p4,cols=3)
