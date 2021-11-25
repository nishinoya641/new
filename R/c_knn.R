d <- function(x,y){
  a <- sqrt((x-y)%*%t(x-y))
  return (a)
}
c_knn <- function(data){
  n <- nrow(data)
  m <- ncol(data)
  knnlist <- rep(0,length=n)
  for (k in 1:n){
    traindata <- data[-k,]
    #要预测的点
    testdata <- data[k,]
    train.len <- n-1
    test.label <- 0
    traindata$dis <- rep(0,train.len)
    #选择邻居数
    K=5
    for (j in 1:train.len){
      traindata$dis[j] <- d(as.matrix(traindata[j,1:(m-1)]),as.matrix(testdata[1,1:(m-1)]))
    }
    nn <- traindata[sort(traindata$dis,decreasing = FALSE,index.return = TRUE)$ix,][1:K,m]
    test.label<-ifelse(mean(nn)>0.5,1,0)
    knnlist[k] <- ifelse(test.label==testdata[m],0,1)
  }
  return(sum(knnlist))
}
