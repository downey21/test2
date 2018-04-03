### R-Object
# 1
x <- c(17, 16, 20, 24, 22, 15, 21, 18)
x[x >= 20]
y <- x[]
y[y >= 20] <- 100
y

# 2
x <- matrix(rep(-1, 25), 5)
for (i in 1:5)
{
  x[i, i] <- i+2
}
y <- x[,-5]
yinfo <- dim(y)
y1 <- y[,]
for (i in 1:5)
{
  for (j in 1:4)
  {
    if (y1[i,j] == -1) y1[i,j] <- 0
  }
}
x
yinfo
y
y1

# 3
rdata <- read.csv("C:\\tmp\\rowdata.txt", header = T)
is.na(rdata)
a <- c()
for (i in nrow(is.na(rdata)))
{
  if (is.na(rdata)[i, 2] == FALSE & is.na(rdata)[i, 3] == FALSE) a[i] <- i 
}
a[!is.na(a)]
rdata1 <- rdata[a,]

# 4
temp <- list(c(TRUE, FALSE), matrix(c(1, 0, 0, 1), 2, 2), seq(0, 1, length = 100), 1, 2, 3, 4)
temp[[2]] <- NULL
temp[[3]]
length(temp)

# 5
a1 <- -1:2
a2 <- 1:2
a1 + a2
a1 <- -(1:2)
a2 <- 1:2
a1 + a2
a1 <- matrix(0, 2, 2)
a2 <- c(3, 4)
a1 + a2
a1 <- matrix(1:4, 2, 2)
a1[a1 > 2]
a1 <- 1:5
a1[-1] - a1[-length(a1)]

### R-Programming
# 1 - 2
a = c()
for (i in 1:20)
{
  if (i < 3) 
  {
    a[1] = 1
    a[2] = 3
  } else {
    a[i] = 0.9 * a[i-1] - 0.1 * a[i-2] + 1
  }
}
a[20]
for (k in 1:length(a))
{
  if (a[k] > 4)
  {
    print(k)
    stop()
  }
}

# 3
A <- matrix(runif(100), 50, 5)
v <- c()
for (i in 1:nrow(A)) 
{
  v[i] = sum(A[i, ])
}

# 4
tmp = rep(0, 10)
a <- 10:1
idx = 1
for ( j in a)
{
  if (j<5)
  {
    tmp[idx] <- a[j]
    idx <- idx + 1
  }
}
tmp

# 5 - 6
set.seed(1)
x <- matrix(runif(5000), 1000, 5)
sid <- sample(1:10, 1000, replace = T)
m.mat <- matrix(NA, 10, 5)
for (i in 1:10)
{
  m.mat[i,] <- apply(x[which(sid == i),], 2, mean)
}
idist <- matrix(NA, 1000, 10)
for (i in 1:1000) 
{
  for (j in 1:10)
  {
    d1 <- sum(x[i,]*m.mat[j,])
    d2 <- sqrt(sum((x[i,])^2))*sqrt(sum((m.mat[j,])^2))
    idist[i,j] <- d1/d2
  }
}

# 7
ivec <- c()
for (i in 1:1000)
{ 
  ivec[i] <- which.min(idist[i,])
}

# 8
set.seed(1)
a = list()
for (i in 1:1000)
{
  x = rpois(1,4)+1
  x = min(x,10)
  a[[i]] = sample(1:10, x)
}
frq <- c()
for (i in 1:1000)
{
  frq[i] <- length(a[[i]])
}
table(frq)
score <- rep(0, 10)
for (i in 1:1000)
{
  if (frq[i] >= 7)
  {
    score[a[[i]][1]] <- score[a[[i]][1]]+3
    score[a[[i]][2]] <- score[a[[i]][2]]+2
    score[a[[i]][3]] <- score[a[[i]][3]]+1
  } else if (frq[i] >= 4)
  {
    score[a[[i]][1]] <- score[a[[i]][1]]+2
    score[a[[i]][2]] <- score[a[[i]][2]]+1
  } else
  {
    score[a[[i]][1]] <- score[a[[i]][1]]+1
  }
}
score

# 9 (1)
set.seed(1)
m1 = 10
m2 = 5
num = 0
while (m1*m2 != 0)
{
  if (rbinom(1, 1, 1/2) == 0) 
  {
    m1 <- m1-1
    m2 <- m2+1
  } else
  {
    m1 <- m1+1
    m2 <- m2-1
  }
  num <- num+1
  if (num == 4) break
}
m1;m2;num

# 9 (2)
set.seed(1)
m1 = 10
m2 = 5
num = 0
while (m1*m2 != 0)
{
  if (rbinom(1, 1, 1/2) == 0) 
  {
    m1 <- m1-1
    m2 <- m2+1
  } else
  {
    m1 <- m1+1
    m2 <- m2-1
  }
  num <- num+1
}
m1;m2;num

# 9 (3)
w <- rep(0, 200)
for (k in 1:200)
{
  set.seed(k)
  m1 = 10
  m2 = 5
  num = 0
  while (m1*m2 != 0)
  {
    if (rbinom(1, 1, 1/2) == 0) 
    {
      m1 <- m1-1
      m2 <- m2+1
    } else
    {
      m1 <- m1+1
      m2 <- m2-1
    }
    num <- num+1
  }
  if (m1 != 0) w[k] <- 1
}
sum(w)

# 10
result <- c()
for (i in c(10, 15, 20, 25))
{
  w <- rep(0, 200)
  for (k in 1:200)
  {
    set.seed(k)
    m1 = 10
    m2 = i
    num = 0
    while (m1*m2 != 0)
    {
      if (rbinom(1, 1, 1/2) == 0) 
      {
        m1 <- m1-1
        m2 <- m2+1
      } else
      {
        m1 <- m1+1
        m2 <- m2-1
      }
      num <- num+1
    }
    if (m1 != 0) w[k] <- 1
  }
  result[i] <- sum(w)
}
result[!is.na(result)]
result[!is.na(result)]/200