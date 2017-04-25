# 求从1：n中包含数字“1”的个数
## 方法一：利用R自带的函数------------------------
# 函数as.character,sub,str_extracr,length.
library('dplyr')
library('stringr')
NumberContainOne <- function(n){
  temp <- sub("1", "a", as.character(1:n)) %>% str_extract("a")
  return(temp[ ! is.na(temp)] %>% length())
}

## 方法二：----------------------------------
NumberContainOne_1 <- function(x){
  le <- nchar(x)
  x <- 1:x
  if(le == 1){
    return(1)
  }
  else{
    for(i in le:2){
      x[x >= 10^(i - 1) & x < 10^i & floor(x / 10^(i -1)) == 1] <- 1
      x[x >= 10^(i - 1) & x < 10^i & floor(x / 10^(i -1)) != 1] <- x[x >= 10^(i - 1) & x < 10^i & floor(x / 10^(i -1)) != 1] %% 10^(i - 1) 
    }
    x[x != 1] <- 0
    return(sum(x))
  }
}
## 方法三：-------------------------------------------
NumberContainOne_2 <- function(x){
  le <- nchar(x)
  if(le == 1){
    return(1)
  }else{
    x <- data.table(y = 1:x)
    number <- numeric(le)
    for(i in le:2){
      number[i] <- x[y >= 10^(i - 1) & floor(y / 10^(i -1)) == 1, .N] 
      x <- x[y < 10^(i - 1) | y > 10^i | floor(y / 10^(i -1)) != 1]
      x[y >= 10^(i - 1) & floor(y / 10^(i -1)) != 1, y := y %% 10^(i - 1)]
    }
    number[1] <- x[y == 1, .N]
    return(sum(number))
  }
}
## 方法二数据量大的时候计算速度比较快，而且第二种方法占的内存比较小。
## 方法三思路跟方法二一样，不过它的实现方式会比方法二快。
