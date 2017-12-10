###mcgowan
install.packages("twitteR", dependencies=T)
install.packages(c('ROAuth','RCurl'))
library(twitteR)
library(streamR)
library(ROAuth)
library(RCurl)
library(base64enc)
library(httr)
library(lubridate)
library(chron)



check_for_num = function(x){
  #ans=which(is.na(as.numeric(gsub("[[:space:]]", "", gsub("[[:punct:]]", " ", x))))==FALSE)
  #ans=which(sum(nchar(gsub("[[:space:]]", "", gsub("[[:punct:]]", " ", x))))==10)
  for(i in 1:length(x)){
    vecs = gsub("[[:space:]]", "", gsub("[[:punct:]]", " ", x))
  }
  
  trigger = which(is.na(as.numeric(vecs))==FALSE)
  
  if(length(trigger)==0){
    return(FALSE)
  }else{
    nums=vecs[trigger]
    
    if(length(nums)>1){
      nums = paste(nums, collapse="")
    }
    
    ans=nchar(nums)==10
    if(ans==TRUE){
      
    }else{
      ans=FALSE
    }
  }
  
  return(ans)
  
}

full_check = function(x){
  tweets.split = strsplit(x, " ")
  ans1= lapply(tweets.split, check_for_num)
  return(which(unlist(ans1)==TRUE))
}

api_key =   ""
api_secret = 	""
a.token = "-"
a.secret = ""



setup_twitter_oauth(api_key, api_secret, a.token, a.secret)



filterStream(file.name = "tweets1.json", track = c("number", "phone", "dox"),language = "en", timeout = 30000, 
             oauth = twitCred) 

tweets.df = parseTweets("tweets1.json", simplify = FALSE)
tweets.df1 = parseTweets("tweets.json", simplify = FALSE)
num.tweet = full_check(tweets.df$text)
num.tweet1 = full_check(tweets.df1$text)

t=lapply(strsplit(tweets.df$created_at, " "),function(x){return(x[4])})
max.t = max(times(t))
min.t = min(times(t))

t1=lapply(strsplit(tweets.df1$created_at, " "),function(x){return(x[4])})
max.t1 = max(times(t1))
min.t1 = min(times(t1))

num.dox.tweets=unique(c(tweets.df[num.tweet,1], tweets.df1[num.tweet1,1]))


###not 10





