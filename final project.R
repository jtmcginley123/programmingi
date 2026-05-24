library(tidyverse)


Listings = read.csv('Listings.csv')
Reviews = read.csv('Reviews.csv')

names(Listings)
names(Reviews)

glimpse(Listings)
glimpse(Reviews)
df = left_join( Listings , Reviews , by = c("id" = "listing_id"))

glimpse(df)

apply( df, 2, FUN = anyNA)
# replace na with mean  ( need to work on this later)
sum(is.na(df$beds))
sum(is.na(df$avg_rating))
sum(is.na(df$room_type))

missing_beds = which(is.na(df$beds))

df$beds[missing_beds] = mean(df$beds, na.rm = TRUE)

missing_rating = which(is.na(df$avg_rating))

df$avg_rating[missing_rating] = mean(df$avg_rating, na.rm = TRUE)



# maybe check all the rows -> use loop 
for(i in 1:ncol(df) ) {
  df[i,] = 
}

# replace nulls with NA / unknown

# Q2

df$room_type[df$room_type == ""] <- NA
df$room_type[df$room_type == ""] <- "Unknown"

df %>%
  group_by(neighborhood , room_type) %>%
    filter(!(room_type == "Unknown")) %>%
      summarize(avg_price = mean(price , na.rm = TRUE)) %>%
        arrange(desc(avg_price))

ggplot(df , aes(room_type, price)) + geom_boxplot()

quantile()
upper = Q3 + 1.5(IQR) 
lower = Q1 - 1.5(IQR)
    
# df[df==""]

sum(is.na(df$room_type))


#Q3

confInt = function(level , price, df) {
  # three arguments level : numerical 
  x = df[["price"]]
  n = length(x)
  
  alpha = 1-level
  avg = mean(x)
  stdev = sd(x)
  sterr = stdev / sqrt(n)
  
  cvT = qt(1-alpha/2, df = n-1)
  
  c(lower = avg-cvT * sterr,
    mean = avg,
    upper = avg + cvT * sterr,
    n = n)
  # name = string
  # dataframe
  
#  x-bar +/- confidence level * sample standard deviation / sqrt(sample size)
}

confInt95 = confInt(.95, "price", df)

round(confInt95, 2)
# Q4 

# t.test
t.test(df$price, mu = 200, alternative = "greater", conf.level = 0.95)

# Q5

ggplot(df , aes(price)) + geom_histogram(binwidth = 25 ) + xlim(0,1000)

# Q6

lm(Price~)




#Q8 

df %>% 
  # filter( !(is.na(beds) ) %>%
  select(-id) %>%
  select_if(is.numeric)%>%
    cor()
  

