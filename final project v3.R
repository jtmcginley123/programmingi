## QUESTION 1 ##
# GOAL: Exploratory Data Analysis 
# check for missing values, replace if needed 
# check data types of all the columns in the data frame 
# determine insights

library(tidyverse)
Listings = read.csv("Listings.csv")
Reviews = read.csv("Reviews.csv")
names(Listings)
names(Reviews)

glimpse(Listings)
glimpse(Reviews)
## use a left join to make sure that the data frame has all the rows from Listing and the aditional info from Reviews
df = left_join( Listings , Reviews , by = c("id" = "listing_id"))
## check to see if there are any NaNs in the dataset
apply( df, 2, FUN = anyNA)
## find the index for where the NaNs are located and store in missing beds vector
missing_beds = which(is.na(df$beds))
## using missing beds vector to replace the NaNs with the mean
## beds should not double -> use ceiling to round 1.92 up to 2
df$beds[missing_beds] = ceiling(mean(df$beds, na.rm = TRUE))
## do the same thing with avg rating
missing_rating = which(is.na(df$avg_rating))
# df$avg_rating[missing_rating] = which(is.na(df$avg_rating))
df$avg_rating[missing_rating] = mean(df$avg_rating, na.rm = TRUE)

## since there are blank values in room type decided to replace these blanks with "unknown"
df$room_type[df$room_type == ""] <- "Unknown"

## now that the data frame has been cleaned of NaNs and Blanks 
df = df %>%
  select(-id) %>%
  mutate(neighborhood = as.factor(neighborhood) , host_since = as.Date(host_since, format = "%m/%d/%Y")
         , room_type = as.factor(room_type) , bathrooms = as.factor(bathrooms),
         bedrooms_f = as.factor(bedrooms))
glimpse(df)
## let us explore each column

# 1) Neighborhood: Character data type -> convert to factor to make more robust for categorical analysis 

neigh_freq=table(df$neighborhood)
barplot(neigh_freq)

# 2) host_since is a character data type -> convert to a date

ggplot(df, aes(x=host_since, y = price)) + geom_line()


# 3) host_acceptance_rate is a double 
## data type makes sense, no need to change . explore values 

summary(df$host_acceptance_rate) 
## summary stats show possible left skewness
ggplot(df, aes(host_acceptance_rate)) + geom_histogram()
ggplot(df, aes(host_acceptance_rate)) + geom_boxplot() + coord_flip()

df2 = df %>%
  mutate( host_acceptance_rate2 = ((1-(host_acceptance_rate)^2) /2)^(1/3))
## since data is left-skewed, can manipulate possibily to make right skewed then apply log/sqrt/cube transform 
ggplot(df2, aes(host_acceptance_rate2)) + geom_histogram( binwidth = 0.1) 
ggplot(df2, aes(host_acceptance_rate2)) + geom_boxplot() + coord_flip()
## test at transform (still not much better)

# 4) Superhost is a logical data type -> this works but for analysis might be better to have 0 and 1
# use if else for a vectorized approach of replacing all values
superhost2=ifelse(df$superhost == FALSE, 0 , 1)
## append the new vector to the exisiting data frame df 
df = data.frame(df, superhost2)

superhost_freq = table(df$superhost)
superhost2_freq = table(df$superhost2)
prop.table(superhost_freq)
pie(superhost_freq)
pie(superhost2_freq)

# 5) host_total_listing is an integer data type 

## first find summary statistics 
summary(df$host_total_listings)
ggplot(df, aes(host_total_listings)) + geom_histogram()

## data for host total listings is right skewed 
## try sqrt transform 

df3 = df %>%
  mutate (host_total_listings2 = sqrt(host_total_listings),
          host_total_listings3 = log(host_total_listings))

ggplot(df3, aes(host_total_listings2)) + geom_histogram(binwidth = 10)
ggplot(df3, aes(host_total_listings3)) + geom_histogram(binwidth =1)

## log transform was a little better 

# 6) room type (already took care of the blanks) 
## data type is character -> lets convert this to a factor data type for better categorical analysis 

barplot(table(df$room_type))
# some sampling error -> way to many entire home/apt compraed to the others 

# 7) accommodates -> is a integer data type 
# first lets see the summary statistics 

summary(df$accommodates)
## check the distribution 
df %>% 
  ggplot(aes(accommodates)) + geom_boxplot() + coord_flip()
df %>% 
  ggplot(aes(accommodates)) + geom_histogram()

# bimodal distribution, slightly skewed to the right 

# 8) bathrooms is character data type -> convert to factor

bathroom_freq = table(df$bathrooms)
prop.table(bathroom_freq)
barplot(bathroom_freq)

## lots of 1 bath -> highly imbalanced sampling 

# 9) bedrooms -> int data type (makes sense, but could possibly use this as a categorical variable as well)

summary(df$bedrooms)
ggplot(df, aes(bedrooms)) + geom_histogram()
ggplot(df, aes(bedrooms)) + geom_boxplot() +coord_flip()

bedrooms_freq = table(df$bedrooms_f)
prop.table(bedrooms_freq)
plot(bedrooms_freq)

# 10) beds -> double data type

summary(df$beds)
ggplot(df, aes(beds)) + geom_histogram(binwidth = 1)
ggplot(df, aes(beds)) + geom_boxplot() + coord_flip()

# 11) price is a double -> find summary statistics
summary(df$price)
ggplot(df, aes(price)) + geom_histogram()
# there was a few data points that are clear outliers < 50000
# simple test to first visualize the data in a box plot with these outliers removed
boxplot(df$price , outline = F) 

# 12) min_nights is an integer data type
summary(df$min_nights)
ggplot(df, aes(min_nights)) + geom_histogram()
# same as price -> first lets visualize without the clear outliers
boxplot(df$min_nights, outline = F)

# 13) total reviews is an int data type 
summary(df$total_reviews)
ggplot(df, aes(total_reviews)) + geom_histogram()

df4 = df %>%
  mutate(total_reviews1 = log(total_reviews),
         total_reviews2 = sqrt(total_reviews))


ggplot(df4, aes(total_reviews1)) + geom_histogram()
ggplot(df4, aes(total_reviews2)) + geom_histogram()
## log transform was better than sqrt transform

## right skewed -> possible transform 


# 14) avg_rating is a double data type
summary(df$avg_rating) 

# left skewed data, with boundary conditions of 1 to 5 
df5 = df %>%
  mutate( avg_rating1 = 5- avg_rating,
          avg_rating2 = sqrt(5- avg_rating),
          avg_rating3 = log(5- avg_rating),
          avg_rating4 = log((5- avg_rating)/ avg_rating))


ggplot(df, aes(avg_rating)) + geom_histogram()
ggplot(df5, aes(avg_rating1)) + geom_histogram()
ggplot(df5, aes(avg_rating2)) + geom_histogram()
## this worked well however 224 non-finite outside the scale range
ggplot(df5, aes(avg_rating3)) + geom_histogram(binwidth = 0.5)
ggplot(df5, aes(avg_rating4)) + geom_histogram()






## lets explore possible correlations between the numerical variables
df %>%
  select_if(is.numeric) %>%
  cor() 

correlation_matrix = cor(select_if(df, is.numeric))
diag(correlation_matrix) = NA
correlation_df = data.frame(correlation_matrix)
names(correlation_df[which.max(abs(correlation_df$price))])
## before any filtering of data , host total listings is the most correlated with price


interquartilerange= IQR(df$price)

upper = quantile(df$price, 0.75)
lower = quantile(df$price , 0.25)

upper_outlier = upper + 1.5 * interquartilerange
lower_outlier = lower - 1.5 * interquartilerange
df= df %>%
  filter ( price > lower_outlier & price < upper_outlier)

# after the filtering of exremes the price histogram looks a lot better
ggplot(df, aes(price)) + geom_histogram( binwidth = 30)


# now that the price outliers are filtered out 
# let us retest the correlation matrix

correlation_matrix_clean = cor(select_if(df, is.numeric))
diag(correlation_matrix_clean) = NA 
correlation_df_clean = data.frame(correlation_matrix_clean)

correlation_df_clean %>%
  arrange(desc(price)) %>%
    select(price)
    

## best three to study are accommodates, bedrooms , beds

## lets start with visualizing price v accommodates, bedroom, bed
## price = dependent variable , the rest are independent variable 


ggplot( df , aes(x = accommodates, y = price)) + geom_point() + geom_jitter() + geom_smooth(method = "lm" , se = FALSE)
ggplot( df , aes(x = bedrooms, y = price)) + geom_point() + geom_jitter() + geom_smooth(method = "lm" , se = FALSE)
ggplot( df , aes(x = beds, y = price)) + geom_point() + geom_jitter() + geom_smooth(method = "lm" , se = FALSE)

ggplot( df , aes(x = accommodates, y = price , color = neighborhood)) + geom_point() + geom_jitter() + geom_smooth(method = "lm" , se = FALSE)
ggplot( df , aes(x = bedrooms, y = price , color = neighborhood)) + geom_point() + geom_jitter() + geom_smooth(method = "lm" , se = FALSE)
ggplot( df , aes(x = beds, y = price , color = neighborhood)) + geom_point() + geom_jitter() + geom_smooth(method = "lm" , se = FALSE)


ggplot(df , aes(x = accommodates, y = price)) + 
  geom_point() + geom_jitter() + geom_smooth(method = "lm" , se = FALSE) + 
  facet_wrap(~neighborhood, scales = "free_x")

ggplot(df , aes(x = beds, y = price)) + 
  geom_point() + geom_jitter() + geom_smooth(method = "lm" , se = FALSE) + 
  facet_wrap(~neighborhood, scales = "free_x")

ggplot(df , aes(x = bedrooms, y = price)) + 
  geom_point() + geom_jitter() + geom_smooth(method = "lm" , se = FALSE) + 
  facet_wrap(~neighborhood, scales = "free_x")

## Question 2 ##


## lets also get some numbers: 
table(df$room_type)
table(df$neighborhood)
table(df$neighborhood, df$room_type)
df %>% 
  group_by(neighborhood) %>%
    summarize(avg_price = mean(price, na.rm = TRUE), Q_price25=quantile(price, 0.25), Q_price50=quantile(price, 0.5), Q_price75=quantile(price, 0.75)) %>%
      arrange(desc(avg_price))
df %>%
  group_by(neighborhood, room_type) %>%
  summarize(avg_price = mean(price, na.rm = TRUE), Q_price25=quantile(price, 0.25), Q_price50=quantile(price, 0.5), Q_price75=quantile(price, 0.75)) %>%
  arrange(desc(avg_price))





## Question 3 

## kind of interesting since we can simply use the t.test function. 

# Jackie:

confInt = function(level , var, data) {
  
  x = data[[var]]
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
  
}

confInt95 = confInt(.95, "price", df)

round(confInt95, 2)





# Larry:

conf_test = function(level , name , dataframe) { 
  # the function will take three arguments
  # level: numerical confidence level 
  # name of the numerical variable (string)
  # name of the dataframe

  # need to only grab all the columns and work with that data
  # since double [[]] for dataframes will pull only the data from that column
  var = dataframe[[name]]
  avg = mean(var , na.rm = TRUE)
  std = sd(var, na.rm = TRUE)
  alpha = 1 -level
  d_f = length(var) - 1
  # one tailed
  t_val = abs(qt(  alpha, d_f ))
  
  conf_int = c( avg - (t_val * (std / sqrt(length(var))))
                ,
                avg + (t_val * (std / sqrt(length(var)))))
  # two tailed
  t_val2 = abs(qt(  alpha/2 , d_f ))
  conf_int2 = c( avg - (t_val2 * (std / sqrt(length(var))))
                       ,
                avg + (t_val2 * (std / sqrt(length(var)))))
  
  return(list(one_tailed = conf_int , two_tailed = conf_int2))

}



## EXTRA: 

pb_reg = lm(price~beds , df)
summary(pb_reg)

df[182,]
df[183,]
df[109,]

## lets test removing these three rows from the df


