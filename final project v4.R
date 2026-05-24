## QUESTION 1 ##
# GOAL: Exploratory Data Analysis 
# check for missing values, replace if needed 
# check data types of all the columns in the data frame 
# determine insights

library(tidyverse)
Listings = read.csv("Listings.csv")
Reviews = read.csv("Reviews.csv")
# names(Listings)
# names(Reviews)
# glimpse(Listings)
# glimpse(Reviews)


## use a left join to make sure that the data frame has all the rows from Listing and the additional info from Reviews
df = left_join( Listings , Reviews , by = c("id" = "listing_id"))
## check to see if there are any NaNs in the data set

# apply( df, 2, FUN = anyNA)

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
## it was found that Union Station, Capital Hill, Columia heights, dupont, and edgewood were the most popular
barplot(neigh_freq)
# 2) host_since is a character data type -> convert to a date
## decided to use the IQR method of removing outliers since the data was skewed with an extreme outlier

interquartilerange= IQR(df$price)
upper = quantile(df$price, 0.75)
lower = quantile(df$price , 0.25)
upper_outlier = upper + 1.5 * interquartilerange
lower_outlier = lower - 1.5 * interquartilerange
df_price= df %>%
  filter ( price > lower_outlier & price < upper_outlier)

ggplot(df, aes(x=host_since, y = price))  + geom_line() +labs(y = "Price" , x = "Host_since",
title = "Line Plot of Raw Price V Host_since")
ggplot(df_price, aes(x=host_since, y = price))  + geom_line() +labs(y = "Price" , x = "Host_since",
title = "Line Plot of Filtered Price V Host_since")


# 3) host_acceptance_rate is a double 
## data type makes sense, no need to change . explore values 

summary(df$host_acceptance_rate) 
## summary stats show possible left skewness
ggplot(df, aes(host_acceptance_rate)) + geom_histogram(binwidth = 0.05)
ggplot(df, aes(host_acceptance_rate)) + geom_boxplot() + coord_flip()
## box plot and histogram confirm left skewness
df_host_test = df %>%
  mutate( host_acceptance_rate2 = ((1-(host_acceptance_rate))^(1/2)))
## since data is left-skewed, can manipulate 
ggplot(df_host_test, aes(host_acceptance_rate2)) + geom_histogram( binwidth = 0.05) 
ggplot(df_host_test, aes(host_acceptance_rate2)) + geom_boxplot() + coord_flip()
## Transform was not great, Outlier analysis performed 
## data is very skewed so IQR outlier analysis is the best approach
IQR_host_rate = IQR(df_price$host_acceptance_rate)
upper_host_price = quantile(df_price$host_acceptance_rate , 0.75)
lower_host_price = quantile(df_price$host_acceptance_rate , 0.25)
upper_outlier_host_price = upper_host_price + 1.5 * IQR_host_rate
lower_outlier_host_price  = lower_host_price - 1.5 * IQR_host_rate

df_host_price = df_price %>%
  filter(host_acceptance_rate > lower_outlier_host_price & host_acceptance_rate < upper_outlier_host_price)

ggplot(df_host_price , aes(host_acceptance_rate)) + geom_histogram(binwidth = 0.01)


# 4) Superhost is a logical data type -> this works but for analysis might be better to have 0 and 1
# use if else for a vectorized approach of replacing all values
superhost2=ifelse(df_price$superhost == FALSE, 0 , 1)
## append the new vector to the exisiting data frame df 
df_price = data.frame(df_price, superhost2)

superhost_freq = table(df_price$superhost)
superhost2_freq = table(df_price$superhost2)
prop.table(superhost_freq)
pie(superhost_freq)
# pie(superhost2_freq)
## approximately 2/3 of users are superhosts 
## makes sense given that the host acceptance rate is so high

# 5) host_total_listing is an integer data type 
## first find summary statistics 
summary(df$host_total_listings)
ggplot(df, aes(host_total_listings)) + geom_histogram(binwidth = 100)

iqr_total_listings = IQR(df_price$host_total_listings)
upper_total_listing = quantile(df_price$host_total_listings, 0.75)
lower_total_listing = quantile(df_price$host_total_listings, 0.25)
upper_outlier_total_listings = upper_total_listing + 1.5 * iqr_total_listings
lower_outlier_total_listings = lower_total_listing - 1.5 * iqr_total_listings

df_listings_price = df_price %>%
  filter ( host_total_listings > lower_outlier_total_listings & host_total_listings < upper_outlier_total_listings)

ggplot(df_listings_price, aes(host_total_listings)) + geom_histogram(binwidth = 5)

## data for host total listings is right skewed 
## try sqrt transform 

df3 = df_listings_price %>%
  mutate (host_total_listings2 = sqrt(host_total_listings),
          host_total_listings3 = log(host_total_listings))

ggplot(df3, aes(host_total_listings2)) + geom_histogram(binwidth = 1)
ggplot(df3, aes(host_total_listings3)) + geom_histogram(binwidth =0.5)

## log transform was a little better 

# 6) room type (already took care of the blanks) 
## data type is character -> lets convert this to a factor data type for better categorical analysis 

barplot(table(df$room_type))
# some sampling error -> way to many entire home/apt compared to the others 

# 7) accommodates -> is a integer data type 
# first lets see the summary statistics 

summary(df$accommodates)
## check the distribution 
df_price %>% 
  ggplot(aes(accommodates)) + geom_boxplot() + coord_flip()
df_price %>% 
  ggplot(aes(accommodates)) + geom_histogram(binwidth = 1)

# bimodal distribution, slightly skewed to the right 
IQR_ap = IQR(df_price$accommodates)
upper_a_p = quantile(df_price$accommodates, 0.75)
lower_a_p = quantile(df_price$accommodates, 0.25)
upper_outlier_ap = upper_a_p + 1.5*IQR_ap
lower_outlier_ap = lower_a_p - 1.5*IQR_ap

df_a_p = df_price %>%
  filter(accommodates > lower_outlier_ap & accommodates < upper_outlier_ap)
df_a_p %>% 
  ggplot(aes(accommodates)) + geom_boxplot() + coord_flip()
df_a_p %>% 
  ggplot(aes(accommodates)) + geom_histogram(binwidth = 1)



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

# after the filtering of extremes the price histogram looks a lot better
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

ggplot(df , aes(x = bedrooms, y = price)) + 
  geom_point() + geom_jitter() + geom_smooth(method = "lm" , se = FALSE) + 
  facet_wrap(~room_type, scales = "free_x")

## Summary statistics, Group By Room_type , Bathrooms, Neighborhood 

df %>% 
  group_by(neighborhood) %>% 
  summarize(avg_price = mean(price) , sd_price = sd(price) , median_price = quantile(price, 0.5) , IQR_price = IQR(price))

df %>% 
  group_by(room_type) %>% 
  summarize(avg_price = mean(price) , sd_price = sd(price) , median_price = quantile(price, 0.5) , IQR_price = IQR(price))

df %>% 
  filter(room_type == "Entire home/apt") %>%
  group_by(neighborhood) %>%
  summarize(avg_price = mean(price) , sd_price = sd(price) , median_price = quantile(price, 0.5) , IQR_price = IQR(price))

df %>% 
  filter(room_type == "Entire home/apt" & neighborhood == "Union Station") %>%
  group_by(bathrooms) %>%
  summarize(avg_price = mean(price) , sd_price = sd(price) , median_price = quantile(price, 0.5) , IQR_price = IQR(price))



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

conf_test(.95, "price", df)

mean(df$price)


# Q3 (Larry Part Two)

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

conf_test(.95, "price", df)

#Q3 (Jackie's Attempt)

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



#Q4 : A T-Test was performed. 

t.test(df$price, mu = 200, alternative = "greater", conf.level = 0.95)

#Q5. Visualize price to test for normality.

#Histogram

ggplot(df, aes(x = price)) + 
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", 
                 color = "black")+
  geom_density(color = "red", size = 1.2) +
  labs(x = "Price",
       y = "Density",
       title = "Airbnb Prices in Washington, DC") +
  theme_minimal()

#Use log(price) to try to normalize and make less skew

ggplot(df, aes(x = log(price))) + 
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", 
                 color = "black")+
  geom_density(color = "red", size = 1.2) +
  labs(x = "Price (Log)",
       y = "Density", 
       title = "Airbnb Prices in Washington, DC (Log)") + 
  theme_minimal()

#Boxplot

ggplot(df, aes(y = price)) +
  geom_boxplot(fill = "lightgreen", outlier.color = "red") +
  labs(y = "Price",
       title = "Boxplot of Airbnb Prices in DC") + 
  theme_minimal()

#Boxplot to compare price and bedrooms

boxplot(price~bedrooms, df)

#Boxplot to compare price and neighborhood

boxplot(price~neighborhood, df)

#Q6. What’s the best simple linear regression model for the “price” of the listings based on R-squared and residual standard error? 
#Compare/present the results of the tested models as a table in your report.

#Simple linear regression of price using neighborhood as predictor.
RegNeighborhood = lm(price~neighborhood, df)

#Simple linear regression of price using bedrooms as predictor.
RegBedrooms = lm(price~bedrooms, df)

#Simple linear regression of price using room type as predictor.
RegRoomType = lm(price~room_type, df)

#Simple linear regression of price using accommodates as predictor.
RegAccommodates = lm(price~accommodates, df)

#Simple linear regression of price using beds as predictor.
RegBeds = lm(price~beds, df)

#Simple linear regression of price using bathrooms as predictor.
RegBathrooms = lm(price~bathrooms, df)

#Simple linear regression of price using minimum nights as predictor.
RegMinNights = lm(price~min_nights, df)

#Simple linear regression of price using total reviews as predictor.
RegReviews = lm(price~total_reviews, df)

#Simple linear regression of price using host acceptance rate as predictor.
RegHostAcceptRate = lm(price~host_acceptance_rate, df)

#Simple linear regression of price using Superhost as predictor.
RegSuperhost = lm(price~superhost, df)

#Simple linear regression of price using host total listings as predictor.
RegHostListings = lm(price~host_total_listings, df)

#Simple linear regression of price using average rating as predictor.
RegAvgRating = lm(price~avg_rating, df)

#Compare the regression models of R-squared and residual standard error.

#Summaries
sumNeighborhood = summary(RegNeighborhood)
sumBedrooms = summary(RegBedrooms)
sumRoomType = summary(RegRoomType)
sumAccommodates = summary(RegAccommodates)
sumBeds = summary(RegBeds)
sumBathrooms = summary(RegBathrooms)
sumMinNights = summary(RegMinNights)
sumReviews = summary(RegReviews)
sumHostAcceptRate = summary(RegHostAcceptRate)
sumSuperhost = summary(RegSuperhost)
sumHostListings = summary(RegHostListings)
sumAvgRating = summary(RegAvgRating)

#Comparison table for all the models.

modelComp = data.frame(
  Model = c("Req with Neighborhood", "Reg with Bedrooms", "Reg with Room Type", 
            "Reg with Accommodates", "Reg with Beds", "Reg with Bathrooms", 
            "Reg with Min Nights", "Reg with Total Reviews", "Reg with Host Acceptance Rate",
            "Reg with Superhost", "Reg with Host Total Listings", "Reg with Average Rating"),
  SER = c(sumNeighborhood$sigma, sumBedrooms$sigma, sumRoomType$sigma, 
          sumAccommodates$sigma, sumBeds$sigma, sumBathrooms$sigma, 
          sumMinNights$sigma, sumReviews$sigma, sumHostAcceptRate$sigma,
          sumSuperhost$sigma, sumHostListings$sigma, sumAvgRating$sigma),
  R2 = c(sumNeighborhood$r.squared, sumBedrooms$r.squared, sumRoomType$r.squared, 
         sumAccommodates$r.squared, sumBeds$r.squared, sumBathrooms$r.squared, 
         sumMinNights$r.squared, sumReviews$r.squared, sumHostAcceptRate$r.squared,
         sumSuperhost$r.squared, sumHostListings$r.squared, sumAvgRating$r.squared)
)

modelComp

#Model with lowest residual standard error
modelComp$Model[which.min(modelComp$SER)]

#Model with highest residual standard error
modelComp$Model[which.max(modelComp$SER)]

#Q7:
# multiple linear model  
# predictors: bedrooms + neighborhood + room_type + accmodates only analysis 
filtReg = lm(price ~ bedrooms + neighborhood + room_type + accommodates,df)

summary(filtReg)

# coefficent table 
coef =  coef(summary(filtReg))
coef

#Diagnostic plots
plot(filtReg, which = 1)
plot(filtReg, which = 2)

#Summary Of Data Model" sigma, R2, AdjR
ModelName = c("filtReg") 
Sigma = c(summary(filtReg)$sigma)
RSqrd = c(summary(filtReg)$r.squared)
AdjRSqrd = c(summary(filtReg)$adj.r.squared)

SummaryTable = data.frame(ModelName,Sigma,RSqrd,AdjRSqrd)
SummaryTable

# predicted price
df$prdctPrce = predict(filtReg)

#Group, summarize, and keep top 5 groups 
topFiveGroup = df %>%
  group_by(neighborhood, room_type) %>%
  summarize(
    numLstngs = n(),
    avgPred = mean(prdctPrce))%>%
  ungroup() %>%
  filter(numLstngs >= 5) %>%           
  arrange(desc(avgPred)) %>%
  slice_head(n = 5)
topFiveGroup

#ggplot top 5

ggplot(topFiveGroup,
       aes(x = avgPred,
           y = reorder(paste(neighborhood, "/", room_type), avgPred))) +
  geom_col(fill = "steelblue") +
  labs(title = "Top 5 By Predicted Price (Avg.)",
       x = "Avg. predicted price", y = "Neighborhood / Room Type") +
  theme_minimal()



#Q8: Ran correlations against bedrooms and accommodates. mild correlation. Still mulling this one over.

cor(df$bedrooms, df$accommodates)












































## Finding actionable insights: 

## EXTRA: 

pb_reg = lm(price~beds , df)
summary(pb_reg)
plot(pb_reg)
df[c(182,183,109),]
df6 = df[-c(109,192,183),]
pb_reg2 = lm(price~beds , df6)
summary(pb_reg2)


pa_reg= lm(price~accommodates, df)
summary(pa_reg)
plot(pa_reg)
df[c(109,1098,1767),]
pa_reg2 = lm(price~accommodates, df7)
summary(pa_reg2)
df7 = df[-c(109,192,183),]

pb_reg = lm(price~bedrooms, df)
summary(pb_reg)
plot(pb_reg)
df[c(327,503,1188,1524),]
df8 = df[-c(327,503,1188,1524),]
pb_reg2 =lm(price~bedrooms, df8)
summary(pb_reg2)
## removing the outlier values from these only really increased the R^2 by around 0.02

pn_reg = lm(price~neighborhood, df)
summary(pn_reg)
plot(pn_reg)

prt_reg = lm(price~room_type, df)
summary(prt_reg)
plot(prt_reg)

mult_reg = lm(price~bedrooms+accommodates+neighborhood, df)
summary(mult_reg)

df9 = df %>%
  filter(neighborhood == "Capitol Hill" | neighborhood == "Edgewood" | neighborhood == "Union Station" |
           neighborhood == "Dupont Circle" | neighborhood == "Columbia Heights")

summary(lm(price~neighborhood , df9))
## due to the highly imbalanced dataset and poor regression 
# narrow into only entire homes since 1500 (over 80 percent of the dataset)
df10 = df %>%
  filter(room_type ==  "Entire home/apt")


ggplot(df10 , aes(x = bedrooms, y = price)) + 
  geom_point() + geom_jitter() + geom_smooth(method = "lm" , se = FALSE) + 
  facet_wrap(~neighborhood, scales = "free_x")

ggplot(df10 , aes(x = accommodates, y = price)) + 
  geom_point() + geom_jitter() + geom_smooth(method = "lm" , se = FALSE) + 
  facet_wrap(~neighborhood, scales = "free_x")

df11 = df10 %>%
  mutate(bedrooms_f = as.factor(bedrooms)) %>%
  filter(neighborhood == "Capitol Hill")


summary(lm(price~bedrooms+accommodates, df11))

ggplot(df11, aes(x= accommodates, y = price,color = factor(bedrooms))) + geom_point() + facet_wrap(~superhost , scales = "free_x") 

df11 %>%
  select_if(is.numeric) %>%
  cor()













## possibilly create a new metric 

ggplot(df, aes(y = log(price), x=log(1+host_total_listings*host_acceptance_rate))) + geom_point()


ggplot(df, aes(y = price, x=0.5*accommodates + 0.5*bedrooms)) + geom_point()


## maybe explore avg rating

ggplot(df, aes(y= price, x = avg_rating)) + geom_point()










