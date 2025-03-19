# r- project on Swiggy

#loading the required libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)

#loading the required csv file
swiggy_data<-read_csv("C:/Users/bhavy/OneDrive/Desktop/swiggy data.csv")

#viewing the data set
View(swiggy_data)

#view structure
str(swiggy_data)

#first few rows
swiggy_1<-head(swiggy_data)
View(swiggy_1)

# first 24 rows
head(swiggy_data,24)



#summary
summary(swiggy_data)

#finding missing values
missing_value<- colSums(is.na(swiggy_data))
missing_value

# Check for duplicates
duplicates <- swiggy_data[duplicated(swiggy_data), ]
nrow(duplicates)

tail
swiggy_2<-tail(swiggy_data)
View(swiggy_2)

#bringing out data of state and sales
selected_data<-swiggy_data%>%
  select(`Average_ratings`, `Number_of_ Reviews`)

View(selected_data)

#renaming the coloums
renamed_data<-selected_data %>% 
  rename(
    Avg_rating=Average_ratings,
    Num_of_Review=`Number_of_ Reviews`
  )
#viewing the result
View(head(renamed_data))

View(swiggy_data)

#filter_data

filtered_data<-swiggy_data%>%
  filter(Price>225)

#viewing the filtered data
View(head(filtered_data))

#analysis and visualisation of dataset

# 1. Which areas have the highest average restaurant rating?
area_avg_ratings <- swiggy_data %>%
  group_by(Area) %>%
  summarize(Average_Rating = mean(Average_ratings)) %>%
  arrange(desc(Average_Rating))

#Viewing the result
View(area_avg_ratings)

ggplot(area_avg_ratings, aes(x = reorder(Area, Average_Rating), y = Average_Rating)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Average Ratings by Area", x = "Area", y = "Average Rating")

# 2. What is the distribution of delivery times?
ggplot(swiggy_data, aes(x = Delivery.Time)) +
  geom_histogram(binwidth = 5, fill = "red", color = "black") +
  labs(title = "Distribution of Delivery Times", x = "Delivery Time (minutes)", y = "Count")

# 3. Which food types are most commonly offered?
food_types <- swiggy_data %>%
  separate_rows(Food_Type, sep = ",") %>%
  count(Food_Type, sort = TRUE)

print(food_types)
View(food_types)
head(food_types)
View(head(food_types))

ggplot(head(food_types), aes(x = reorder(Food_Type, n), y = n)) +
  geom_bar(stat = "identity", fill = "forestgreen", width = 0.8) +
  labs(title = "Most Common Food Types", x = "Food Type", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_flip()



# 4. What is the relationship between price and delivery time?
ggplot(swiggy_data, aes(x = Price, y = Delivery.Time)) +
  geom_point(color = "blue") +
  labs(title = "Price vs Delivery Time", x = "Price", y = "Delivery Time")



# 5. Which restaurants have the most reviews?
restaurant_reviews <- swiggy_data %>%
  arrange(desc(`Number_of_ Reviews`))
  
View(head(restaurant_reviews))

#print
print(restaurant_reviews)
View(head(restaurant_reviews))
swiggy_data %>% 
ggplot(head(restaurant_reviews), aes(x = reorder(Restaurant, `Number_of_ Reviews`), y = `Number_of_ Reviews`)) +
  geom_col(fill = "purple", width = 0.8) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 10 Restaurants by Number of Reviews", x = "Restaurant", y = "Number of Reviews")

# 6. What is the average delivery time by city?
city_avg_delivery <- swiggy_data %>%
  group_by(City) %>%
  summarize(Average_Delivery_Time = mean(Delivery.Time)) %>%
  arrange(desc(Average_Delivery_Time))

#viewing the result
View(city_avg_delivery)

ggplot(city_avg_delivery, aes(x = reorder(City, Average_Delivery_Time), y = Average_Delivery_Time)) +
  geom_col(fill = "gold") +
  coord_flip() +
  labs(title = "Average Delivery Time by City", x = "City", y = "Average Delivery Time")

# 7. What is the distribution of restaurant ratings?
ggplot(swiggy_data, aes(x = Average_ratings)) +
  geom_density(fill = "cyan", alpha = 0.5) +
  labs(title = "Distribution of Restaurant Ratings", x = "Average Ratings", y = "Density")

# 8. How does the number of reviews relate to average ratings?
ggplot(swiggy_data, aes(x = `Number_of_ Reviews`, y = Average_ratings)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Number of Reviews vs Average Ratings", x = "Number of Reviews", y = "Average Ratings")

# 9. Which cities have the highest average prices?
city_avg_price <- swiggy_data %>%
  group_by(City) %>%
  summarize(Average_Price = mean(Price)) %>%
  arrange(desc(Average_Price))

#viewing the result
print(city_avg_price)

ggplot(city_avg_price, aes(x = reorder(City, Average_Price), y = Average_Price)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(title = "Average Prices by City", x = "City", y = "Average Price")

# 10. What are the most expensive restaurants?
expensive_restaurants <- swiggy_data %>%
  arrange(desc(Price)) %>%
  head(10)

#viewing the result
View(expensive_restaurants)

# Boxplot
ggplot(expensive_restaurants, aes(x = "", y = Price)) +
  geom_boxplot(fill = "firebrick", alpha = 0.7) +
  labs(title = "Price Range of Top 10 Most Expensive Restaurants", x = "Restaurants", y = "Price")

#  Scatter plot with labels
ggplot(expensive_restaurants, aes(x = reorder(Restaurant, Price), y = Price)) +
  geom_point(size = 3, color = "darkred") +
  geom_text(aes(label = Price), hjust = -0.2, color = "black") +
  coord_flip() +
  labs(title = "Top 10 Most Expensive Restaurants", x = "Restaurant", y = "Price")

# 10 Pie chart
expensive_restaurants_pie <- expensive_restaurants %>%
  mutate(Percentage = Price / sum(Price) * 100, 
         Label = paste(Restaurant, round(Percentage, 1), "%"))

ggplot(expensive_restaurants_pie, aes(x = "", y = Percentage, fill = Restaurant)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Share of Prices Among Top 10 Most Expensive Restaurants") +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5))


# 11. How does average rating vary by food type?
food_avg_rating <- swiggy_data %>%
  separate_rows(Food_Type, sep = ",") %>%
  group_by(Food_Type) %>%
  summarize(Average_Rating = mean(Average_ratings)) %>%
  arrange(desc(Average_Rating))

#viewing the result
View(food_avg_rating)

ggplot(food_avg_rating, aes(x = reorder(Food_Type, Average_Rating), y = Average_Rating)) +
  geom_col(fill = "mediumorchid") +
  coord_flip() +
  labs(title = "Average Rating by Food Type", x = "Food Type", y = "Average Rating")

# 12. What is the price range distribution?
# Visualization with density plot
ggplot(swiggy_data, aes(x = Price)) +
  geom_density(fill = "skyblue", alpha = 0.6) +
  labs(title = "Price Range Distribution", x = "Price", y = "Density")

# 13. What is the average delivery time for different food types?
food_delivery_time <- swiggy_data %>%
  separate_rows(Food_Type, sep = ",") %>%
  group_by(Food_Type) %>%
  summarize(Average_Delivery_Time = mean(Delivery.Time)) %>%
  arrange(desc(Average_Delivery_Time))

#viewing the result
View(food_delivery_time)

# Visualization with a lollipop chart
ggplot(food_delivery_time, aes(x = reorder(Food_Type, Average_Delivery_Time), y = Average_Delivery_Time)) +
  geom_col(fill = "salmon") +
  coord_flip() +
  labs(title = "Average Delivery Time by Food Type", x = "Food Type", y = "Delivery Time")

# 14. How does the number of reviews vary by city?
city_reviews <- swiggy_data %>%
  group_by(City) %>%
  summarize(Total_Reviews = sum(`Number_of_ Reviews`)) %>%
  arrange(desc(Total_Reviews))

#viewing the result
View(city_reviews)

ggplot(city_reviews, aes(x = reorder(City, Total_Reviews), y = Total_Reviews)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Total Reviews by City", x = "City", y = "Number of Reviews")


# 15. What is the correlation between delivery time and ratings?
ggplot(swiggy_data, aes(x = Delivery.Time, y = Average_ratings)) +
  geom_point(alpha = 0.5, color = "darkred") +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Delivery Time vs Ratings", x = "Delivery Time", y = "Average Ratings")

# 16. What is the average price of meals by area?
Area_price<-swiggy_data %>%
  group_by(Area) %>%
  summarise(Average_Price = mean(Price)) %>%
  arrange(desc(Average_Price))

#  Visualize the average price of meals by area.
swiggy_data %>%
  group_by(Area) %>%
  summarise(Average_Price = mean(Price)) %>%
  ggplot(aes(x = reorder(Area, -Average_Price), y = Average_Price)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Average Price by Area", x = "Area", y = "Average Price")

# 17. What is the total price of all orders in each city?
swiggy_data %>%
  group_by(City) %>%
  summarise(Total.Price = sum(Price))

# 18. Visualize the correlation between delivery time and the number of reviews.
ggplot(swiggy_data, aes(x = `Delivery.Time`, y = `Number_of_ Reviews`)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Delivery Time vs Number of Reviews", x = "Delivery Time", y = "Number of Reviews")


#19. Which city has the highest number of restaurants?
swiggy_data %>%
  count(City, name = "Restaurant_Count") %>%
  arrange(desc(Restaurant_Count))

#20.  Visualize the number of restaurants by city.
swiggy_data %>%
  count(City, name = "Restaurant_Count") %>%
  ggplot(aes(x = reorder(City, -Restaurant_Count), y = Restaurant_Count)) +
  geom_bar(stat = "identity", fill = "brown") +
  coord_flip() +
  labs(title = "Number of Restaurants by City", x = "City", y = "Number of Restaurants")
