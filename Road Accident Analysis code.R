library(ggplot2)      
library(dplyr)        
library(tidyr)        
library(readxl)  
library(class)
library(corrplot)

# Load data
Road_Accidents_data <- read_excel("C:/Users/akash/OneDrive/AppData/Desktop/BCA/Sem 5/CAP-484/CA2 and CA4/Road Accidents data.xlsx")
head(Road_Accidents_data)

#....................code of research que.............................


# que 1
ggplot(Road_Accidents_data, aes(x = Weather_Condition)) + 
  geom_bar(fill = "red", position = "dodge") + 
  labs(title = "Accident Severity by Weather Condition",
       x = "Weather Condition",
       y = "Number of Accidents") +
  theme_minimal()

# que 2
ggplot(Road_Accidents_data[Road_Accidents_data$Alcohol_Involved == "Yes",], aes(x = Vehicle_Type)) +
  geom_bar(fill = "blue") +
  labs(title = "Number of Alcohol-Related Accidents by Vehicle Type",
       x = "Vehicle Type",
       y = "Number of Accidents") +
  theme_minimal()

# que 3
Road_Accidents_data$Hour <- as.integer(format(as.POSIXct(Road_Accidents_data$Time_of_Accident, format = "%H:%M"), "%H"))
accidents_by_hour <- table(Road_Accidents_data$Hour)

ggplot(data.frame(Hour = as.numeric(names(accidents_by_hour)), Count = as.vector(accidents_by_hour)), aes(Hour, Count)) +
  geom_area(fill = "skyblue") +
  labs(title = "Cumulative Number of Accidents by Hour",
       x = "Hour of Day",
       y = "Cumulative Number of Accidents") +
  theme_minimal()


# que 4
ggplot(Road_Accidents_data, aes(x = Vehicle_Type, fill = Vehicle_Type)) + 
  geom_bar() +
  labs(title = "Number of Accidents by Vehicle Type",
       x = "Vehicle Type",
       y = "Number of Accidents") 

#....................	Statical and ML technique.............................

# 1. Regression Model
model <- lm(Casualties ~ Vehicle_Speed , data = Road_Accidents_data)
summary(model)

plot(Road_Accidents_data$Vehicle_Speed, Road_Accidents_data$Casualties, 
     main = "Regression: Casualties vs Vehicle Speed",
     xlab = "Vehicle Speed", ylab = "Casualties", 
     col = "blue", pch = 20)
abline(lm(Casualties ~ Vehicle_Speed, data = Road_Accidents_data), col = "red", lwd = 2)

# 2. Correlation
numeric_columns <- Road_Accidents_data[, c("Driver_Age", "Casualties", "Vehicle_Speed")]
cor_matrix <- cor(numeric_columns)

corrplot(cor_matrix, method = "number")

# 3. ANOVA
anova_model <- aov(Vehicle_Speed ~ Road_Type + Severity, data = Road_Accidents_data)
summary(anova_model)

boxplot(Vehicle_Speed ~ Road_Type, data = Road_Accidents_data,
        main = "Vehicle Speed by Road Type",
        xlab = "Road Type", ylab = "Vehicle Speed",
        col = c("skyblue", "orange", "purple"))

boxplot(Vehicle_Speed ~ Severity, data = Road_Accidents_data,
        main = "Vehicle Speed by Severity",
        xlab = "Severity", ylab = "Vehicle Speed",
        col = c("lightgreen", "pink", "yellow"))


# 4. K-Means Clustering
set.seed(123)
k <- 3

kmeans_result <- kmeans(Road_Accidents_data[, c("Driver_Age", "Vehicle_Speed")],
                        centers = k, nstart = 10)

Road_Accidents_data$Cluster <- as.factor(kmeans_result$cluster)

ggplot(Road_Accidents_data, aes(x = Driver_Age, y = Vehicle_Speed, color = Cluster)) +
  geom_point(size = 2, alpha = 0.8) +  
  geom_point(data = as.data.frame(kmeans_result$centers),
             aes(x = Driver_Age, y = Vehicle_Speed),
             color = "black", shape = 8, size = 6, inherit.aes = FALSE) +  
  labs(
    title = "K-Means Clustering of Road Accident Data",
    x = "Driver Age",
    y = "Vehicle Speed",
    color = "Cluster"
  ) +
  theme_minimal() 
