##########################################################################
#
# Project     : ANALYZING SALES AND PREDICTING FRAUD AND LATE DELIVERY RISKS
# Presenters  : Modhaka Jampana,Sree Meghana Reddy Kandi,Gnana Deepak Madduri
# Course      : CS5610 - Advanced R for Data Science
# Instructor  : Dr.Wassnaa Al-Mawee, Ph.D., 
#               Department of Computer Science
#
# 
##########################################################################

# install.packages("ggmap")
# install.packages("tmaptools")
# install.packages("RCurl")
# install.packages("jsonlite")
# install.packages("leaflet")
# install.packages("tidyverse")
# install.packages("RColorBrewer")
# install.packages("ggthemes")
# install.packages("lubridate")
# install.packages("geosphere")
# install.packages("splitTools")
# install.packages("ROCR")
# install.packages("plotROC")
# install.packages("DataExplorer")
# install.packages("WVPlots")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("caret")
# install.packages("geosphere")
# install.packages("readr")
# install.packages("glmnet")
# install.packages("plotmo")
# install.packages("stringr")
# install.packages("leaflet.extras")
# install.packages("randomForest")
# install.packages("e1071")
# install.packages("rpart")
# install.packages("shiny")
# install.packages("plotly")
# install.packages("tensorflow")
# install.packages("keras")
# install.packages("MASS")

library(ggmap)                  # Load ggmap package for visualizations using Google Maps
library(tmaptools)              # Load tmaptools package for working with thematic maps
library(RCurl)                  # Load RCurl package for making HTTP requests
library(jsonlite)               # Load jsonlite package for working with JSON data
library(leaflet)                # Load leaflet package for interactive maps
library(tidyverse)              # Load tidyverse package for data manipulation and visualization
library(RColorBrewer)           # Load RColorBrewer package for color palettes
library(ggthemes)               # Load ggthemes package for additional themes for ggplot2
library(lubridate)              # Load lubridate package for working with dates and times
library(geosphere)              # Load geosphere package for geospatial calculations
library(splitTools)             # Load splitTools package for splitting data into training and testing sets
library(ROCR)                   # Load ROCR package for visualizing performance of classification models
library(plotROC)                # Load plotROC package for plotting ROC curves
library(DataExplorer)           # Load DataExplorer package for data exploration
library(WVPlots)                # Load WVPlots package for visualizing missing data
library(dplyr)                  # Load dplyr package for data manipulation
library(ggplot2)                # Load ggplot2 package for data visualization
library(caret)                  # Load caret package for machine learning tools
library(geosphere)              # Load geosphere package for geospatial calculations
library(readr)                  # Load readr package for reading CSV files
library(glmnet)                 # Load glmnet package for fitting elastic net models
library(plotmo)                 # Load plotmo package for visualizing partial dependence plots
library(stringr)                # Load stringr package for replacing special characters of country and city names
library(leaflet.extras)         # Load leaflet.extras package for creating interactive maps
library(randomForest)           # Load randomForest package for classification and regression models.
library(e1071)                  # Load e1071 package for support vector machines (SVMs)
library(rpart)                  # Load rpart package for building decision trees
library(shiny)                  # Load shiny package for creating interactive web applications.
library(plotly)                 # Load plotly package for creating interactive plots and charts
library(tensorflow)             # Load tensorflow package for building and training machine learning models
library(keras)                  # Load keras package for building and training neural networks.
library(MASS)                   # Load Mass package for mathematical statistics.


# Read the dataset
supply_chain <- read.csv('DataCoSupplyChainDataset.csv')

# cleansing the data begins here to remove special characters, missing values, whitespaces etc.

country <- unique(supply_chain$Order.Country)

# Convert character encoding to ASCII
country_ascii <- iconv(country, from = "UTF-8", to = "ASCII//TRANSLIT")

# Remove non-alphanumeric characters
country_clean <- gsub("[^[:alnum:][:space:]]", "", country_ascii)

# Remove leading/trailing whitespace
country_clean <- trimws(country_clean)

# Get unique country names
unique_country <- unique(country_clean)
unique_country

# create a vector of unique country values
countries <- unique(supply_chain$Order.Country)

# loop over each unique country value and replace with the corresponding translated value
for (country in countries) {
  index <- supply_chain$Order.Country == country
  if (anyNA(index)) {
    next  # skip iteration if there are missing values
  }
  supply_chain$Order.Country[index] <- str_replace(supply_chain$Order.Country[index], country, unique_country[country == countries])
}


country <- unique(supply_chain$Customer.Country)

# Convert character encoding to ASCII
country_ascii <- iconv(country, from = "UTF-8", to = "ASCII//TRANSLIT")

# Remove non-alphanumeric characters
country_clean <- gsub("[^[:alnum:][:space:]]", "", country_ascii)

# Remove leading/trailing whitespace
country_clean <- trimws(country_clean)

# Get unique country names
unique_country <- unique(country_clean)

# create a vector of unique country values
countries <- unique(supply_chain$Customer.Country)

# loop over each unique country value and replace with the corresponding translated value
for (country in countries) {
  index <- supply_chain$Customer.Country == country
  supply_chain$Customer.Country[index] <- str_replace(supply_chain$Customer.Country[index], country, unique_country[country == countries])
}


# Get unique customer city names
customer_city <- unique(supply_chain$Customer.City)

# Convert character encoding to ASCII
customer_city_ascii <- iconv(customer_city, from = "UTF-8", to = "ASCII//TRANSLIT")

# Remove non-alphanumeric characters
customer_city_clean <- gsub("[^[:alnum:][:space:]]", "", customer_city_ascii)

# Remove leading/trailing whitespace
customer_city_clean <- trimws(customer_city_clean)

# Get unique customer city names
unique_customer_city <- unique(customer_city_clean)

# Loop over each unique customer city value and replace with the corresponding translated value
for (city in unique_customer_city) {
  index <- supply_chain$Customer.City == city
  supply_chain$Customer.City[index] <- str_replace(supply_chain$Customer.City[index], city, unique_customer_city[city == unique_customer_city])
}

# Get unique city names
city <- unique(supply_chain$Order.City)
city

# Convert character encoding to ASCII
city_ascii <- iconv(city, from = "UTF-8", to = "ASCII//TRANSLIT")

# Remove non-alphanumeric characters
city_clean <- gsub("[^[:alnum:][:space:]]", "", city_ascii)

# Remove leading/trailing whitespace
city_clean <- trimws(city_clean)

# Get unique city names
unique_city <- unique(city_clean)
unique_city

# create a vector of unique city values
cities <- unique(supply_chain$Order.City)

# loop over each unique city value and replace with the corresponding translated value
for (city in cities) {
  index <- supply_chain$Order.City == city
  if (anyNA(index)) {
    next  # skip iteration if there are missing values
  }
  supply_chain$Order.City[index] <- str_replace(supply_chain$Order.City[index], city, unique_city[city == cities])
}

print(supply_chain)

x <- c("Rep�blica Dominicana", "Turqu�a", "M�xico", "Espa�a")

# replace "México" with "mexico"
y <- c("Dominican Republic", "Turkey", "Mexico", "Spain")

# print the updated vector
for (i in 1:length(x)) {
  supply_chain$`Customer.Country` <-
    supply_chain$`Customer.Country` %>% str_replace(x[i], y[i])
  supply_chain$`Order.Country` <-
    supply_chain$`Order.Country` %>% str_replace(x[i], y[i])
}

##################################################################################
# DELIVERY RISK ANALYSIS AND PREDICTION ON SUPPLY CHAIN
##################################################################################

# Risk VS No-risk status on all the global regions

Delivery_market <- function(data) {
  data %>% group_by(Market, Late_delivery_risk) %>%
    # Count the occurrences of each group
    count() %>%
    # Mutate the Late_delivery_risk column to replace 1 with 'Risk' and other values with 'No Risk'
    mutate(Late_delivery_risk = ifelse(Late_delivery_risk == 1, 'Risk', 'No Risk')) %>%
    # Group the data by Market
    group_by(Market) %>%
    # Calculate the percentage of occurrences in each group
    mutate(percent = n / sum(n)) %>%
    # Create a ggplot object with Market on the x-axis, percent on the y-axis, and Late_delivery_risk as fill color
    ggplot(data = ., aes(x = Market, y = percent, fill = Late_delivery_risk)) +
    # Create a bar chart with fill color based on Late_delivery_risk and position set to "fill"
    geom_bar(position = "fill", stat = "identity") +
    labs(title = "Proportion of Risk VS No-Risk deliveries across various Regions ") +
    scale_fill_manual(values = c("#8FBC8F", "#FFA07A"))
}


#To check Late Delivery Risk by Various Departments
Delivery_department <- function(data) {
  data %>% group_by(Department.Name, Late_delivery_risk) %>%
    count() %>%
    mutate(Late_delivery_risk = ifelse(Late_delivery_risk == 1, 'Risk', 'No Risk')) %>%
    group_by(Department.Name) %>%
    mutate(percent = n / sum(n)) %>%
    ggplot(data = .,
           aes(x = Department.Name, y = percent, fill = Late_delivery_risk)) +
    geom_bar(position = "fill", stat = "identity")+
    ggtitle("Late Delivery Risk by Various Departments") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_fill_manual(values = c("lightseagreen", "hotpink3"))
}

# To calculate Percentage of Late Deliveries by Shipping Modes.
Delivery_shipping <- function(data) {
  data %>% group_by(Shipping.Mode, Late_delivery_risk) %>%
    count() %>%
    mutate(Late_delivery_risk = ifelse(Late_delivery_risk == 1, 'Risk', 'No Risk')) %>%
    group_by(Shipping.Mode) %>%
    mutate(percent = n / sum(n)) %>%
    ggplot(data = ., aes(x = Shipping.Mode, y = percent, fill = Late_delivery_risk)) +
    geom_bar(position = "fill", stat = "identity")+
    labs(x = "Shipping Mode", y = "Percentage of Orders",
         title = "Percentage of Late Deliveries by Shipping Modes") +
    scale_fill_manual(values = c("#87CEFA", "#708090"))
}


# 
Delivery_location <- function(data) {
  data %>% select(Latitude, Longitude) %>%
    leaflet(width = 1200, height = "600px") %>%
    addTiles() %>%
    addMarkers(clusterOptions = markerClusterOptions(), popup = "order location")
}


#To evaluate Top 8 Cities with Late Deliveries in the US
Delivery_delay <- function(data) {
  
  Late_delivery <- data$Late_delivery_risk[data$Order.Region == "West of USA" |
                                             data$Order.Region == "US Center" |
                                             data$Order.Region == "East of USA" |
                                             data$Order.Region == "South of USA"]
  Order.City <- data$Order.City[data$Order.Region == "West of USA" |
                                  data$Order.Region == "US Center" |
                                  data$Order.Region == "East of USA" |
                                  data$Order.Region == "South of USA"]
  
  # Create a data frame LD_data with Late_delivery and Order.City
  LD_data <- data.frame(Late_delivery, Order.City)
  
  # Count occurrences of Late_delivery and Order.City and name the count column as "count"
  LD_data <- LD_data %>%
    count(Late_delivery, Order.City, name = "count")
  
  # Arrange LD_data by count column in descending order and keep top 12 rows
  LD_data <- LD_data %>% arrange(desc(count)) %>% slice(1:12)
  
  
  # Visualize late deliveries by city using ggplot
  ggplotly(ggplot(data = LD_data, aes(x = Order.City, y = count, fill = Late_delivery)) +
             geom_bar(stat = 'identity') +
             labs(x = "Order City", y = "Number of Orders", title = "Top 8 Cities with Late Deliveries in the US")+
             theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))
}


# prediction on late delivery begins

set.seed(75)
trainIndex <- createDataPartition(supply_chain$Late_delivery_risk, p = 0.8, 
                                  list = FALSE, times = 1)
supply_chain_train <- supply_chain[ trainIndex,]
supply_chain_test  <- supply_chain[-trainIndex,]

# Fit logistic regression model
glm_model <- glm(Late_delivery_risk ~ Shipping.Mode, data = supply_chain_train, family = binomial())
glm_model

# Make predictions on test data
supply_chain_test$predicted_risk <- predict(glm_model, newdata = supply_chain_test, type = "response")

# Convert predicted probabilities to binary predictions
supply_chain_test$predicted_late_delivery <- ifelse(supply_chain_test$predicted_risk >= 0.5, "Yes", "No")

# Evaluate model accuracy on test data
glm_predictions <- predict(glm_model, newdata = supply_chain_test, type = "response")
glm_predictions <- ifelse(glm_predictions > 0.5, 1, 0)
confusion_matrix <- table(glm_predictions, supply_chain_test$Late_delivery_risk)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy
precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
f_measure <- 2 * precision * recall / (precision + recall)

cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F-measure:", f_measure, "\n")



# Fit the Random Forest Model
set.seed(210)

# Fit random forest model
rf_model <- randomForest(Late_delivery_risk ~ Shipping.Mode, data = supply_chain_train)

# Make predictions on test data
supply_chain_test$predicted_late_delivery <- predict(rf_model, newdata = supply_chain_test, type = "class")

# Evaluate model accuracy on test data
confusion_matrix <- table(supply_chain_test$predicted_late_delivery, supply_chain_test$Late_delivery_risk)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy
precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
f_measure <- 2 * precision * recall / (precision + recall)

cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F-measure:", f_measure, "\n")



#Fit the Naive Bayes Model
set.seed(210)

#Fit Naive Bayes model
nb_model <- naiveBayes(Late_delivery_risk ~ Shipping.Mode, data = supply_chain_train)

#Make predictions on test data
supply_chain_test$predicted_late_delivery <- predict(nb_model, newdata = supply_chain_test)

#Evaluate model accuracy on test data
confusion_matrix <- table(supply_chain_test$predicted_late_delivery, supply_chain_test$Late_delivery_risk)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy
precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
f_measure <- 2 * precision * recall / (precision + recall)

cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F-measure:", f_measure, "\n")



# subset of train and test data sets
sub_supply_chain_train <- supply_chain_train[, c("Late_delivery_risk", "Shipping.Mode")]
sub_supply_chain_test <- supply_chain_test[, c("Late_delivery_risk", "Shipping.Mode")]

#Fit Decision Tree Model
set.seed(129)

# Convert Late_delivery_risk to factor
sub_supply_chain_train$Late_delivery_risk <- as.factor(sub_supply_chain_train$Late_delivery_risk)
sub_supply_chain_test$Late_delivery_risk <- as.factor(sub_supply_chain_test$Late_delivery_risk)

#Fit decision tree model
tree_model <- rpart(Late_delivery_risk ~ Shipping.Mode, data = sub_supply_chain_train)

#Make predictions on test data
sub_supply_chain_test$predicted_late_delivery <- predict(tree_model, newdata = sub_supply_chain_test, type = "class")

#Evaluate model accuracy on test data
confusion_matrix <- table(sub_supply_chain_test$predicted_late_delivery, sub_supply_chain_test$Late_delivery_risk)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy
precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
f_measure <- 2 * precision * recall / (precision + recall)

cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F-measure:", f_measure, "\n")



##################################################################################
# FRAUD DETECTION ON SUPPLY CHAIN
##################################################################################


# Create a categorical variable for response.
supply_chain_model <- supply_chain %>%
  mutate(y_fraud = if_else(`Order.Status` == 'SUSPECTED_FRAUD', 1, 0))
supply_chain_yfraud <- supply_chain_model

supply_chain_model <- supply_chain[c("Type", "Sales.per.customer")]


head(supply_chain_model)


supply_chain_model[c('Benefit.per.order', 'Sales.per.customer', 'Sales', 'Order.Item.Total','y_fraud')] <- supply_chain_yfraud[c('Benefit.per.order', 'Sales.per.customer', 'Sales', 'Order.Item.Total','y_fraud')]
str(supply_chain_model)

for (x in names(supply_chain_model)) {
  print(paste(x ,':', length(unique(supply_chain_model[[x]]))))
}
table(supply_chain_model$Type)
supply_chain_model$Type <- recode(supply_chain_model$Type, "DEBIT"=7, "TRANSFER"=5, "PAYMENT"=4, "CASH"=2)
#View(supply_chain_model)



# Set the seed for reproducibility
set.seed(42)
# Split the data into training and testing sets
split <- createDataPartition(supply_chain_model$y_fraud, p = 0.8, list = FALSE)
train_data <- supply_chain_model[split, ]
test_data <- supply_chain_model[-split, ]
# Fit a logistic regression model on the training data
model <- glm(y_fraud ~ ., data = train_data, family = "binomial")
# Convert y_fraud to factor with levels "0" and "1"
test_data$y_fraud <- factor(test_data$y_fraud, levels = c("0", "1"))
# Make predictions on the testing data
pred <- predict(model, newdata = test_data, type = "response")
#pred_class <- ifelse(pred > 0.5, 1, 0)
pred_class <- factor(ifelse(pred > 0.5, "1", "0"), levels = c("0", "1"))
# Print the accuracy score
accuracy <- confusionMatrix(pred_class, test_data$y_fraud)$overall['Accuracy']
print(paste0("Accuracy: ", round(accuracy, 3)))
# Print the confusion matrix
cm <- confusionMatrix(pred_class, test_data$y_fraud)
print(cm$table)



# Fit a LDA model on the training data
model <- lda(y_fraud ~ ., data = train_data)

# Make predictions on the testing data
pred <- predict(model, newdata = test_data)

# Print the accuracy score
accuracy <- confusionMatrix(pred$class, test_data$y_fraud)$overall['Accuracy']
print(paste0("Accuracy: ", round(accuracy, 3)))

# Print the confusion matrix
cm <- confusionMatrix(pred$class, test_data$y_fraud)
print(cm$table)


# Calculate the ratio between Fraud and valid data and plot the pie chart
Fraud_valid <- function(data) {
  data %>% count(y_fraud) %>% 
    plot_ly(labels = ~ factor(y_fraud), values = ~ n, type = "pie", marker = list(colors = c("#F5DEB3", "#CD0000"))) %>% 
    layout(title = "Fraudulent vs Valid Transaction Ratio", showlegend = T, margin = list(l = 0, r = 0, b = 0, t = 50, pad = 0), legend = list(orientation = "h"))
}


# Define a function to calculate order status and plot the pie chart
Fraud_order_status  <- function(data) {
  unique(data$`Order.Status`) 
  data %>% count(`Order.Status`) %>% 
    plot_ly(labels = ~ `Order.Status`, values = ~ n, type = "pie", hole = 0.4, marker = list(colors = rainbow(length(unique(data$`Order.Status`))))) %>% 
    layout(title = "Percentages Of Various Order Status", showlegend = T, margin = list(l = 0, r = 0, b = 0, t = 50, pad = 0), legend = list(orientation = "h"))
}


##################################################################################
# DATA ANALYSIS ON SUPPLY CHAIN DATA
##################################################################################


# Define a function to create customer segment and plot the pie chart
create_customer_segment <- function(data) {
  data_Customer_Segment <- data %>%
    group_by(`Customer.Segment`) %>%
    summarize(Number_of_Orders = n()) %>%
    mutate(Percent_of_Orders = (Number_of_Orders / sum(Number_of_Orders)) * 100) %>%
    arrange(desc(Percent_of_Orders))
  
  
  colors <- c("#F9A825", "#1E88E5", "#43A047")
  plot_ly(data_Customer_Segment, labels = ~`Customer.Segment`, 
          values = ~Percent_of_Orders, type = "pie",
          textinfo = "label+percent",
          marker = list(colors = colors)) %>%
    layout(title = "Percentage of Orders by Customer Segment",
           width = 600, height = 600)
}

# Define a function to get top products and plot the graph
create_top_products <- function(data) {
  top3_products <- supply_chain %>% 
    group_by(Product.Name) %>% 
    summarise(Total_Sales = sum(Order.Item.Total)) %>% 
    top_n(3, Total_Sales) %>% 
    arrange(desc(Total_Sales))
  
  ggplotly(ggplot(top3_products, aes(x = Product.Name, y = Total_Sales)) +
             geom_bar(stat = "identity", fill = "plum", width = 0.8) +
             labs(title = "Top 3 Products by Sales", x = "Product Name", y = "Total Sales") +
             theme(axis.text.x =  element_text(angle = 90, hjust = 0.2, size = 0.8))) %>% 
    layout(yaxis = list(tickformat = ",.0f"))
}



# Define a function to get top countries and plot the graph
create_top_country <- function(data) {
  df_sales_country <- data %>%
    group_by(`Order.Country`) %>%
    summarize(`Sales of Orders` = sum(Sales)) %>%
    arrange(desc(`Sales of Orders`)) %>%
    head(10)
  
  ggplot(df_sales_country, aes(x = `Sales of Orders`, y = `Order.Country`, fill = `Sales of Orders`)) +
    geom_col() +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    labs(title = "Top 10 Countries by Sales", x = "Sales of Orders", y = "Country") +
    theme(plot.title = element_text(hjust = 0.5), 
          axis.text.x = element_text(angle = 45, hjust = 1))
}


# Define a function to get top customers and plot the graph
create_top_customers  <- function(data) {
  data$Customer_ID_STR <- as.character(data$Customer.Id)
  data_customers_profit <- aggregate(data$Order.Profit.Per.Order, 
                                     by = list(data$Customer_ID_STR), 
                                     FUN = sum, 
                                     na.rm = TRUE)
  
  colnames(data_customers_profit) <- c("Customer_ID_STR", "Profit_of_Orders")
  
  top_customers <- head(data_customers_profit[order(-data_customers_profit$Profit_of_Orders), ], 20)
  
  
  plot_ly(top_customers, x = ~Profit_of_Orders, y = ~Customer_ID_STR, type = "bar", 
          marker = list(color = ~Profit_of_Orders, colorscale = "Viridis"), 
          hovertemplate = paste("Customer ID: %{y}<br>", "Profit: $%{x:.2f}")) %>%
    layout(title = " Top 20 Most Profitable Customers")
  
}


library(shinythemes)

# Define UI
ui <- navbarPage("Supply Chain Dataco Analysis Dashboard",
                 # Introduction page
                 tabPanel("Introduction",
                          fluidPage(theme = shinythemes::shinytheme("cerulean"),
                                    br(),
                                    h2("Welcome to the Supply Chain Dataco Analysis Dashboard!"),
                                    br(),
                                    h3("Data Description"),
                                    shiny::p("The Dataco Supply Chain data comprises a rich collection of features 
                                              that provide a comprehensive view of the supply chain operations of the company. 
                                              The dataset includes 53 features that cover various aspects of customers, orders,
                                              products, and locations. 
                                              Some of the key features of the dataset include Delivery status, late delivery 
                                              risk, category name, order country, order city, order date, shipping mode, 
                                              shipping date, order status, product name, customer details, product price, 
                                              and product status. The Delivery status feature indicates whether the delivery was
                                              successful or not. The late delivery risk feature helps to identify the likelihood
                                              of a late delivery. The category name feature provides information on the type of 
                                              product ordered, while the order country and order city features provide 
                                              information on the geographic location of the order. The order date feature 
                                              captures the date the order was placed, while the shipping mode and shipping date 
                                              features capture information on the mode and date of shipping. The order status 
                                              feature indicates whether the order has been fulfilled or not. The product name 
                                              feature captures the name of the product ordered, while the customer details 
                                              feature provides information on the customer who placed the order. The product 
                                              price feature captures the price of the product, while the product status feature 
                                              indicates whether the product is available or not."),
                                    br(),
                                    h3("Dashboard Features"),
                                    shiny::p("This dashboard provides an interactive way to analyze the sales, 
                                             late deliveries, and fraud data of the supply chain. It includes various 
                                             charts and maps to visualize the data and gain insights."),
                                    br(),
                                    h3("Instructions"),
                                    shiny::p("Use the tabs on the top to navigate between the different analysis pages. 
                                             Select a chart from the dropdown menu on each page to view the corresponding 
                                             visualization."),
                                    br(),
                                    h3("Exploratory Data Analysis"),
                                    shiny::p("As part of the exploratory data analysis, a variety of visualization techniques
                                             were used to gain insights from the data. Histograms and density plots were used to 
                                             visualize continuous variables, while bar plots and pie charts were used for categorical
                                             variables.The goal of an EDA on delivery and fraud data is to gain a deep understanding of 
                                             the data that causes late deliveries and identify potential fraud indicators that can be 
                                             used to detect fraudulent activity")
                      )
             ),
             # Sales analysis page
             tabPanel("Sales Analysis",
                      sidebarPanel(
                        h4("Select a chart to display:"),
                        selectInput(inputId = "chartType", label = NULL,
                                    choices = c("Customer Segments", "Top Products", "Top Countries", "Top Customers"))
                      ),
                      mainPanel(
                        plotlyOutput("chart")
                      ),
                      sidebarPanel(
                        h4("Sales Analysis Summary"),
                        shiny::p("Revenue is a key driver of profitability. By improving sales, 
                                 dataco can increase its profits and ensure its long-term financial 
                                 stability. This can be achieved by identifying areas where revenue can 
                                 be increased and implementing strategies to optimize revenue generation
                                 and improve customer satisfaction.")
                      )
             ),
             # Delivery analysis page
             tabPanel("Delivery Analysis",
                      sidebarPanel(
                        h4("Select a chart to display:"),
                        selectInput(inputId = "DeliveryChartType", label = NULL,
                                    choices = c("Delivery Market", "Delivery Department", "Delivery Shipping", "Delivery Location", "Delivery Delay"))
                      ),
                      mainPanel(
                        plotlyOutput("DeliveryChart"),
                        div(style="margin-top: 20px", leafletOutput("map"))
                      ),
                      sidebarPanel(
                        h4("Late Delivery Risk Analysis Summary"),
                        shiny::p("Ensuring customer satisfaction is of paramount importance to retailers.
                                  Through this analysis, businesses can potentially save significant 
                                  sums by anticipating delayed deliveries and taking proactive measures 
                                  to maintain high levels of customer satisfaction.")
                      )
             ),
             # Fraud analysis page
             tabPanel("Fraud Analysis",
                      sidebarPanel(
                        h4("Select a chart to display:"),
                        selectInput(inputId = "fraudChartType", label = NULL,
                                    choices = c("Fraud Order Status" , "Fraud vs valid"))
                      ),
                      mainPanel(
                        plotlyOutput("fraudChart")
                      ),
                      sidebarPanel(
                        h4("Fraud Analysis Summary"),
                        shiny::p("Fraud analysis can assist in the implementation of effective controls 
                                 and mitigation strategies to prevent future fraudulent activities, as 
                                 well as in improving the overall efficiency and transparency of the 
                                 supply chain. Ultimately, the goal of fraud analysis is to safeguard 
                                 the company's assets and ensure its long-term financial stability.")
                      )
             )
  )



# Define the server logic
server <- function(input, output) {
  
  output$chart <- renderPlotly({
    if (input$chartType == "Customer Segments") {
      create_customer_segment(supply_chain)
    } else if (input$chartType == "Top Products") {
      create_top_products(supply_chain)
    } else if (input$chartType == "Top Countries"){
      create_top_country(supply_chain)
    } else if (input$chartType == "Top Customers"){
      create_top_customers(supply_chain)
    }
  })
  
  output$fraudChart <- renderPlotly({
    if (input$fraudChartType == "Fraud Order Status") {
      Fraud_order_status(supply_chain)
    } else {
      Fraud_valid(supply_chain_model)
    }
  })
  
  output$DeliveryChart <- renderPlotly({
    if (input$DeliveryChartType == "Delivery Market") {
      Delivery_market(supply_chain)
    } else if (input$DeliveryChartType == "Delivery Department") {
      Delivery_department(supply_chain)
    } else if (input$DeliveryChartType == "Delivery Shipping") {
      Delivery_shipping(supply_chain)
    } else if (input$DeliveryChartType == "Delivery Delay") {
      Delivery_delay(supply_chain)
    }
  })
  
  output$map <- renderLeaflet({
    if (input$DeliveryChartType == "Delivery Location") {
      Delivery_location(supply_chain)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
