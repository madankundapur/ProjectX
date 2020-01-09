#=============================================================
# Customer RFM Analysis, Segmentation and Attrition Prediction
#============================================================

# Load required libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")


# Setting digits 
options(digits=3)

# Load orders data from csv file
# Source of this file is Tableau Community forums
orders <- read_csv("SuperstoreOrders.csv")

#=======================================================
# Explore, Clean & Tidy Data
#=======================================================

str(orders)

# Remove spaces from column names
names(orders)<-str_replace_all(names(orders), c(" " = ""))


# Order Date is a character variable - Convert to date
orders$OrderDate <- as.Date(orders$OrderDate, "%m/%d/%Y")
class(orders$OrderDate)
summary(orders$OrderDate)


# Check for duplicates
duplicates <- which(duplicated(orders))
duplicates

# No duplicates exist in data
rm(duplicates)

# Invoice data is repeated because it contains product level details
# we dont need data at this level
# aggregate and select necessary data
orders <- orders %>%
  group_by(CustomerID, OrderID , OrderDate) %>%
  summarize(Sales = sum(Sales)) %>%
  select(CustomerID, OrderID , OrderDate, Sales)

# Checking if the orders are equal to the observations in the dataset
length(unique(orders$OrderID))
nrow(orders)

# Check range of order dates
range(orders$OrderDate)

# Compute max date + 1 from  the dataset to help compute  days since purchase
max_date <- max(orders$OrderDate)+1

# Compute year and days since purchase 
orders <- orders %>% 
  mutate(PurchaseYear = as.numeric(format(OrderDate, "%Y")),
         DaysSincePurchase = as.numeric(difftime(max_date, OrderDate,"days")))
rm(max_date)


#=======================================================
# Compute Recency, Frequency, Monetary Value
#=======================================================
# Recency is duration in days since the last purchase
# Frequency is number of distinct orders by customer
# Monetary value is total order amount for the customer
customers <- orders %>%
  group_by(CustomerID) %>%
  summarise(DaysSinceFirstPurchase = max(DaysSincePurchase),
            Recency = min(DaysSincePurchase),
            Frequency = n_distinct(OrderID), 
            Monetary = sum(Sales))

summary(customers)


# plot Recency
customers %>% ggplot(aes(Recency))  + 
  geom_histogram(bins=20,fill = "darkred") +
  labs(x = "Recency", y = "Count", title = "Recency Distribution")

# plot Frequency
customers %>% ggplot(aes(Frequency))  + 
  geom_histogram(bins=10,fill = "steelblue")+
  labs(x = "Frequency", y = "Count", title = "Frequency Distribution")

# plot Monetary value
customers %>% ggplot(aes(Monetary))  + 
  geom_histogram(bins=20,fill = "green4") +
  labs(x = "Monetary Value", y = "Count", title = "Monetary Value Distribution")


# since the scale of values are very different for Recency, Frequency and Monetary  
# remove the positive skew and then standardise them
customers$RecencyZ <- scale(log(customers$Recency), center=TRUE, scale=TRUE)
customers$FrequencyZ <- scale(log(customers$Frequency), center=TRUE, scale=TRUE)
customers$MonetaryZ <- scale(log(customers$Monetary), center=TRUE, scale=TRUE)


#=======================================================
# Initial Analysis of RFM through simple scoring
#=======================================================

# Adding RFM Scoring for initial analysis
# Using a scale of 1 to 3 assign a score to Recency, Frequency & Monetary value
customers <- customers %>%
  mutate(RScore = ntile(desc(Recency),3), 
          FScore = ntile(Frequency,3), 
          MScore = ntile(Monetary,3),
          RFMScore = round((RScore+FScore+MScore)/3,0),
          RFMScoreLabel = case_when(RFMScore == 1 ~ "1-Low", RFMScore == 2 ~ "2-Medium", RFMScore == 3 ~ "3-High"))

table(customers$RFMScore)

# see how the plot looks for RFM scores
customers %>% ggplot(aes(RFMScoreLabel)) +
  geom_bar(fill = "steelblue") +
  labs(x = "RFM Score", y = "Count", title = "RFM Score") 


#=======================================================
# Clutering using k-means
#=======================================================

# see how the scatter plot looks for standardised RFM values
customers %>% ggplot(aes(x=FrequencyZ,y=MonetaryZ)) +
  geom_point(aes(colour = RecencyZ)) +
  scale_colour_gradient(name="Recency") +
  labs(x = "Frequency", y = "Monetary Value", title = "Scatter plot - Recency vs Frequency vs Monetary Value")


#assuming 10 as the maximum number of cluster centers
j<- 10

# data frame to hold cluster components
ss <- data.frame(K=integer(),
                 TWSS=numeric())

# ensure customers dataset is a data frame
customers <- as.data.frame(customers)

# loop to create upto 10 clusters 
for (i in 1:j ) {
  
  set.seed(1, sample.kind="Rounding")
  
  # K-Means is an algorithm that takes in a dataset and a constant
  # k and returns k centroids (which define clusters of data in the
  # dataset which are similar to one another).
  # Run k-means with i centers, assume nstart =25
  km <- kmeans(customers[,c(5:7)], centers = i, nstart = 25)
  
  # Adding cluster data  to customers dataset for each i in different variables
  col_nm <- paste("C", i, sep="")
  customers[,(col_nm)] <- factor(km$cluster, levels = c(1:i))

  # Find medians for RFM grouped by cluster and print them
  med <- customers %>% 
      group_by(Cluster = customers[,(col_nm)]) %>% 
      summarize(Recency=round(median(Recency), 0),
              Frequency=round(median(Frequency),1),
              Monetary=round(median(Monetary),2))
  print(paste(i, "Clusters", sep=" "))
  print(med)
  
  # store cluster info
  ss[i,("K")] <- i 
  ss[i,("TWSS")] <- km$tot.withinss
}


# Plot sum within sum of squares
ss %>% ggplot(aes(x = K, y = TWSS)) +
  geom_point() +
  geom_line()+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(breaks = 1:j)+
  xlab("Clusters")+
  ylab("Total within Sum of Squares")


# color palette for the scatter plot
palette <- c('darkred','steelblue','green4','orange', "cyan")

# Plot RFM flor cluster 2
p1 <- customers %>% ggplot( aes(x = FrequencyZ, y = MonetaryZ))+
  geom_point(aes(colour = C2))+
  scale_colour_manual(name = "Cluster", values=palette)+
  xlab("Frequency")+
  ylab("Monetary Value")+
  ggtitle(paste("2 Cluster Plot", sep=" "))

# Plot RFM flor cluster 3
p2<- customers %>% ggplot( aes(x = FrequencyZ, y = MonetaryZ))+
  geom_point(aes(colour = C3))+
  scale_colour_manual(name = "Cluster", values=palette)+
  xlab("Frequency")+
  ylab("Monetary Value")+
  ggtitle(paste("3 Cluster Plot", sep=" "))

# Plot RFM flor cluster 4
p3<- customers %>% ggplot( aes(x = FrequencyZ, y = MonetaryZ))+
  geom_point(aes(colour = C4))+
  scale_colour_manual(name = "Cluster", values=palette)+
  xlab("Frequency")+
  ylab("Monetary Value")+
  ggtitle(paste("4 Cluster Plot", sep=" "))

# Plot RFM flor cluster 5
p4<- customers %>% ggplot( aes(x = FrequencyZ, y = MonetaryZ))+
  geom_point(aes(colour = C5))+
  scale_colour_manual(name = "Cluster", values=palette)+
  xlab("Frequency")+
  ylab("Monetary Value")+
  ggtitle(paste("5 Cluster Plot", sep=" "))

# Arrange plots
grid.arrange(p1, p2, p3, p4, ncol=2, nrow = 2)

rm(ss, med, col_nm, i, j, km, palette, p1, p2, p3, p4)


#=======================================================
# Yearly orders initial analysis
#=======================================================

# Summarize total customers, orders & sales by year
yearly_orders <- orders %>%
  group_by(PurchaseYear) %>%
  summarise(Customers = n_distinct(CustomerID),
            Orders = n_distinct(OrderID), 
            Sales = sum(Sales))

knitr::kable(yearly_orders)

# Plot Number of Customers by year 
yearly_orders %>% ggplot(aes(x = PurchaseYear, y = Customers)) +
  geom_bar(stat = "identity", fill = 'steelblue') +
  labs(x = "Year", y = "No. of Customers", title = "Yearly Customers") 

# Plot Orders by year 
yearly_orders %>% ggplot(aes(x = PurchaseYear, y = Orders)) +
  geom_bar(stat = "identity", fill = 'steelblue') +
  labs(x = "Year", y = "Orders", title = "Yearly Orders")
  
# Plot Total Sales by year
yearly_orders %>% ggplot(aes(x = PurchaseYear, y = Sales)) +
  geom_bar(stat = "identity", fill = 'steelblue') +
  labs(x = "Year", y = "Sales", title = "Yearly Sales")

rm(yearly_orders)

#=======================================================
# Prepare data for customer attrition prediction
#=======================================================

# Compute orders by year and arrange in a cross tab  
customer_orders_by_year <- orders %>%
       group_by(CustomerID, PurchaseYear) %>%
       summarize(count = n()) %>%
       spread(PurchaseYear, count)

head(customer_orders_by_year)

# updates NA's with 0
customer_orders_by_year[is.na(customer_orders_by_year)] = 0

# change names of years (numbers) to Y+ year
names(customer_orders_by_year)<-str_replace_all(names(customer_orders_by_year), c("2" = "Y2"))

# Update customer status -  Attrited or Retained
# if there are no orders in the later  years means customer has attrited
# note that there are customers who may not have any orders in between but have returned
customer_orders_by_year<- customer_orders_by_year %>% 
                          mutate(Status = case_when(Y2018 == 0 ~ "Attrited", 
                                                    Y2018 == 0 &  Y2017 == 0 ~ "Attrited", 
                                                    Y2018 == 0 &  Y2017 == 0 & Y2016 == 0 ~ "Attrited",
                                 TRUE ~ "Retained"))

# Merge Customer Status with customers dataset
customers <- merge(customers, customer_orders_by_year,  by="CustomerID", all=TRUE, sort=TRUE) 

rm(customer_orders_by_year)



# Select required variables for predicting status
# select data for last year and prior for training and testing
customer_status <- customers %>% 
  select(CustomerID, Recency, Frequency, Monetary, Status)

# Update Status class to factor
customer_status$Status <- factor(customer_status$Status)

# Update customer id as row names and remove customer id variable
row.names(customer_status) <- customer_status$CustomerID
customer_status <- select(customer_status, -CustomerID)

# Status variable distribution
table(customer_status$Status)


#=======================================================
# Using different models for prediction
#=======================================================

# Create training data and test data 
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(customer_status$Status, times = 1, p = 0.2, list = FALSE)
train_set <- customer_status[-test_index,]
test_set <- customer_status[test_index,]

# Generalized linear model - ignore warnings
train_glm <- train(Status ~ ., data = train_set, method = "glm")
glm_preds <- predict(train_glm, test_set)
mean(glm_preds == test_set$Status)

# Linear discriminant analysis
train_lda <- train(Status ~ ., data = train_set, method = "lda")
lda_preds <- predict(train_lda, test_set)
mean(lda_preds == test_set$Status)

# Quadratic discriminant analysis
train_qda <- train(Status ~ ., data = train_set, method = "qda")
qda_preds <- predict(train_qda, test_set)
mean(qda_preds == test_set$Status)

# K Nearest Neighbour
train_knn <- train(Status ~ ., data = train_set, method = "knn", tuneLength=20)
knn_preds <- predict(train_knn, test_set)
mean(knn_preds == test_set$Status)

# Random Forest
train_rf <- train(Status ~ ., data = train_set, method = "rf", tuneLength=20, importance = TRUE)
rf_preds <- predict(train_rf, test_set)
mean(rf_preds == test_set$Status)

# Ensemble
ensemble <- cbind(glm = glm_preds == test_set$Status, 
                  lda = lda_preds == test_set$Status, 
                  qda = qda_preds == test_set$Status, 
                  knn = knn_preds == test_set$Status, 
                  rf = rf_preds == test_set$Status)
ensemble_preds <- ifelse(rowMeans(ensemble) > 0.5, "Retained", "Attrited")
mean(ensemble_preds == test_set$Status)

# listing model results
model_results <- resamples(list(glm = train_glm,
                                lda = train_lda,
                                qda = train_qda,
                                knn = train_knn,
                                rf = train_rf))
summary(model_results)

# Summarising accuracy from various models
models <- c("Logistic regression", "Linear discriminant analysis", "Quadratic discriminant analysis", "K nearest neighbors", "Random forest", "Ensemble")

accuracy <- c(mean(glm_preds == test_set$Status),
              mean(lda_preds == test_set$Status),
              mean(qda_preds == test_set$Status),
              mean(knn_preds == test_set$Status),
              mean(rf_preds == test_set$Status),
              mean(ensemble_preds == test_set$Status)) 
acc_model<- data.frame(Model = models, Accuracy = accuracy)
print(acc_model)

# Acccuracies plot
acc_model %>% ggplot(aes(Accuracy, Model, group=1)) +
  geom_point(color="orange") + 
  geom_line(color="steelblue") +
  labs(x = "Accuracy", y = "Model", title = "Accuracies of different models")
       

# RF method has the highest accuracy
confusionMatrix(data = rf_preds, reference = test_set$Status)

# get customer observations from test set
customers_in_test_set <- customers %>%
  filter(CustomerID %in% row.names(test_set))

# using RF results to compute probability of attrition
rf_prob <- predict(train_rf, customers_in_test_set, type = "prob")

# Compute probabilities
customers_in_test_set$AttritionProbability <- rf_prob$Attrited

rm(test_index, train_set, test_set, train_glm, train_knn, train_lda, train_qda, train_rf, ensemble, customer_status)
rm(glm_preds, lda_preds, qda_preds, rf_preds, knn_preds, ensemble_preds, models, accuracy, acc_model, model_results, rf_prob)

#=======================================================
# Final Analysis for Insights
#=======================================================

# Inspect RFM Score, Cluster 3 and Status for Attrition Probability >= .9 
res<- customers_in_test_set %>% filter(AttritionProbability>= .9) %>%
  select(CustomerID, RFMScore, C3, Status, AttritionProbability)

table(res$Status)
table(res$RFMScore)
table(res$C3)

# inspect attrition probability vs status 
table(customers_in_test_set$AttritionProbability, customers_in_test_set$Status)

rm(res)

# End