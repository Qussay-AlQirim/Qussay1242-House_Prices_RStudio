#--------------------------
# Loading Libraries
#--------------------------

library(tidyverse)
library(dplyr)
library(corrplot)
library(summarytools)
library(reshape2)
library(caTools)
library(Hmisc)
library(Metrics)


#--------------------------
# Importing the CSV File
#--------------------------
  
data <- read.csv("house_price_train.csv")



#---------------------------------------------------
# Exploratory Data Analysis (Descriptive Statistics)
#---------------------------------------------------
  

      # Analysing Data for missing values
    
      table(is.na(data))
      
      number_missing <- sapply(data, function(x) sum(is.na(x)))
      num_missing <- data.frame(number_missing)
      
      Missing <- sapply(data, function(x) (((sum(is.na(x)))/(nrow(data)))*100))
      
      Missing_df <- data.frame(Missing)
      
      
      
      # Analysing Structure of Dataset
      
      str(data)
      
      # Analysing for Duplicate House ID's (There are no duplicate values in dataset)
      
      length(unique(data[,"Id"]))
      
      # Visualizing Data
      
      ggplot(data, mapping = aes(x= GrLivArea)) +
        geom_boxplot() +
        labs(title = "Above Ground Living Area") +
        xlab("Above Ground Living Area Square Feet")
      
      ggplot(data, mapping = aes(x= YearBuilt)) +
        geom_histogram(color = 'white', fill = 'red') +
        labs(title = "Distribution of Number of Houses built per Year") +
        xlab("Original Construction Date") +
        ylab("Number of Houses")
      
      ggplot(data, mapping = aes(x= OverallQual)) +
        geom_histogram(color = 'white', fill = 'blue') +
        labs(title = "Distribution of Overall Quality amongst Houses") +
        ylab("Number of Houses") +
        xlab("Overall Material & Finish Quality")
      
      ggplot(data, mapping = aes(x= GarageCars)) +
        geom_histogram(color = 'darkblue', fill = 'lightblue') +
        labs(title = "Garage Capacity") +
        ylab("Number of Houses") +
        xlab("Size of Garage in Car Capacity")
      
      ggplot(data, mapping = aes(x= TotalBsmtSF)) +
        geom_boxplot() +
        labs(title = "Total Square feet of Basement Area for each House") +
        xlab("Total Square feet of Basement Area")
        
      ggplot(data, mapping = aes(x= YearRemodAdd)) +
        geom_histogram(color = 'black', fill = 'yellow') +
        labs(title = "Distribution of Number of Houses Remodeled per Year") +
        ylab("Number of Houses") +
        xlab("Remodel Date")


      
#--------------------------
# Correlation Test
#--------------------------
        
        
        
      # Filtering Table to include numeric data only
      
      num_data <- data %>% dplyr::select(where(is.numeric))
      
      view(dfSummary(num_data))


      # Dropping categorical variables identified as numeric

      num_only = subset(num_data, 
                        select = -c(Id, MSSubClass, LotFrontage, MasVnrArea, 
                                    GarageYrBlt) )
      
      
     
      # Correlation test on num_only dataframe
      
      correlation <- round(cor(num_only), 2)
      correlation
      

      corr_p_value <- rcorr(as.matrix(num_only))
      corr_p_value
    
       # Converting the correlation and p-value to a dataframe
      
      Correlation_df <- data.frame(correlation)

      
       # Selecting the SalePrice Column to perform Correlation Matrix Heatmap
      
      matrix <- Correlation_df["SalePrice"]
      matrix <- cbind(rownames(matrix), matrix)
      rownames(matrix) <- NULL
      colnames(matrix) <- c("Variables","SalePrice")
      
      melt(matrix)
      final_matrix <- melt(matrix)
      colnames(final_matrix) <- c("Variables","SalePrice", "Value")
      
      corr_matrix <- final_matrix %>% arrange(desc(Value))
      corr_matrix
      
      
      
      # Creating a Correlation Table with the Variables of interest only
      
      focus_matrix <- filter(corr_matrix, Variables %in% c('YearBuilt', 'GrLivArea', 
                             'GarageCars', 'TotalBsmtSF', 'YearRemodAdd','OverallQual'))
        
      
      
        
      # Visualizing the Correlation Matrix

      ggplot(data = corr_matrix, aes(SalePrice, Variables, fill = Value))+
        geom_tile(color = "white")+
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                             midpoint = 0, limit = c(-1,1), space = "Lab", 
                             name="Pearson\nCorrelation") +
        theme_minimal()+ 
        theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                         size = 12, hjust = 1))+
        coord_fixed()
        
        

#--------------------------
#Splitting Data
#--------------------------

set.seed(2011)   
split = sample.split(data, SplitRatio = 0.75)
HouseTrain = subset(data, split == TRUE)
HouseTest  = subset(data, split == FALSE)



#--------------------------
# Linear Regression Model
#--------------------------


lm3 <- lm(SalePrice ~
            YearBuilt + GrLivArea + GarageCars + TotalBsmtSF +YearRemodAdd + 
            OverallQual,
          data = HouseTrain)
summary(lm3)
plot(lm3)


#--------------------------
# Predictions
#--------------------------

# Predicting Data using the Training Sample

predictTrain = predict(lm3, type="response")

rmse( HouseTrain$SalePrice, predictTrain)



# Predicting Data using the Testing Sample

predictTest = predict(lm3, type = "response", newdata = HouseTest)
predictTest

rmse( HouseTest$SalePrice, predictTest)


# Comapring the predicted sales data from the test data with actual value
# of sales data from the test data. Then using percentage difference to 
# calculate how off are our predictions.

compare_test <- data.frame(HouseTest['SalePrice'], predictTest)

compare_test$percentage_difference <- 
  (((predictTest - HouseTest['SalePrice'])/HouseTest['SalePrice'])*100)

compare_test


# Determine the number of predictions that are only 10 percent above or below
# the actual saleprice value in the Test Data

filtered_test <- compare_test %>% filter(percentage_difference < 10
                                         & percentage_difference > -10)
