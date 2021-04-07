#===============================================================================
# Course:       BC2407 ANALYTICS II: ADVANCED PREDICTIVE TECHNIQUES
# Component:    Group Project
# Authors:      Chew Wen Qiang, Jeremy (U1911921H)
#               Kenneth Chia (U1910780H)
#               Lim Zheng Yi, Dennis (U1910514C)
#               Tan Jun Hong, (U1920048B)
#               Wang Wayne (U1920270G)
# Data Source:  "allegations.csv",
# Packages:     data.table, stringr, corrplot, ggplot2, caTools, DMwR, car, rpart, rpart.plot, earth, caret
# Version:      2.0
# Updated:      27-Mar-2021
#===============================================================================

# Import required packages
library(package = data.table)
library(package = stringr)
library(package = corrplot)
library(package = ggplot2)
library(package = caTools)
library(package = DMwR)
library(package = car)
library(package = earth)
library(package = caret)
library(package = rpart)
library(package = rpart.plot)
library(package = randomForest)

# Set seed to stabilize results
set.seed(seed = 2407)

# Set threshold to be used in logistic regression and MARS
threshold <- 0.5

# Read in CSV file and convert columns with string to categorical
police_data <- fread(input = 'allegations.csv', stringsAsFactors = TRUE)


#===============================================================================
# Start of Data Cleaning
#===============================================================================

# Convert categorical data values to upper case
police_data <- as.data.table(x = lapply(X = police_data, FUN = function(x) if (is.factor(x = x)) factor(x = toupper(x = x)) else(x)))

# Check for NA values in the dataset
sum(is.na(x = police_data))

# Taken from https://stackoverflow.com/questions/50079695/replace-missing-with-mode-for-factor-column-and-mean-for-numeric-column-in-r
# Create function to get statistical mode
# Also, replace categorical NA with mode, numerical values with median
get_mode <- function(x) {
  unique_values <- unique(x = x)
  unique_values[which.max(x = tabulate(bin = match(x = x, table = unique_values)))]
}

police_data[-1] <- lapply(X = police_data[-1], function(x) {
  if (is.factor(x = x)) replace(x = x, list = is.na(x), values = get_mode(na.omit(object = x)))
  else if (is.numeric(x = x)) replace(x = x, list = is.na(x), values = median(x = x, na.rm = TRUE))
  else x
})

# Make a new column called misconduct
# If unsubstantiated or exonerated, row value is 'NO'
# Else, row value is 'YES'
police_data[, misconduct := ifelse(test = board_disposition == 'UNSUBSTANTIATED' | board_disposition == 'EXONERATED', yes = "NO", no = "YES")]
police_data$misconduct <- factor(x = police_data$misconduct)

# Make a new column called remedial_action
# If substantiated, copy the actions taken to the new column, removing brackets and 'SUBSTANTIATED' for row value
# If unsubstantiated, row value is 'NONE'
# Then, convert the column to categorical

police_data$remedial_action <- as.factor(x = sapply(X = police_data$board_disposition, function(x) {
  if (x == 'UNSUBSTANTIATED' | x == 'EXONERATED') 'NONE'
  else gsub(pattern = '[()]', replacement = '', x = str_split(string = x, pattern = ' ', n = 2)[[1]][2])
}))


# Create another column called repeated_complaints
# If officer has multiple complaints, row value = 'YES'
# Else, row value = 'NO'
police_data[, repeated_complaints := ifelse(test = .N > 1, yes = 'YES', no = 'NO'), by = unique_mos_id]
police_data$repeated_complaints <- factor(x = police_data$repeated_complaints)

# Check for erroneous data
summary(object = police_data$complainant_age_incident)
police_data[complainant_age_incident <= 3]
# According to https://www1.nyc.gov/site/ccrb/complaints/complaints.page
# You can file a complaint regardless of your age
# Set threshold of age = 3 where a child below the age of 3 is assumed to be erroneous data
# Replace records where age <= 3 with median age
police_data[complainant_age_incident <= 3, complainant_age_incident := median(police_data$complainant_age_incident)]

# Merge rows with complainant ethnicity = blank, other race and refused all into unknown
police_data[complainant_ethnicity == "" |
              complainant_ethnicity == "OTHER RACE" |
              complainant_ethnicity == "REFUSED"]$complainant_ethnicity <- "UNKNOWN"
police_data$complainant_ethnicity <- factor(x = police_data$complainant_ethnicity)

# Merge complainant gender = transman to male, transwoman to female, and gender non-conforming, blank, not described to unknown
police_data[complainant_gender == "TRANSMAN (FTM)"]$complainant_gender <- "MALE"
police_data[complainant_gender == "TRANSWOMAN (MTF)"]$complainant_gender <- "FEMALE"
police_data[complainant_gender == "GENDER NON-CONFORMING" |
              complainant_gender == "" |
              complainant_gender == "NOT DESCRIBED"]$complainant_gender <- "UNKNOWN"
police_data$complainant_gender <- factor(x = police_data$complainant_gender)

# Merge outcome description into the respective top 5 categories
police_data[outcome_description == ""]$outcome_description <- "UNKNOWN"
police_data[outcome_description == "NO ARREST MADE OR SUMMONS ISSUED"]$outcome_description <- "NONE"
police_data[str_detect(string = outcome_description, pattern = "ARREST")]$outcome_description <- "ARREST"
police_data[str_detect(string = outcome_description, pattern = "SUMMONS")]$outcome_description <- "SUMMONS"
police_data$outcome_description <- factor(x = police_data$outcome_description)

# Merge command at incident into the respective top 5 categories
police_data[str_detect(string = command_at_incident, pattern = "PCT")]$command_at_incident <- "PCT"
police_data[str_detect(string = command_at_incident, pattern = "NAR")]$command_at_incident <- "NAR"
police_data[str_detect(string = command_at_incident, pattern = "PB")]$command_at_incident <- "PB"
police_data[str_detect(string = command_at_incident, pattern = "PSA")]$command_at_incident <- "PSA"
police_data[command_at_incident != "PCT" &
              command_at_incident != "NAR" &
              command_at_incident != "PB" &
              command_at_incident != "PSA"]$command_at_incident <- "OTHER"
police_data$command_at_incident <- factor(x = police_data$command_at_incident)

# Merge command now into the respective top 5 categories
police_data[str_detect(string = command_now, pattern = "PCT")]$command_now <- "PCT"
police_data[str_detect(string = command_now, pattern = "DET")]$command_now <- "DET"
police_data[str_detect(string = command_now, pattern = "NAR")]$command_now <- "NAR"
police_data[str_detect(string = command_now, pattern = "DB")]$command_now <- "DB"
police_data[command_now != "PCT" &
              command_now != "DET" &
              command_now != "NAR" &
              command_now != "DB"]$command_now <- "OTHER"
police_data$command_now <- factor(x = police_data$command_now)

# Merge contact reason into the respective top 5 categories
police_data[str_detect(string = contact_reason, pattern = "PD SUSPECTED C/V OF VIOLATION/CRIME")]$contact_reason <- "SUSPICION"
police_data[str_detect(string = contact_reason, pattern = "VIOLATION")]$contact_reason <- "VIOLATION"
police_data[str_detect(string = contact_reason, pattern = "REPORT")]$contact_reason <- "REPORT"
police_data[str_detect(string = contact_reason, pattern = "C/V")]$contact_reason <- "C/V"
police_data[contact_reason != "SUSPICION" &
              contact_reason != "VIOLATION" &
              contact_reason != "REPORT" &
              contact_reason != "C/V"]$contact_reason <- "OTHER"
police_data$contact_reason <- factor(x = police_data$contact_reason)

# Merge rank abbrev incident into the respective top 5 categories
police_data[str_detect(string = rank_abbrev_incident, pattern = "PO")]$rank_abbrev_incident <- "PO"
police_data[str_detect(string = rank_abbrev_incident, pattern = "DT")]$rank_abbrev_incident <- "DT"
police_data[rank_abbrev_incident == "SSA" | rank_abbrev_incident == "SDS"]$rank_abbrev_incident <- "SGT"
police_data[rank_abbrev_incident != "PO" &
              rank_abbrev_incident != "SGT" &
              rank_abbrev_incident != "DT" &
              rank_abbrev_incident != "LT"]$rank_abbrev_incident <- "OTHER"
police_data$rank_abbrev_incident <- factor(x = police_data$rank_abbrev_incident)

# Merge rank abbrev now into the respective top 5 categories
police_data[str_detect(string = rank_abbrev_now, pattern = "PO")]$rank_abbrev_now <- "PO"
police_data[str_detect(string = rank_abbrev_now, pattern = "DT")]$rank_abbrev_now <- "DT"
police_data[rank_abbrev_now == "SSA" | rank_abbrev_now == "SDS"]$rank_abbrev_now <- "SGT"
police_data[rank_abbrev_now != "PO" &
              rank_abbrev_now != "SGT" &
              rank_abbrev_now != "DT" &
              rank_abbrev_now != "LT"]$rank_abbrev_now <- "OTHER"
police_data$rank_abbrev_now <- factor(x = police_data$rank_abbrev_now)

#===============================================================================
# End of Data Cleaning
#===============================================================================


#===============================================================================
# Start of Preliminary Data Exploration and Visualization
#===============================================================================

#-------------------------------------------------------------------------------
# Plot a correlation plot of all variables 
#-------------------------------------------------------------------------------
corrplot(corr = cor(x = data.frame(lapply(X = police_data, function(x) as.numeric(x = x))), use = "complete.obs"),
         method = "ellipse",
         type = "lower")

#-------------------------------------------------------------------------------
# Barchart for command at incident (top 11 in terms of misconduct count)
#-------------------------------------------------------------------------------
barplot(height = head(x = sort(x = table(x = police_data[misconduct == "YES"]$command_at_incident), decreasing = TRUE), n = 11),
        main = "Misconduct by Command at Incident (Top 10)",
        xlab = "Command at incident",
        ylab = "Frequency",
        col = c("#f8766d", "#00bfc4"))

#-------------------------------------------------------------------------------
# Stacked Barchart for rank_abbrev_incident and misconduct
#-------------------------------------------------------------------------------
barplot(height = table(police_data$misconduct, police_data$rank_incident),
        main = "Misconduct by Rank during incident",
        xlab = "Rank during incident",
        ylab = "Frequency",
        col = c("#f8766d", "#00bfc4"))

legend(x = "topright",
       inset = c(0, 0),
       fill = c("#f8766d", "#00bfc4"),
       legend = rownames(x = table(police_data$misconduct, police_data$rank_incident)),
       border = "grey",
       cex = 1)
# A large proportion of police complaints are made against POM and SGT

#-------------------------------------------------------------------------------
# Stacked Barchart for complainant_ethnicity and misconduct
#-------------------------------------------------------------------------------
barplot(height = table(police_data$misconduct, police_data$complainant_ethnicity),
        main = "Misconduct by Complainant Ethnicity",
        xlab = "Complainant Ethnicity",
        ylab = "Frequency",
        col = c("#f8766d", "#00bfc4"))

legend(x = "topright",
       inset = c(0, 0),
       fill = c("#f8766d", "#00bfc4"),
       legend = rownames(x = table(police_data$misconduct, police_data$complainant_ethnicity)),
       border = "grey",
       cex = 1)
# A large proportion of complainants are black

#-------------------------------------------------------------------------------
# Stacked Barchart for mos_ethnicity and misconduct
#-------------------------------------------------------------------------------
barplot(height = table(police_data$misconduct, police_data$mos_ethnicity),
        main = "Misconduct by Police Ethnicity",
        xlab = "Police Ethnicity",
        ylab = "Frequency",
        col = c("#f8766d", "#00bfc4"))

legend(x = "topright",
       inset = c(0, 0),
       fill = c("#f8766d", "#00bfc4"),
       legend = rownames(x = table(police_data$misconduct, police_data$mos_ethnicity)),
       border = "grey",
       cex = 1)
# A large proportion of policemen who received complaints are white

#-------------------------------------------------------------------------------
# Stacked Barchart for complainant_gender and misconduct
#-------------------------------------------------------------------------------
barplot(height = table(police_data$misconduct, police_data$complainant_gender),
        main = "Misconduct by Complainant Gender",
        xlab = "Complainant Gender",
        ylab = "Frequency",
        col = c("#f8766d", "#00bfc4"))

legend(x = "topright",
       inset = c(0, 0),
       fill = c("#f8766d", "#00bfc4"),
       legend = rownames(x = table(police_data$misconduct, police_data$complainant_gender)),
       border = "grey",
       cex = 1)
# Male victims have a higher proportion of substantiated misconduct

#-------------------------------------------------------------------------------
# Stacked Barchart for mos_gender and misconduct
#-------------------------------------------------------------------------------
barplot(height = table(police_data$misconduct, police_data$mos_gender),
        main = "Misconduct by Police Gender",
        xlab = "Police Gender",
        ylab = "Frequency",
        col = c("#f8766d", "#00bfc4"))

legend(x = "topright",
       inset = c(0, 0),
       fill = c("#f8766d", "#00bfc4"),
       legend = rownames(x = table(police_data$misconduct, police_data$mos_gender)),
       border = "grey",
       cex = 1)
# Male police have a higher proportion of substantiated misconduct

#-------------------------------------------------------------------------------
# Boxplot of police age and misconduct
#-------------------------------------------------------------------------------
ggplot(data = police_data, aes(x = misconduct, y = mos_age_incident, fill = misconduct)) +
  geom_boxplot() +
  labs(x = "Misconduct",
       y = "Police age at incident",
       title = "Boxplot for Police age at incident and Misconduct") +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14))
# Younger police are more likely to perform police misconduct

#-------------------------------------------------------------------------------
# Boxplot of complainant age and misconduct
#-------------------------------------------------------------------------------
ggplot(data = police_data, aes(x = misconduct, y = complainant_age_incident, fill = misconduct)) +
  geom_boxplot() +
  labs(x = "Misconduct",
       y = "Complainant age at incident",
       title = "Boxplot for Complainant age at incident and Misconduct") +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14))

#===============================================================================
# End of Preliminary Data Exploration and Visualization
#===============================================================================


#===============================================================================
# Start of PART (A) - Analysis of significant variables (root causes)
#===============================================================================

#-------------------------------------------------------------------------------
# Start of Logistic Regression - PART (A)
#-------------------------------------------------------------------------------

# Initial Logistic Regression Model to check p-value
glm_analysis <- glm(formula = misconduct ~ rank_incident +
  mos_ethnicity +
  mos_gender +
  mos_age_incident +
  complainant_ethnicity +
  complainant_gender +
  complainant_age_incident +
  repeated_complaints +
  command_at_incident, family = binomial, data = police_data)
summary(object = glm_analysis)

# Calculate Odds Ratio and display it
options(scipen = 1)
glm_analysis_OR <- exp(x = coef(object = glm_analysis))
glm_analysis_OR

# Get Odds Ratio confidence interval and show it
glm_analysis_OR.CI <- exp(confint(glm_analysis))
round(glm_analysis_OR.CI, 3)

# Conduct backwards elimination and show results
glm_analysis_BE <- step(object = glm_analysis)
summary(object = glm_analysis_BE)

#-------------------------------------------------------------------------------
# End of Logistic Regression - PART (A)
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Start of MARS - PART (A)
#-------------------------------------------------------------------------------

# Do MARS first to check variable importance
mars_analysis <- earth(formula = misconduct ~ rank_incident +
  mos_ethnicity +
  mos_gender +
  mos_age_incident +
  complainant_ethnicity +
  complainant_gender +
  complainant_age_incident +
  repeated_complaints +
  command_at_incident, degree = 1, data = police_data, glm = list(family = binomial))
evimp(object = mars_analysis, trim = FALSE)

#-------------------------------------------------------------------------------
# End of MARS - PART (A)
#-------------------------------------------------------------------------------

#===============================================================================
# End of PART (A) - Analysis of significant variables (root causes)
#===============================================================================


#===============================================================================
# Start of PART (B) - Development of a prediction model
#===============================================================================


#===============================================================================
# Start of Train-Test Split
#===============================================================================

# Do a train test split, with 75% of the data as train set and the rest as test set
train_test_split <- sample.split(Y = police_data$misconduct, SplitRatio = 0.75)
police_train_set <- subset(x = police_data, subset = train_test_split == TRUE)
police_test_set <- subset(x = police_data, subset = train_test_split == FALSE)

# Do SMOTE to over-sample the minority data on the train set.
police_train_set_smote <- SMOTE(form = misconduct ~ ., data = police_train_set, perc.over = 100, k = 5, perc.under = 200)

#===============================================================================
# End of Train-Test Split
#===============================================================================


#===============================================================================
# Start of Logistic Regression
#===============================================================================

# Train the model
glm1 <- glm(formula = misconduct ~ mos_age_incident +
  mos_ethnicity +
  complainant_age_incident +
  complainant_gender +
  complainant_ethnicity +
  repeated_complaints, family = binomial, data = police_train_set_smote)

# Significant variables from initial logistic regression model:
summary(object = glm1)

# Check the Odds Ratio of variables to find trends
options(scipen = 1)
glm1.OR <- exp(x = coef(object = glm1))
glm1.OR

# Check for multicollinearity problem in our model.
vif(mod = glm1)
# For VIF > 5 or VIF > 10, we may conclude that there is multicollinearity.
# For our case, there is no multicollinearity problem between variables.

#-------------------------------------------------------------------------------
# Logistic Regression Model Accuracy
#-------------------------------------------------------------------------------

# Calculate the train set probabilities, and use it to predict the data
# Plot the confusion matrix as well, along with the accuracy and other coefficients
glm1_trainset_probabilities <- predict(object = glm1, type = "response", newdata = police_train_set)
glm1_trainset_predicted <- ifelse(test = glm1_trainset_probabilities > threshold, yes = "YES", no = "NO")
confusionMatrix(data = table(police_train_set$misconduct, glm1_trainset_predicted, deparse.level = 2), mode = "everything")

# Calculate the test set probabilities, and use it to predict the data
# Plot the confusion matrix as well, along with the accuracy and other coefficients
glm1_testset_probabilities <- predict(object = glm1, type = "response", newdata = police_test_set)
glm1_testset_predicted <- ifelse(test = glm1_testset_probabilities > threshold, yes = "YES", no = "NO")
confusionMatrix(data = table(police_test_set$misconduct, glm1_testset_predicted, deparse.level = 2), mode = "everything")

#===============================================================================
# End of Logistic Regression
#===============================================================================


#===============================================================================
# Start of CART
#===============================================================================

#-------------------------------------------------------------------------------
# CART - Variable Importance Analysis
#-------------------------------------------------------------------------------

# Use CART to find important variables, and display the results
cart_analysis <- rpart(formula = misconduct ~ rank_incident +
  mos_ethnicity +
  mos_gender +
  mos_age_incident +
  complainant_ethnicity +
  complainant_gender +
  complainant_age_incident +
  repeated_complaints +
  command_at_incident, method = "class", data = police_data, control = rpart.control(minsplit = 2, cp = 0))
cart_analysis$variable.importance
round(x = cart_analysis$variable.importance / sum(cart_analysis$variable.importance) * 100, digits = 4)

#-------------------------------------------------------------------------------
# CART - End of Variable Importance Analysis
#-------------------------------------------------------------------------------

# Use CART to create a predictive model
cart1 <- rpart(formula = misconduct ~ mos_age_incident +
  mos_ethnicity +
  complainant_age_incident +
  complainant_gender +
  complainant_ethnicity +
  repeated_complaints, method = "class", data = police_train_set_smote, control = rpart.control(minsplit = 2, cp = 0))

# Plotting the maximal tree
# rpart.plot(cart1, nn = T, main = "Maximal Tree")
printcp(x = cart1)
plotcp(x = cart1, main = "Subtrees")

# Compute min CVerror + 1SE in maximal tree cart
CVerror.cap <- cart1$cptable[which.min(cart1$cptable[, "xerror"]), "xerror"] + cart1$cptable[which.min(cart1$cptable[, "xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart1.
i <- 1; j <- 4
while (cart1$cptable[i, j] > CVerror.cap) {
  i <- i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if 
# optimal tree has at least one split. Where i = 3 in this case.
cp.opt <- ifelse(test = i > 1, yes = sqrt(x = cart1$cptable[i, 1] * cart1$cptable[i - 1, 1]), no = 1)

# Prune the max tree using the optimal cp = cp.opt
cart2 <- prune(tree = cart1, cp = cp.opt)
rpart.plot(x = cart2, nn = T, main = "Optimal Tree")

# Check the variable importance of all variables
round(x = cart2$variable.importance / sum(cart2$variable.importance) * 100)

# Show the splits of the tree after pruning
summary(object = cart2)

#-------------------------------------------------------------------------------
# CART Model Accuracy
#-------------------------------------------------------------------------------

# Predict the train set and show the confusion matrix of the data, along with the accuracy and other coefficients
cart2_trainset_predicted <- predict(object = cart2, type = "class", newdata = police_train_set)
confusionMatrix(data = table(police_train_set$misconduct, cart2_trainset_predicted, deparse.level = 2), mode = "everything")

# Predict the test set and show the confusion matrix of the data, along with the accuracy and other coefficients
cart2_testset_predicted <- predict(object = cart2, type = "class", newdata = police_test_set)
confusionMatrix(data = table(police_test_set$misconduct, cart2_testset_predicted, deparse.level = 2), mode = "everything")

#===============================================================================
# End of CART
#===============================================================================


#===============================================================================
# Start of MARS
#===============================================================================

# Create a MARS prediction model, using glm with family binomial as we have categorical Y
mars1 <- earth(formula = misconduct ~ mos_age_incident +
  mos_ethnicity +
  complainant_age_incident +
  complainant_gender +
  complainant_ethnicity +
  repeated_complaints, degree = 1, data = police_train_set_smote, glm = list(family = binomial))

# Show the hinges of the MARS model
summary(object = mars1)

#-------------------------------------------------------------------------------
# MARS Model Accuracy
#-------------------------------------------------------------------------------

# Predict and create confusion matrix for both train and test set along with the accuracy and other coefficients
mars_trainset_predicted <- predict(object = mars1, type = "class", newdata = police_train_set)
confusionMatrix(data = table(police_train_set$misconduct, mars_trainset_predicted, deparse.level = 2), mode = "everything")

mars_testset_predicted <- predict(object = mars1, type = "class", newdata = police_test_set)
confusionMatrix(data = table(police_test_set$misconduct, mars_testset_predicted, deparse.level = 2), mode = "everything")

#===============================================================================
# End of MARS
#===============================================================================


#===============================================================================
# Start of Random Forest
#===============================================================================

#-------------------------------------------------------------------------------
# Start of Random Forest Variable Importance Analysis
#-------------------------------------------------------------------------------

# Random forest using default settings to check variable importance
rf_analysis <- randomForest(formula = misconduct ~ rank_incident +
  mos_ethnicity +
  mos_gender +
  mos_age_incident +
  complainant_ethnicity +
  complainant_gender +
  complainant_age_incident +
  repeated_complaints +
  command_at_incident, data = police_data, importance = TRUE)

# Check the importance of the variables
importance(x = rf_analysis)

# Plot the variable importance, which uses mean decreasing accuracy
varImpPlot(x = rf_analysis, type = 1)

#-------------------------------------------------------------------------------
# End of Random Forest Variable Importance Analysis
#-------------------------------------------------------------------------------

# Random forest using default settings to generate model
rf1 <- randomForest(formula = misconduct ~ mos_age_incident +
  mos_ethnicity +
  complainant_age_incident +
  complainant_gender +
  complainant_ethnicity +
  repeated_complaints, data = police_train_set)

# Show the model and its confusion matrix
rf1

# Check variable importance
importance(x = rf1)
varImpPlot(x = rf1)

plot(x = rf1)

#-------------------------------------------------------------------------------
# Random Forest Model Accuracy
#-------------------------------------------------------------------------------

# Predict the model and show the confusion matrix along with the accuracy and other coefficients
rf_trainset_predicted <- predict(object = rf1, newdata = police_train_set)
confusionMatrix(data = table(police_train_set$misconduct, rf_trainset_predicted, deparse.level = 2), mode = "everything")

rf_testset_predicted <- predict(object = rf1, newdata = police_test_set)
confusionMatrix(data = table(police_test_set$misconduct, rf_testset_predicted, deparse.level = 2), mode = "everything")

#===============================================================================
# End of Random Forest
#===============================================================================

#===============================================================================
# End of PART (B) - Development of a prediction model
#===============================================================================