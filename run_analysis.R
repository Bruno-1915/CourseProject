library(dplyr)
library(stringr)
### Load all the data
## Train datasets
X_train_path <- 'UCI HAR Dataset/train/X_train.txt'
X_train_data <- read.table(X_train_path, sep = '')

train_subject_path <- 'UCI HAR Dataset/train/subject_train.txt'
train_subject_data <- read.table(train_subject_path, sep ='')

Y_train_path <- 'UCI HAR Dataset/train/y_train.txt'
Y_train_data <- read.table(Y_train_path, sep = '')


## Test datasets
X_test_path <- 'UCI HAR Dataset/test/X_test.txt'
X_test_data <- read.table(X_test_path, sep ='')

test_subject_path <- 'UCI HAR Dataset/test/subject_test.txt'
test_subject_data <- read.table(test_subject_path, sep ='')

Y_test_path <- 'UCI HAR Dataset/test/y_test.txt'
Y_test_data <- read.table(Y_test_path, sep = '')

### Finish loading data

### Names for the datasets
## X names
X_names_path <- 'UCI HAR Dataset/features.txt'
X_names <- read.table(X_names_path)
names(X_train_data) <- X_names[[2]]
names(X_test_data) <- X_names[[2]]
names(train_subject_data) <- 'subject'
names(test_subject_data) <- 'subject'

## Y name
names(Y_train_data) <- 'activity'
names(Y_test_data) <- 'activity'
### Finish naming 

### Bind subject labels to X data
X_train_data <- cbind(train_subject_data, X_train_data)
X_test_data <- cbind(test_subject_data, X_test_data)
### Finish binding subject to X data

### Bind all X data
X_data <- rbind(X_train_data, X_test_data)
###

### Bind all Y data
Y_data <- rbind(Y_train_data, Y_test_data)
###

### Bind all the data
data <- cbind(Y_data, X_data)
### Finish binding all the data

### Remove from environment all the data that wont be used
rm(list = c('X_data', 'Y_data', 'X_train_data', 'X_test_data', 'test_subject_data', 
     'train_subject_data', 'Y_test_data', 'Y_train_data', 'X_names'))
###

### Label the data with descriptive activity names
activity_labels_path <- 'UCI HAR Dataset/activity_labels.txt'
activity_labels <- read.table(activity_labels_path, sep ='')
data <- merge(activity_labels, data,
              by.x="V1",by.y="activity")
data <- data %>% select(-V1)
colnames(data)[1] <- 'activity'
### Finish labeling data

### Select only mean and std data
# Logical vector of mean measurements
log_mean <- grepl(pattern = 'mean', names(data))
# Logical vector of standard deviation measurements
log_std <- grepl(pattern = 'std', names(data))

# Logical vector with both mean or std measurements
# This vector will be used to select only mean and std variables
index <- log_mean | log_std
temp_data <- data %>% select(1,2)
data <- data[,index]
data <- cbind(temp_data, data)
### Finish selecting mean and std data

### Tidy data set with the average of each variable for each
### activity and each subject
data_mean <- data %>% group_by(activity, subject) %>%
        mutate_all(funs('mean' = mean))
len1 <- length(data) + 1
len2 <- length(data_mean)
data_mean <- data_mean %>% select(c(1, 2, len1:len2))
data_mean <- data_mean[!duplicated(data_mean),]
data_mean <- arrange(data_mean, subject)
### Remove extra data
rm(list = c('activity_labels', 'temp_data'))
### Finish
### Write a txt with the tidy data set
write.table(data_mean, file = 'tidy_data.txt', sep = ' ',
            row.names = FALSE)
#### END
