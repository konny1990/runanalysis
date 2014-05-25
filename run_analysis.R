# Read data files
subject_test  <- read.table(paste(testdir, "subject_test.txt", sep="/"), sep="\n", strip.white=T)
subject_train <- read.table(paste(traindir, "subject_train.txt", sep="/"), sep="\n", strip.white=T)

train_y <- read.table(paste(traindir, "y_train.txt", sep="/"), sep="\n", strip.white=T)
test_y  <- read.table(paste(testdir, "y_test.txt", sep="/"), sep="\n", strip.white=T)

train_x <- read.table(paste(traindir, "X_train.txt", sep="/"), sep="\n", strip.white=T)
test_x  <- read.table(paste(testdir, "X_test.txt", sep="/"), sep="\n", strip.white=T)

feature_names <- read.table(paste(datadir, "features.txt", sep="/"), sep="\n", strip.white=T)
feature_names <- gsub("^[0-9]+ ", "", feature_names$V1)

# Keep only features involving mean or std values
keep_features <- grepl("mean|std", feature_names)

# Done reading from files
rm(datafile, datadir, testdir, traindir)

# Break single column into multiples
train_x <- ldply(strsplit(gsub(" {2,}", " ", train_x$V1), " "))
test_x  <- ldply(strsplit(gsub(" {2,}", " ", test_x$V1), " "))

# Bind predicted value with subject and features
train <- cbind(train_y, subject_train, train_x)
test  <- cbind(test_y, subject_test, test_x)
rm(train_y, train_x, test_y, test_x, subject_train, subject_test)

# Combine train and test data sets
combined <- rbind(train, test)
rm(train, test)

# Trim data frame columns 
combined <- combined[,c(TRUE, TRUE, keep_features)]

# Label columns
column_headers <- c("Subject","Activity", feature_names[keep_features])
rm(feature_names, keep_features)
colnames(combined) <- column_headers

# make feature factor values numeric values
for (i in 3:ncol(combined)){
  combined[,i] <- as.numeric(combined[,i])
}

# write.csv(combined, file="combined_data_set.csv")

# Creates a second, independent tidy data set with the average of 
# each variable for each activity and subject

means <- aggregate( combined[,3] ~ combined$Subject + combined$Activity, data = combined, FUN = mean )

for (i in 4:ncol(combined)){
  means[,i] <- aggregate( combined[,i] ~ combined$Subject + combined$Activity, data = combined, FUN = mean )[,3]
}

# Replace column headers with descriptive labels
colnames(means) <- column_headers

# Replace activity code with descriptive label
labels <- c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")
means$Activity <- labels[means$Activity]

# Write to csv file
write.csv(means, file="average_by_activity_and_subject.csv")
