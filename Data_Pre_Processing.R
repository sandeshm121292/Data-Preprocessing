# Assignment_1 by Melkote Shivashankar Sandesh
#-----------------------------------------------------------------------------------------------
install.packages(c("caret", "rgl", "Amelia", "RANN"))
#-----------------------------------------------------------------------------------------------

# To read the data from adult.data file
#-----------------------------------------------------------------------------------------------

adult_db <- read.table(file="~/Desktop/Assignment 1/Datafiles/adult.data", header=FALSE, sep=",", 
                       na.strings=c("?","NA"), strip.white=TRUE, 
                       stringsAsFactors=FALSE)

fix(adult_db)
#-----------------------------------------------------------------------------------------------

# 2. Assign attribute names (column names) to the data we just imported
#-----------------------------------------------------------------------------------------------

names(adult_db) = c("age",
                    "workclass",
                    "fnlwgt",
                    "education",
                    "education_num",
                    "marital_status",
                    "occupation",
                    "relationship",
                    "race",
                    "sex",
                    "capital_gain",
                    "capital_loss",
                    "hours_per_week",
                    "native_country",
                    "class")
#-----------------------------------------------------------------------------------------------
# Inspect data set in tabular form
# -----------------------------
fix(adult_db)

# Change class labels to 1 (adults who earn more than 50K) and 0 (adults who earn less than or equal to 50K)
# ----------------------------------------------------------------------------------------------
adult_db$class[adult_db$class==">50K"] <- 1
adult_db$class[adult_db$class=="<=50K"] <- 0

# 3. Check for missing values
#-----------------------------------------------------------------------------------------------
# To find the missing values in the data.
#-----------------------------------------------------------------------------------------------
apply(adult_db, MARGIN=2, function(x) sum(is.na(x)))


# Delete records (rows) with any missing value
# --------------------------------------- #
adult_db_missing = adult_db
adult_db_nomiss <- na.omit(adult_db_missing)
apply(adult_db_nomiss,2,function(x) sum(is.na(x)))

# Number of row before deleting the missing value
nrow(adult_db_missing)
# Number of row after deleting the missing value
nrow(adult_db_nomiss)

adult_db_nomiss <- adult_db_missing[complete.cases(adult_db_missing),]
nrow(adult_db_nomiss)

# 4. To Select a	small	 data	sample	 of	 1000	 examples (rows) 
# ------------------------------------------------------------------------------- #
set.seed(1013)
idx = sample(1:nrow(adult_db_nomiss),1000)
adult_db_lim = adult_db_nomiss[idx,]
row.names(adult_db_lim) <- NULL

fix(adult_db_lim)
table(adult_db_lim$age)
table(adult_db_lim$class)

# 5. Examine attributes of the dataset
#----------------------------------------------------------------------------------------------
# Plot histogram for numeric attribute "age"
hist(adult_db_lim$age[adult_db_lim$class=="0"], breaks=50, main="Age Distribution", 
     xlab="Age", ylab="Frequency", col="red")
hist(adult_db_lim$age[adult_db_lim$class=="1"], breaks=50, main="Age Distribution", 
     xlab="Age", ylab="Frequency", col="blue",add=T)

#-----------------------------------------------------------------------------------------------
# Plot barchart for categorical attribute "race",
table(adult_db_lim$race)

barplot(table(adult_db_lim$race), col=1:5, xlab="Race", 
        main="Race of adults")
legend(x=0, y=20000, legend = c("Amer-Indian-Eskimo", "Asian-pac-Islander", "Black", "Other", "White"), 
       col = 1:5, pch=c(16))
#-----------------------------------------------------------------------------------------------

# 6. Plot a boxplot for attribute "Age" and show possible outlier for this attribute

boxplot(adult_db_lim$age, pch=20, col="red", main="Age of Adults", 
        ylab="Age")
boxplot.stats(adult_db_lim$age)$out
sort(boxplot.stats(adult_db_lim$age)$out)
adult_db_lim_trimmed<-adult_db_lim[adult_db_lim$age<=80,]

#-----------------------------------------------------------------------------------------------

# To Create new data set from our latest dataset with only numeric attributes
# ------------------------------------------------------------------------
adult_db_numeric <- adult_db_lim[,c("age", "fnlwgt", "education_num", "capital_gain", "capital_loss", "hours_per_week")]
class_val <- as.numeric(adult_db_lim[,c("class")])

# 7. Standardize numeric attributes in "adult_db_numeric" dataset.
# mean = 0 and sd = 1 for all numeric attributes
# -----------------------------------------------------------------------------------------------
adult_db_num_mean <- mean(class_val)

adult_db_num_std <- sd(class_val)

adult_std <- ( class_val - adult_db_num_mean)/adult_db_num_std 
mean(adult_std)
sd(adult_std)

adult_db_std <- scale(adult_db_numeric)

  
# we can check the mean and standard deviation of the standardized data

# ------------------------------------------------------------------
apply(adult_db_std, 2, mean)
apply(adult_db_std, 2, sd)

# 8. Run Principal Component Analysis (PCA) on the numeric dataset from above "adult_db_std"(I've changed the variable names)
# plot the first 2 principal components
# -----------------------------------------------------------------------------------------
pr.out <- prcomp(adult_db_std, scale = TRUE, center = TRUE)
names(pr.out)
head(pr.out$x)

principal_comp <- pr.out$x
head(principal_comp)

# ******** PLOT FOR FIRST TWO PRINCIPAL COMPONENTS GOES HERE ****** #  
plot(principal_comp[,1:2], col=(class_val + 2), pch=16,   main="First two Principal Components")
legend(x=-8, y=6, legend = c("<=50k", ">50k"), col = c(2, 3), pch=20 )
      
  
# 9. Plot percentage of the variance explained by principal components
# ----------------------------------------------------------------------------
pr.var = pr.out$sdev^2
pve = pr.var/sum(pr.var)
#-----------------------------------------------------------------------------------------------

par(mfrow=c(1,2), oma=c(0,0,2,0))
plot(pve , xlab=" Principal Component ", ylab="Variance Explained", ylim=c(0,1) ,type='b')
plot(cumsum(pve ), xlab=" Principal Component ", ylab ="Cumulative Proportion of Variance", ylim=c(0,1), type='b')
mtext("Proportion of variance explained by Principal Components", outer=TRUE, cex=1.2)
par(mfrow=c(1,1))
  
#-----------------------------------------------------------------------------------------------
# 9.C  
#From the above results 3 PC's	is used to capture	at	least	50% of	the	total	variance	in	the	dataset.
#	and 6 PC's	to	capture	at	least	90%	of the total variance in the dataset.
