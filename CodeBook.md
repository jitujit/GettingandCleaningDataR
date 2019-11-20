## Peer-graded Assignment: Getting and Cleaning Data Course Project CodeBook

Data was created using following steps:

1) Merge the training and the test sets to create one data set.
2) Extract only the measurements on the mean and standard deviation for each measurement.
3) Use descriptive activity names to name the activities in the data set
4) Appropriately labels the data set with descriptive variable names.
5) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

Activites :

1 WALKING
2 WALKING_UPSTAIRS
3 WALKING_DOWNSTAIRS
4 SITTING
5 STANDING
6 LAYING

Feature Selection 
=================
The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 
Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 
Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 
These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

The set of variables that were estimated from these signals are: 

mean(): Mean value
std(): Standard deviation
mad(): Median absolute deviation 
max(): Largest value in array
min(): Smallest value in array
sma(): Signal magnitude area
energy(): Energy measure. Sum of the squares divided by the number of values. 
iqr(): Interquartile range 
entropy(): Signal entropy
arCoeff(): Autorregresion coefficients with Burg order equal to 4
correlation(): correlation coefficient between two signals
maxInds(): index of the frequency component with largest magnitude
meanFreq(): Weighted average of the frequency components to obtain a mean frequency
skewness(): skewness of the frequency domain signal 
kurtosis(): kurtosis of the frequency domain signal 
bandsEnergy(): Energy of a frequency interval within the 64 bins of the FFT of each window.
angle(): Angle between to vectors.

Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:

gravityMean
tBodyAccMean
tBodyAccJerkMean
tBodyGyroMean
tBodyGyroJerkMean

Identifiers:
ActivityId                           
ActivityName                        
SubjectId        
 
Completed List of varibles and idetifiers: 

 ActivityId                           ActivityName                         SubjectId                            TimeBodyAccMeanX                     TimeBodyAccMeanY                    
 TimeBodyAccMeanZ                     TimeBodyAccStdX                      TimeBodyAccStdY                      TimeBodyAccStdZ                      TimeGravityAccMeanX                 
 TimeGravityAccMeanY                  TimeGravityAccMeanZ                  TimeGravityAccStdX                   TimeGravityAccStdY                   TimeGravityAccStdZ                  
 TimeBodyAccJerkMeanX                 TimeBodyAccJerkMeanY                 TimeBodyAccJerkMeanZ                 TimeBodyAccJerkStdX                  TimeBodyAccJerkStdY                 
 TimeBodyAccJerkStdZ                  TimeBodyGyroMeanX                    TimeBodyGyroMeanY                    TimeBodyGyroMeanZ                    TimeBodyGyroStdX                    
 TimeBodyGyroStdY                     TimeBodyGyroStdZ                     TimeBodyGyroJerkMeanX                TimeBodyGyroJerkMeanY                TimeBodyGyroJerkMeanZ               
 TimeBodyGyroJerkStdX                 TimeBodyGyroJerkStdY                 TimeBodyGyroJerkStdZ                 TimeBodyAccMagMean                   TimeBodyAccMagStd                   
 TimeGravityAccMagMean                TimeGravityAccMagStd                 TimeBodyAccJerkMagMean               TimeBodyAccJerkMagStd                TimeBodyGyroMagMean                 
 TimeBodyGyroMagStd                   TimeBodyGyroJerkMagMean              TimeBodyGyroJerkMagStd               FreqBodyAccMeanX                     FreqBodyAccMeanY                    
 FreqBodyAccMeanZ                     FreqBodyAccStdX                      FreqBodyAccStdY                      FreqBodyAccStdZ                      FreqBodyAccMeanFreqX                
 FreqBodyAccMeanFreqY                 FreqBodyAccMeanFreqZ                 FreqBodyAccJerkMeanX                 FreqBodyAccJerkMeanY                 FreqBodyAccJerkMeanZ                
 FreqBodyAccJerkStdX                  FreqBodyAccJerkStdY                  FreqBodyAccJerkStdZ                  FreqBodyAccJerkMeanFreqX             FreqBodyAccJerkMeanFreqY            
 FreqBodyAccJerkMeanFreqZ             FreqBodyGyroMeanX                    FreqBodyGyroMeanY                    FreqBodyGyroMeanZ                    FreqBodyGyroStdX                    
 FreqBodyGyroStdY                     FreqBodyGyroStdZ                     FreqBodyGyroMeanFreqX                FreqBodyGyroMeanFreqY                FreqBodyGyroMeanFreqZ               
 FreqBodyAccMagMean                   FreqBodyAccMagStd                    FreqBodyAccMagMeanFreq               FreqBodyAccJerkMagMean               FreqBodyAccJerkMagStd               
 FreqBodyAccJerkMagMeanFreq           FreqBodyGyroMagMean                  FreqBodyGyroMagStd                   FreqBodyGyroMagMeanFreq              FreqBodyGyroJerkMagMean             
 FreqBodyGyroJerkMagStd               FreqBodyGyroJerkMagMeanFreq          AngleTimeBodyAccMeanGravity          AngleTimeBodyAccJerkMeanGravityMean  AngleTimeBodyGyroMeanGravityMean    
 AngleTimeBodyGyroJerkMeanGravityMean AngleXGravityMean                    AngleYGravityMean    
