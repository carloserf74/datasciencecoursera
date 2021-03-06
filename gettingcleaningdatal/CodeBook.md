# CodeBook

Human Activity Recognition Using Smartphones Data Set

**Abstract**: Human Activity Recognition database built from the recordings of 30 subjects performing activities of daily living (ADL) while carrying a waist-mounted smartphone with embedded inertial sensors.


| TITLE                          | VALUE                      | TITLE                     | VALUE | TITLE                 | VALUE      |
|--------------------------------|----------------------------|---------------------------|-------|-----------------------|------------|
| **Data Set Characteristics:**  | Multivariate, Time-Series  | **Number of Instances:**  | 10299 | **Area:**             | Computer   |
| **Attribute Characteristics:** | N/A                        | **Number of Attributes:** | 561   | **Date Donated:**     | 2012-12-10 |
| **Associated Tasks:**          | Classification, Clustering | **Missing Values?**       | N/A   | **Num. of Web Hits:** | 420836     |

This is a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data.

## The Data Source

**Data:** [https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip] (https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)

**Description of dataset:** [http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones] (http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)

## Data Set Information

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

## Attribute Information:

For each record in the dataset it is provided: 

* Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration. 
* Triaxial Angular velocity from the gyroscope. 
* A 561-feature vector with time and frequency domain variables. 
* Its activity label. 
* An identifier of the subject who carried out the experiment.

#### The dataset includes:

* features_info.txt: Show 561 features vector
* features.txt: List all of features
* activity_labels.txt: Show 6 activity name
* train/X_train.txt: Show 7352 training observations of the 561 features
* subject_train.txt: Show 7352 IDs of the volunteers. Its range is from 1 to 30
* train/y_train.txt: Show 7352 training labels 
* test/X_test.txt:  Show 2947 test observations of the 561 features
* subject_test.txt: Show 2947 IDs of the volunteers. Its range is from 1 to 30
* test/y_test.txt': Show 2947 test labels

#### Check the README.txt file for further details about this dataset. 
