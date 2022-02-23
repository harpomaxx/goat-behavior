Journal flow
================

## Contributions:

1.  Try to predict goat behavior with a focus on detecting Grazing from
    Mimosa trees. An ethogram considering Grazing, Grazing Mimosa,
    Resting, and Walking is considered.
2.  Build a classification model considering the minimum number of
    features while maintaining the required accuracy level

## Material and Methods

### **2.1 Animals, location, and instrumentation.**

A brief description of characteristics of the animal considered. The
location and the instruments used for measuring.

### **2.2. Observations**

Information about the times of the observations, Video recording
methods, etc.

### **2.3. Behavioural annotation of video files Video.**

Who annotate the datasets and the method used.

### **2.4 Ethogram development**

A brief description of the ethogram with the corresponding justification

### **2.5 Calculation of movement metrics**

Not sure if necessary. (Fogerty 2020) uses different epochs, but we are
using only 5m windows. In this section, (Fogerty 2020) also mentions the
features used according to a table.

### **2.6 Training and test data sets (Overview of the method)**

A Description of the dataset used for training and testing. In this
case, we have 14 datasets corresponding to 14 animals.

1.  **Dataset A:** 6 Animal are used for feature selection and analysis.

2.  **Dataset B:** The remaining 8 animals are used for evaluating the
    model capability using hold-out LOOCV

**Dataset A** is used only for feature selection. We generate \~30
datasets using bootstraping.

1.  A first approach uses BORUTA and we test models with top 5, 10, 15
    and 20 features always present according to BORUTA.

2.  ~~A second approach analyzes features using SHAP values when
    training a model. Summary Plots can be generated considering a
    sample of the \~30 datasets.~~

    At the end ~~6~~ 4 different sets of features are considered. (Four
    from BORUTA and ~~two from SHAP~~)

**Dataset B** is used for analyzing model performance using different
set of features using hold-out LOOCV

A description of the activities present in each dataset should be
included. (Fogerty 2020) presents the table at the beginning of the
results sections. but I think is better to put the info here.

### **2.7 ML Classifiers**

Boost classifiers: a brief description of the idea behind boosting

### **2.8 Feature selection**

#### BORUTA

A description of BORUTA algorithm for feature selection

#### SHAP Values

A description of SHAP values

### **2.9 Evaluation metrics**

A description of evaluation metrics used: Accuracy, Sensitivity and
Specificity, Macro and Micro

## 3. Results

### 3.1 Feature selection results.

Show a couple of tables withe the features selected by BORUTA

3.2 Model performance results

## Discussion

THe idea is to use SHAP values results for the best model to analize and
disccuss the quality of the features
