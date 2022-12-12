Four different metrics approaches are used:

Resample: 
We provide perfomance metrics during resamples. (i.e. information about the results on each fold)

Test:
We provide classic performance metrics for the complete test dataset. Including information about each class

LOOCV:
train and test considering all but one animal. 

TestLOOCV:
Similar to Test, but we provide information for each animal and then we average the results.


Note: We use macro average in all cases. Notice that in some cases there is no difference between macro and micro average
