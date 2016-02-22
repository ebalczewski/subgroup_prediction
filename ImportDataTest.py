#from sklearn.tree import DecisionTreeRegressor

#from sklearn.ensemble import BaggingRegressor
from sklearn.ensemble import RandomForestRegressor

from sklearn.metrics import mean_squared_error
from sklearn.metrics import median_absolute_error
from sklearn.metrics import r2_score

import numpy as np


# ----> DO WE USE TRUE VALUE AND TWIN ESTIMATE OR BOTH ESTIMATES FOR Z?

# Things to try for regression step:
# 1) Train on controls to estimate twin of treatment. Train on treatment to estimate twin for controls.
# 2) Feature elimination before model fitting.
# 3) Bagging with different regressors.
# 4) Messing with parameters.
# 5) Cross-fold validation.


'''How well did our estimator do? Could also use internal scoring function for estimators
        or compare to dummy. Things to try later.
'''
def estimator_metrics(true_values, estimates):

    print "---------------------------------------"
    print "MSE: " 
    print mean_squared_error(true_values, estimates)
    print "MAE: " 
    print median_absolute_error(true_values, estimates)
    print "R-squared: " 
    print r2_score(true_values, estimates)
    print "---------------------------------------"

    return


'''Random forest regressor to estimate treatement outcomes based on training data.
        X: Array of size (number subjects, covariates)
        y: Array of size (number subjects, )
'''
def random_forest_regressor(X, y):

    # Building a random forest regressor model of the data
    regressor = RandomForestRegressor(n_estimators=50, min_samples_split=1)
    regressor.fit(X, y)

    # Predicting value of our target
    estimates = regressor.predict(X)

    # Prints out MSE, MAE, and R-squared metrics to compare true to estimated data
    estimator_metrics(y, estimates)

    return estimates


''' Swaps 0s and 1s for treatment groups.
'''
def invert(column):
    for i in range(len(column)):
        column[i] = 1 - column[i]

    return column


''' Computes z score for each patient:
        z = y_trt - y_control

        where y is the estimated treatment effect from our regressor.
'''
def z_scores(trt, estimates_true, estimates_inverted):
    n = len(trt)
    z_scores = np.empty(n)

    for i in range(n):
        # If patient was originally a control, their treatment "twin" is in the inverted estimate.
        if trt[i] == 0.0:
            z_scores[i] = estimates_inverted[i] - estimates_true[i]
        # If patient was originally treated, their control "twin" is in the inverted estimate.
        else: 
            z_scores[i] = estimates_true[i] - estimates_inverted[i]

    return z_scores


def main():

    labels = np.genfromtxt("Data/Training_Data.csv", dtype="S5", delimiter=",")
    data = np.genfromtxt("Data/Training_Data.csv", dtype=float, delimiter=",", skip_header=1)
    #training_data = np.genfromtxt("Data/Training_Data.csv", dtype=float, delimiter=",", skip_header=1)
    #testing_data = np.genfromtxt("Data/Data.csv", dtype=float, delimiter=",", skip_header=1)

    # Slices for ease of indexing the master array. Don't want to create copies! To use,
    #    just index with array[slice].
    s_null = slice(None,None,None)

    s_dset = (s_null, slice(0,1,None))
    s_id = (s_null, slice(1,2,None))
    s_trt = (s_null, slice(2,3,None))
    s_y = (s_null, slice(3,4,None))
    s_disc = (s_null, slice(4,24,None))
    s_cont = (s_null, slice(24,44,None))
    s_covariates = (s_null, slice(4,44,None))

    n = data.shape[0]

    # Covariates; what we are building the model off of
    X = np.c_[data[s_trt],data[s_covariates]]
    # True values of what we are trying to estimate (in this case, treatment effect)
    y = np.ravel(data[s_y])


    # Building a random forest regressor model of the data
    regressor = RandomForestRegressor(n_estimators=50, min_samples_split=1)
    regressor.fit(X, y)

    # Predicting value of our target (y), with true data, and inverted treatment groups
    estimates_true = regressor.predict(X)
    estimates_inverted = regressor.predict(np.c_[invert(data[s_trt]), data[s_covariates]])

    # Prints out MSE, MAE, and R-squared metrics to compare true to estimated data
    estimator_metrics(y, estimates_true)


    z = z_scores(data[s_trt], estimates_true, estimates_inverted)
    print np.mean(z)
    print np.median(z)
    print np.std(z)

    z_threshold = np.mean(z)

    

if __name__ == '__main__':
    main()