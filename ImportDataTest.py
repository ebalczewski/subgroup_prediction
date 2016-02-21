from sklearn.feature_selection import VarianceThreshold

from sklearn.tree import DecisionTreeRegressor

from sklearn.ensemble import BaggingRegressor
from sklearn.ensemble import RandomForestRegressor

from sklearn.metrics import mean_squared_error
from sklearn.metrics import median_absolute_error
from sklearn.metrics import r2_score

import numpy as np


# How well did our estimator do? Could also use internal scoring function for estimators
#   or compare to dummy. Things to try later.
def estimator_metrics(true_values, estimates):

    print "---------------------------------------"
    print "MSE: " 
    print mean_squared_error(true_values, estimates)
    print "MAE: " 
    print median_absolute_error(true_values, estimates)
    print "R-squared: " 
    print r2_score(true_values, estimates)
    print "---------------------------------------"

    return None

def main():

    labels = np.genfromtxt("Data/Training_Data.csv", dtype="S5", delimiter=",")
    training_data = np.genfromtxt("Data/Training_Data.csv", dtype=float, delimiter=",", skip_header=1)
    #testing_data = np.genfromtxt("Data/Data.csv", dtype=float, delimiter=",", skip_header=1)

    # Slices for ease of indexing the master array. Don't want to create copies! To use,
    #    just index with array[slice].

    s_null = slice(None,None,None)
    s_dset = slice(0,1,None)
    s_id = slice(1,2,None)
    s_trt = slice(2,3,None)
    s_y = slice(3,4,None)
    s_disc = slice(4,24,None)
    s_cont = slice(24,44,None)
    s_covariates = slice(4,44,None)

    # Random forest regressor to estimate treatement outcomes based on training data.
    #   X: Array of size (number subjects, covariates)
    #   y: Array of size (number subjects, )

    for i in range(50,500,50):
        true_values = np.ravel(training_data[s_y])
        regressor = RandomForestRegressor(n_estimators=i, min_samples_split=1)
        regressor.fit(training_data[s_covariates].T, true_values)
        estimates = regressor.predict(training_data[s_covariates].T)
        #print estimates

        # Prints out MSE, MAE, and R-squared metrics to compare true to estimated data.
        estimator_metrics(true_values, estimates)

if __name__ == '__main__':
    main()