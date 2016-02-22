from sklearn.tree import DecisionTreeRegressor, export_graphviz

#from sklearn.ensemble import BaggingRegressor
from sklearn.ensemble import RandomForestRegressor

from sklearn.metrics import mean_squared_error
from sklearn.metrics import median_absolute_error
from sklearn.metrics import r2_score

import numpy as np

from StringIO import StringIO


# ----> DO WE USE TRUE VALUE AND TWIN ESTIMATE OR BOTH ESTIMATES FOR Z?

# Things to try for regression step:
# 1) Train on controls to estimate twin of treatment. Train on treatment to estimate twin for controls.
# 2) Feature elimination before model fitting.
# 3) Bagging with different regressors.
# 4) Messing with parameters.
# 5) Cross-fold validation.
# 6) F-test?


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

# How to parse: http://scikit-learn.org/dev/auto_examples/tree/unveil_tree_structure.html

def decision_tree_regressor(X, y, labels):

    regressor = DecisionTreeRegressor(max_depth=3)
    regressor.fit(X, y)

    estimates_z = regressor.predict(X)
    leaves = regressor.apply(X)

    leaves_hash = np.zeros(np.max(leaves)+1)
    for i in range(len(y)):
        if ((estimates_z[i]-y[i])>0.05 and estimates_z[i]>0.6 and y[i]>0):
            # print estimates_z[i]
            # print y[i]
            # print estimates_z[i]-y[i]
            # print ((estimates_z[i]-y[i])>0.1 and estimates_z[i]>0 and y[i]>0)
            # print leaves[i]
            leaves_hash[leaves[i]] += 1
            # print leaves_hash[leaves[i]]
        else:
            leaves_hash[-1] += 1

    #print regressor.tree_.decision_path(X)
    print regressor.tree_.feature
    print regressor.tree_.threshold
    print leaves_hash
    print regressor.feature_importances_

    visualize_tree(regressor.tree_, labels)
    return estimates_z

def visualize_tree(tree, feature_names):
    """Create tree png using graphviz.

    Args
    ----
    tree -- scikit-learn DecsisionTree.
    feature_names -- list of feature names.
    """
    with open("dt.dot", 'w') as f:
        export_graphviz(tree, out_file=f,
                        feature_names=feature_names)

    command = ["dot", "-Tpng", "dt.dot", "-o", "dt.png"]
    try:
        subprocess.check_call(command)
    except:
        exit("Could not run dot, ie graphviz, to "
             "produce visualization")

def rule_extractor():

    return

def main():

    np.set_printoptions(precision=3, suppress=True)

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

    # Predicting value of our target (y, outcome), with true data, and inverted treatment groups
    estimates_true = regressor.predict(X)
    estimates_inverted = regressor.predict(np.c_[invert(data[s_trt]), data[s_covariates]])

    # Prints out MSE, MAE, and R-squared metrics to compare true to estimated data
    estimator_metrics(y, estimates_true)

    z = z_scores(data[s_trt], estimates_true, estimates_inverted)
    #print z
    # print np.mean(z)
    # print np.median(z)
    # print np.std(z)

    n_datasets = (data.shape[0] / 240)
    for i in range(n_datasets):
        ds_slice = slice(240*i,240*(i+1),None)
        estimates_z = decision_tree_regressor(data[ds_slice, s_covariates[1]], 
                                z[ds_slice], labels[s_covariates])


    # percent_good_z = np.empty(n)
    # for i in range(n):
    #     if z[i] > 0.6:
    #         percent_good_z[i] = 1
    #     else:
    #         percent_good_z[i] = 0

    # print np.mean(percent_good_z)



if __name__ == '__main__':
    main()