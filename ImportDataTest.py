from sklearn.feature_selection import VarianceThreshold
import numpy as np

# Split into control and not
# For controls, find average outcome = baseline
# For patients, calculate treatment effect (difference of y from baseline)
# Find group of treated with -0.6<
# Figure out how we found that group
# Add back in control patients

def discrete_p_estimator(column):
    p = 0.0
    q = 0.0
    not_pq = 0.0
    for item in column:
        #print item
        if item==0.0:
            p+=1
        elif item==1.0:
            q+=1
        else:
            not_pq+=1
    return (p/column.shape[0],q/column.shape[0], not_pq/column.shape[0])

def running_mean(x, N):
    cumsum = np.cumsum(np.insert(x, 0, 0)) 
    print cumsum
    print (cumsum[N:] - cumsum[:-N]) / N
    return (cumsum[N:] - cumsum[:-N]) / N 


def main():


    labels = np.genfromtxt("Data/Training_Data.csv", dtype="S5", delimiter=",")
    training_data = np.genfromtxt("Data/Training_Data.csv", dtype=float, delimiter=",", skip_header=1)
    #testing_data = np.genfromtxt("Data/Data.csv", dtype=float, delimiter=",", skip_header=1)

    s_null = slice(None,None,None)
    s_dset = slice(0,1,None)
    s_id = slice(1,2,None)
    s_trt = slice(2,3,None)
    s_disc = slice(4,24,None)
    s_cont = slice(24,44,None)

    discrete = training_data[s_null,s_disc]
    # continuous = data[s_null, s_cont]
    # ids = data[s_null,s_id]


    running_mean([1,2,3,4,5,6,7,8,9,10], 10)
    # print discrete
    # print discrete.shape

    # for column in discrete.T:
    #     #print column.shape
    #     print discrete_p_estimator(column)


    # print labels
    # print data.shape
    # print data[:,4:24]
    # print data[rows, columns[4,24]]

if __name__ == '__main__':
    main()