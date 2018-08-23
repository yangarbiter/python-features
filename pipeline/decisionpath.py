import sys
import joblib
import numpy as np
import csv
import sklearn.externals.joblib


file_path = sys.argv[3]
encfile = sys.argv[1]
enc = joblib.load(encfile)

def load_csv(file_path):
    ret = []
    spans = []
    with open(file_path, "r") as csvfile:
        reader = csv.reader(csvfile)
        for _, row in enumerate(reader):
            ret.append(row[1:])
            spans.append(row[0])
        header = ret[0]
        ret = ret[1:]
        spans = spans[1:]
    return ret, header, spans

ret = {}
nominals = ['F-Size', 'F-Size-P', 'F-Size-C1', 'F-Size-C2', 'F-Size-C3']
ignore = ['X-UD', 'X-DU']
v, header, spans = load_csv(file_path)
categories = [i for i in range(len(header[2:])) \
    if header[2:][i] not in nominals and header[2:][i] not in ignore]
nominals = [i for i in range(len(header[2:])) if header[2:][i] in nominals]
X = []
X += [np.array([r[2:][i] for i in categories], object) for r in v]

X = np.asarray(X)

l = np.array(v)[:, :2].astype(float)
n = np.array(v)[:, 2:][:, nominals]
n[n == ''] = 0
ret = np.hstack((l, enc.transform(np.array(v)[:, 2:][:, categories]).toarray(), n)).astype(float)

feature_names = []
for i, cates in enumerate(enc.categories_):
    for cat in cates:
        feature_names.append(header[2:][categories[i]] + '_' + str(cat))
for i in nominals:
    feature_names.append(header[2:][i])

# print(ret)
# print('------------------------------------------')
# print(header)
# print('------------------------------------------')
# print(ret_categories)

modelfile = sys.argv[2]
estimator = sklearn.externals.joblib.load(modelfile)
X_test = ret[:,2:]
test_labels = ret[:,1]
node_indicator = estimator.decision_path(X_test)


n_nodes = estimator.tree_.node_count
children_left = estimator.tree_.children_left
children_right = estimator.tree_.children_right
feature = estimator.tree_.feature
threshold = estimator.tree_.threshold

leave_id = estimator.apply(X_test)

for ind, _ in enumerate(X_test):
    # print ('----------------------------------')
    # print ('For span')
    print(spans[ind])
    # print ('with confidence')
    print ((estimator.predict_proba(X_test[ind].reshape(1,-1)))[0][1])
    # print ('our prediction is')
    # print ((estimator.predict(X_test[ind].reshape(1,-1)))[0])
    # print ('should be')
    print (test_labels[ind])
    sample_id = ind
    # print X_test[sample_id]

    node_index = node_indicator.indices[node_indicator.indptr[sample_id]:
                                        node_indicator.indptr[sample_id + 1]]

    print('Rules used to predict sample %s: ' % sample_id)
    for node_id in node_index:

        if leave_id[sample_id] == node_id:  # <-- changed != to ==
            #continue # <-- comment out
            # print("leaf node {} reached, no decision here".format(leave_id[sample_id])) # <--
            pass

        else: # < -- added else to iterate through decision nodes
            if (X_test[sample_id, feature[node_id]] <= threshold[node_id]):
                threshold_sign = False
            else:
                threshold_sign = True

            print("%s : %s"
                    % (feature_names[feature[node_id]],
                        threshold_sign))
            # print("decision id node %s : (X[%s, %s] (= %s) %s %s)"
            #       % (node_id,
            #          sample_id,
            #          feature[node_id],
            #          X_test[sample_id, feature[node_id]], # <-- changed i to sample_id
            #          threshold_sign,
            #          threshold[node_id]))
            # print(feature_names[feature[node_id]]);
    print ('----------------------------------')
