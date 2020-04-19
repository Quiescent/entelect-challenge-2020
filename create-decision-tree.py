#!/usr/bin/env python

import sys
from sklearn.model_selection import cross_val_score
from sklearn.ensemble import RandomForestClassifier
from sklearn.tree import export_text
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score
import pandas as pd


DATA_FILE_PATH = "data.csv"
POST_PROCESSED_DATA_FILE_PATH = "data_post_processed.csv"
MOVE_TRANSLATION = {
    'ACCELERATE': 0,
    'TURN_LEFT': 1,
    'TURN_RIGHT': 2,
    'USE_BOOST': 3
}
TRANSLATE_OUTCOME = {
    'T': 1,
    'NIL': 0
}
ALL_ATTRIBUTES = ['X',
                  'Y',
                  'Speed',
                  'Boosts',
                  'Mud_Ahead',
                  'Mud_Up',
                  'Mud_Down',
                  'Speed_Ahead',
                  'Speed_Up',
                  'Speed_Down',
                  'Move',
                  'Objective']
ALL_FEATURES = ['Speed',
                'Boosts',
                'Mud_Ahead',
                'Mud_Up',
                'Mud_Down',
                'Speed_Ahead',
                'Speed_Up',
                'Speed_Down',
                'Move']
LABELS = ['Objective']
LOOK_AHEAD = 2


def load_data(data_file_path):
    """Load data from DATA_FILE_PATH without preprocessing."""
    return pd.read_csv(data_file_path)


def load_data_with_preprocessing(data_file_path):
    """Load data from DATA_FILE_PATH and apply preprocessing."""
    data_frame = pd.read_csv(data_file_path)
    data_frame.columns = ALL_ATTRIBUTES
    data_frame['Move'] = data_frame['Move'].map(MOVE_TRANSLATION)
    data_frame['Objective'] = (data_frame['Objective']
                               .map(TRANSLATE_OUTCOME))
    return data_frame


def good_move_based_on_future_speed(data):
    """Update objective in DATA to T if future moves have a better acceleration."""
    for i in range(len(data) - 1):
        current_speed = data['Speed'][i]
        look_ahead = min(LOOK_AHEAD, scan_for_start(data, i) - i)
        average = average_speed_in_range(data, i + 1, look_ahead)
        data['Objective'][i] = 1 if average >= current_speed else 0


def average_speed_in_range(data, i, j):
    """Produce the average speed in DATA from rows I through I + J."""
    if j == 0:
        return data['Speed'][i]
    total = 0
    for k in range(i, i + j):
        total += data['Speed'][k]
    return total / j


def scan_for_start(data, i):
    """Produce the index in DATA ahead of I where the round ends."""
    for j in range(i, len(data)):
        if j == len(data):
            return j
        if data['X'][j] == 0:
            return j
    return i


def shuffle_data(data):
    """Produce a shuffled version of DATA."""
    return data.sample(frac=1)


def create_tree(data_frame):
    """Create a decision tree from DATA_FRAME."""
    y = data_frame['Objective']
    X = data_frame[ALL_FEATURES]
    decision_tree_classifier = RandomForestClassifier(n_estimators=10)
    return decision_tree_classifier.fit(X, y)


if __name__ == '__main__':
    if len(sys.argv) > 1 and sys.argv[1] == 'post_process':
        print('Post processing...')
        data = load_data_with_preprocessing(DATA_FILE_PATH)
        good_move_based_on_future_speed(data)
        data.to_csv(POST_PROCESSED_DATA_FILE_PATH)
        print('Done')
        sys.exit(0)
    data = load_data(POST_PROCESSED_DATA_FILE_PATH)
    # tree = create_tree(data)
    y = data['Objective']
    X = data[ALL_FEATURES]
    decision_tree_classifier = RandomForestClassifier(n_estimators=10)
    scores = cross_val_score(decision_tree_classifier, X, y, cv=5)
    print(scores)
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2)
    tree = decision_tree_classifier.fit(X_train, y_train)
    predictions = tree.predict(X_test)
    print(accuracy_score(y_test, predictions))
    # export_graphviz(tree.estimators_[0],
    #                 out_file='tree.dot',
    #                 feature_names=ALL_FEATURES,
    #                 rounded=True,
    #                 proportion=False,
    #                 precision=2,
    #                 filled=True)
    # call(['dot', '-Tpng', 'tree.dot', '-o', 'tree.png', '-Gdpi=600'])
    print(export_text(tree.estimators_[0],
                      feature_names=ALL_FEATURES,
                      show_weights=True))
