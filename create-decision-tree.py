#!/usr/bin/env python

from sklearn.model_selection import cross_val_score
from sklearn import tree
import pandas as pd


DATA_FILE_PATH = "data.csv"
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
                  'Less_Than_Objective']
ALL_FEATURES = ['X',
                'Y',
                'Speed',
                'Boosts',
                'Mud_Ahead',
                'Mud_Up',
                'Mud_Down',
                'Speed_Ahead',
                'Speed_Up',
                'Speed_Down',
                'Move']
LABELS = ['Less_Than_Objective']


def load_data(data_file_path):
    """Load data from DATA_FILE_PATH and apply preprocessing."""
    data_frame = pd.read_csv(data_file_path).sample(frac=1)
    data_frame.columns = ALL_ATTRIBUTES
    data_frame['Move'] = data_frame['Move'].map(MOVE_TRANSLATION)
    data_frame['Less_Than_Objective'] = (data_frame['Less_Than_Objective']
                                         .map(TRANSLATE_OUTCOME))
    return data_frame


def create_tree(data_frame):
    """Create a decision tree from DATA_FRAME."""
    y = data_frame['Less_Than_Objective']
    X = data_frame[ALL_FEATURES]
    decision_tree_classifier = tree.DecisionTreeClassifier()
    return decision_tree_classifier.fit(X, y)


if __name__ == '__main__':
    data = load_data(DATA_FILE_PATH)
    # tree = create_tree(data)
    y = data['Less_Than_Objective']
    X = data[ALL_FEATURES]
    decision_tree_classifier = tree.DecisionTreeClassifier()
    scores = cross_val_score(decision_tree_classifier, X, y, cv=5)
    print(scores)
