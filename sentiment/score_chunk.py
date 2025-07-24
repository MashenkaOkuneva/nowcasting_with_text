# -*- coding: utf-8 -*-
"""
Created on Thu Jun 26 18:35:36 2025

@author: mokuneva
"""

from sklearn.feature_extraction.text import CountVectorizer
import numpy as np

def score_chunk(text_chunk, vocabulary, score_array):
    """
    For a chunk of texts, count occurrences of each vocabulary term
    and return a NumPy array of sentiment score sums.
    """
    vectorizer = CountVectorizer(vocabulary=vocabulary)
    X = vectorizer.fit_transform(text_chunk)
    # X is (n_docs, n_vocab), score_array is (n_vocab,)
    score_sum = X.dot(score_array)
    return np.asarray(score_sum).flatten()