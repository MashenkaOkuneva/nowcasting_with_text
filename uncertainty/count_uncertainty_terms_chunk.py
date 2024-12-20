# -*- coding: utf-8 -*-
"""
Created on Thu Dec 12 18:35:08 2024

@author: mokuneva
"""

from sklearn.feature_extraction.text import CountVectorizer
import numpy as np

def count_uncertainty_terms_chunk(text_chunk, vocabulary):
    
    """Calculate uncertainty term counts using CountVectorizer"""
    
    vectorizer = CountVectorizer(vocabulary=vocabulary)
    X_chunk = vectorizer.fit_transform(text_chunk)
    return np.asarray(X_chunk.sum(axis=1)).flatten()