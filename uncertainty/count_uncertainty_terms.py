# -*- coding: utf-8 -*-
"""
Created on Mon Mar 25 14:40:09 2024

@author: mokuneva
"""

import re

def count_uncertainty_terms(text, uncertainty_terms):
    
    '''
    Counts occurrences of each term in the given set of uncertainty terms within the provided text.
    
    Returns:
    - dict: A dictionary with terms from the uncertainty_terms set that were found in the text as keys, 
            and their respective counts as values. The function accounts for case-insensitivity and 
            allows the terms to be followed by common punctuation marks, ensuring accurate 
            word boundary detection and counting.
    '''
    
    # Lowercase the text to match the case-insensitive search
    text_lower = text.lower()
    # Initialize a dictionary to hold counts of each uncertainty term
    term_counts = {}

    # Iterate over each term in uncertainty_terms
    for term in uncertainty_terms:
        # Use a regex pattern that allows for punctuation before and after the term
        pattern = r'[\'"`\(]*\b{}\b[.,!?;:\'"`\)]*'.format(re.escape(term))
        count = len(re.findall(pattern, text_lower))
        if count > 0:
            term_counts[term] = count
    
    return term_counts