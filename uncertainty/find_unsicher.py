# -*- coding: utf-8 -*-
"""
Created on Mon May 17 12:13:26 2021

@author: mokuneva
"""

import re

def find_unsicher(text):
    
    """Match words containing unsicher"""
    
    # Lowercase the text to make the search case-insensitive
    text = text.lower()
    
    # Matches words containing 'unsicher'
    pat = r'(\w*unsicher\w*)'
    
    return re.findall(pat, text)