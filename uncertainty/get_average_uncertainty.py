# -*- coding: utf-8 -*-
"""
@author: mokuneva
"""

import pandas as pd

def get_average_uncertainty(group, n_articles=10):
    """
    Function to calculate the average uncertainty for each topic on a particular day.
    It selects 'n_articles' articles with the highest proportion of each topic and
    calculates the average uncertainty among these articles.
    Returns a dataframe with topic names as the columns and the average uncertainty as values.
    """
    # Prepare the list of topic column names
    topic_columns = [col for col in group.columns if col.startswith('T')]
    average_uncertainty_all_topics = []

    # Iterate through each topic
    for topic in topic_columns:
        # Sort the group by topic proportion and select top 'n_articles' articles
        top_articles = group.sort_values(by=topic, ascending=False).head(n_articles)
        # Calculate the average uncertainty
        average_uncertainty = top_articles['uncertainty'].mean()
        average_uncertainty_all_topics.append(average_uncertainty)

    # Return a dataframe with topic names as the columns and the average uncertainty as values
    return pd.DataFrame([average_uncertainty_all_topics], columns=topic_columns, index=[group['date'].iloc[0]])

