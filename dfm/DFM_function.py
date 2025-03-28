# -*- coding: utf-8 -*-
"""
Created on Tue Mar 25 14:58:40 2025

@author: mokuneva
"""

import types
import numpy as np
import pandas as pd
import statsmodels.api as sm
from datetime import datetime
from dateutil.relativedelta import relativedelta


# --- Helper functions ---

def transform(column, transforms):
    transformation = transforms[column.name]
    # For quarterly data like GDP, we will compute
    # annualized percent changes
    mult = 4 if column.index.freqstr[0] == 'Q' else 1
    
    # 1 => No transformation
    if transformation == 1:
        pass
    # 2 => First difference
    elif transformation == 2:
        column = column.diff()
    # 3 => Log first difference, multiplied by 100
    #      (i.e. approximate percent change)
    #      with optional multiplier for annualization
    elif transformation == 3:
        column = np.log(column).diff() * 100 * mult
        
    return column

def load_data(vintage, q_var):
    
    # - Monthly data --------------------------------------------------------------
    # 1. Download data
    orig_m = (pd.read_csv(f'../data/vintages_monthly/{vintage}.csv')
                .dropna(how='all'))
    
    # 2. Extract transformation information
    transform_m = orig_m.iloc[0, 1:]
    orig_m = orig_m.iloc[1:]

    # 3. Extract the date as an index
    orig_m.index = pd.PeriodIndex(orig_m.date.tolist(), freq='M')
    orig_m.drop('date', axis=1, inplace=True)

    # 4. Apply the transformations
    dta_m = orig_m.apply(transform, axis=0,
                         transforms=transform_m)

    # - Quarterly data --------------------------------------------------------------
    # 1. Download data
    orig_q = (pd.read_csv(f'../data/vintages_quarterly/{vintage}.csv')
                .dropna(how='all'))
    # Keep the quarterly variable that will be forecasted
    orig_q = orig_q[['date', q_var]]

    # 2. Extract transformation information
    transform_q = orig_q.iloc[0, 1:]
    orig_q = orig_q.iloc[1:]

    # 3. Extract the date as an index
    orig_q.index = pd.PeriodIndex(orig_q.date.tolist(), freq='Q')
    orig_q.drop('date', axis=1, inplace=True)

    # 4. Apply the transformations
    dta_q = orig_q.apply(transform, axis=0,
                          transforms=transform_q)

    # - Output datasets ------------------------------------------------------
    return types.SimpleNamespace(
        orig_m=orig_m, orig_q=orig_q,
        dta_m=dta_m, transform_m=transform_m,
        dta_q=dta_q, transform_q=transform_q)

def vintage_dates(target_month):
    """
    Given a target month string in "YYYY-MM" format, this function returns a list of 7 vintage date strings.
    
    For example, if target_month is "2008-03", it returns:
      ['2008-01-01', '2008-01-16', '2008-02-01', '2008-02-16', '2008-03-01', '2008-03-16', '2008-04-01']
    """
    # Convert target_month to a date object representing the first day of that month
    target_date = datetime.strptime(target_month + "-01", "%Y-%m-%d").date()
    
    # The sequence should start two months before the target month
    start_month = target_date - relativedelta(months=2)
    
    vintages = []
    current = start_month
    # For each month from start_month to target_date (inclusive), add the 1st and 16th day of the month
    for _ in range(3):  # there are three months in a quarter
        first_day = current
        mid_month = current.replace(day=16)
        vintages.append(first_day.strftime("%Y-%m-%d"))
        vintages.append(mid_month.strftime("%Y-%m-%d"))
        current += relativedelta(months=1)
        
    # Append the first day of the month following the target month
    next_month = target_date + relativedelta(months=1)
    vintages.append(next_month.strftime("%Y-%m-%d"))
    
    return vintages

def factor_specification(groups, additional_factors=None):
    """
    Construct a dictionary mapping each variable
    to a list of factors according to the desired specification.

    Parameters:
      groups : pandas.DataFrame
          DataFrame that must contain at least two columns: 
          "Description" (the variable name) and "Group" (its group, e.g., 'Activity', 'Prices', 'Labor market',
          'Financial', or 'Surveys').
      
      additional_factors : None, str, or list of str
          - If None or an empty list, only "Global" is included.
          - If "all", then each variable loads on a global factor and a group-specific factor.
          - If a list (e.g. ['Labor market'] or ['Prices', 'Labor market']), 
            then a variable gets the extra factor only if its group is in that list.
            
    Returns:
      A dictionary where keys are the variable names and values are lists of factors.
    """
    factors = {}
    for _, row in groups.iterrows():
        desc = row['Description']
        group = row['Group']
        facs = ['Global']  # Always include the global factor
        
        if additional_factors:
            # If "all" then include each variable's own group as a factor.
            if additional_factors == "all":
                facs.append(group)
            # If additional_factors is a list, only include if the group's name is in the list.
            elif isinstance(additional_factors, list) and group in additional_factors:
                facs.append(group)
        factors[desc] = facs
    return factors

# --- Main function that produces forecasts for the quarter of interest based on 7 vintages ---
def get_forecasts(forecast_month, q_var, additional_factors, factor_multiplicities, factor_orders, start):
    """
    Given the input parameters, this function:
      - Generates the list of vintage dates for the forecast month.
      - Loads monthly and quarterly datasets for each vintage.
      - Loads variable definition files, renames variables in the original dataset and reorders them.
      - Specifies the factor structure based on additional_factors.
      - Constructs and fits a monthly Dynamic Factor Model for each vintage.
      - Returns a dictionary of forecast values (keyed by vintage) for GDP/Consumption/Investment.
    
    Parameters:
      forecast_month: string in "YYYY-MM" format (e.g., "2008-03")
      q_var: string, quarterly variable being forecasted (e.g., 'GDP')
      additional_factors: None, "all", or a list of groups (e.g., ['Labor market'])
      factor_multiplicities: dictionary (e.g., {'Global': 1})
      factor_orders: dictionary (e.g., {'Global': 3})
      start: string indicating start date for estimation sample (e.g., "1991-02")
    
    Returns:
      forecasts: dict mapping vintage date (string) to forecast value (for GDP/Consumption/Investment)
    """
    # Generate vintage dates
    vintages = vintage_dates(forecast_month)
    
    # Load data for each vintage
    dta = {vint: load_data(vint, q_var = q_var) for vint in vintages}
    
    # Load definitions for monthly and quarterly variables
    defn_m = pd.read_excel('../data/data_monthly/variables_definitions.xlsx')
    defn_m.index = defn_m['Mnemonic']
    defn_q = pd.read_excel('../data/data_quarterly/variables_definitions.xlsx')
    defn_q = defn_q[defn_q.Mnemonic == q_var]
    defn_q.index = defn_q['Mnemonic']
         
    # Create mapping from mnemonic to description
    map_m = defn_m['Description'].to_dict()
    map_q = defn_q['Description'].to_dict()
    
    # Replace column names for monthly and quarterly datasets in each vintage
    for vint in dta.keys():
        dta[vint].orig_m.columns = dta[vint].orig_m.columns.map(map_m)
        dta[vint].dta_m.columns = dta[vint].dta_m.columns.map(map_m)
        dta[vint].orig_q.columns = dta[vint].orig_q.columns.map(map_q)
        dta[vint].dta_q.columns = dta[vint].dta_q.columns.map(map_q)
    
    # Re-order the monthly data columns based on the definitions file order
    columns = [name for name in defn_m['Description'] if name in dta[vintages[0]].dta_m.columns]
    for vint in dta.keys():
        dta[vint].dta_m = dta[vint].dta_m.reindex(columns, axis=1)
        
    # Get groups (variable -> group) mapping from monthly definitions
    groups = defn_m[['Description', 'Group']].copy()
    
    # Add our quarterly variable into the "Activity" group
    q_var_description = defn_q.loc[q_var, 'Description']
    groups.loc[q_var] = {'Description': q_var_description, 'Group': 'Activity'}
    
    # Define factor structure using 'factor_specification' function
    factors = factor_specification(groups, additional_factors=additional_factors)
    
    # Loop over each vintage, fit model, and store forecast
    forecasts = {}
    for vint in vintages:
        # Get monthly and quarterly datasets for this vintage
        endog_m = dta[vint].dta_m.loc[start:, :]
        endog_q = dta[vint].dta_q.loc[start:, [q_var_description]]
        
        # Construct the Dynamic Factor Model
        model = sm.tsa.DynamicFactorMQ(
            endog_m, endog_quarterly=endog_q,
            factors=factors, factor_orders=factor_orders,
            factor_multiplicities=factor_multiplicities)
        
        # Fit the model
        results = model.fit(disp=10)
        
        # Get the point forecast for the quarter of interest
        point_forecast = results.get_prediction(start=forecast_month, end=forecast_month).predicted_mean[q_var_description]
        forecast_value = point_forecast.loc[forecast_month]
        forecasts[vint] = forecast_value
        
    return forecasts