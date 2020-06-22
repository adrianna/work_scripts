#######################################################################################
#
# Project AVI64 Characterization
# Author: Adrianna Galletta
# Date: 23 April 2020
#
# Description: Calculate Offset Error and Perform Calibration on Measurement
#
######################################################################################
import os

import csv
import math
import re
import pandas as pd
import numpy as np
from sklearn.linear_model import LinearRegression
from sklearn import metrics
from scipy import stats
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import pdb
import datetime
import argparse


# Read file function
def readFile(rFile):
    df = pd.read_csv(rFile)

    return df

###################################################################################
#######################        Main        ########################################
###################################################################################

# Change directory and Read-in file
print(os.getcwd())
# dataFile = "./poco_data.csv"
dataFile = "./poco_linearfit.csv"
dataFile = "./poco_small.csv"

file = readFile(dataFile)
file = file.rename(columns={"Error Offset": "Offset"})

# Calculate the predict values for mx+b Equation
slope_array = []
intercept_array = []
f_grp = file.groupby('Site')

for site, group in f_grp:
    site_data = f_grp.get_group(site)
    x = site_data['Voltage']
    y = site_data['Offset']
    # y = site_data['Error Offset']
    slope, intercept, r_value, p_value, std_err = stats.linregress(x, y)
    tmp = [slope] * len(f_grp.get_group(site))
    slope_array += tmp
    tmp = [intercept] * len(f_grp.get_group(site))
    intercept_array += tmp

    print(site)
    #print("slope: %d, intercept $d", slope, intercept)

    print("#################")

# Add 'Slope' and 'Intercept' column to the dataFrame
file['Slope'] = slope_array
file['Intercept'] = intercept_array

## Graph Predict Equation -- Optional
#for site, group in f_grp:
#    site_data = f_grp.get_group(site)
#    x = site_data['Voltage']
#    y = site_data['Offset']
#    y_predict = site_data['Site_Predict']
#    residuals = site_data['Residuals']
#    plt.plot(x, y, 'offset')
#    plt.plot(x, y_predict, 'offset_prediction')
#    plt.plot(x, residuals, 'residuals')
#    plt.show()
#    print("debug")

################ Custom Functions for Predict and Residual Equations
# Predict Equation Results using calculated slope and intercept
def predict(df):
    y_pred = df['Slope'] * df['Voltage'] + df['Intercept']

    return y_pred

# Add 'Offset_Predict' column to the dataFrame
offset_predict = file.groupby('Site').apply(predict)
print("debug")
file['Offset_Predict'] = offset_predict.to_list()

# Residual Equation
def residuals(df):
    y_resid = df['Offset_Predict'] - df['Offset']
    #print("residuals function")

    return y_resid

# Add 'Corrections' column to the dataFrame
corrections = file.groupby('Site').apply(residuals)
file['Corrections'] = corrections.to_list()


# Calculate Median and Standard Deviation for Corrections
correction_median = file['Corrections'].median()
correction_stdev = file['Corrections'].std()
print("Correction Factor, Median: %d", correction_median)
print("Correction Factor, Stdev:  %d", correction_stdev)

# Calculate the +/- 2 Std Boundary for 95% distribution
correction_ub = correction_median + 2 * correction_stdev
correction_lb = correction_median - 2 * correction_stdev
print("Boundaries for Correction: +/- 2*stdev = [%d, %d]", correction_ub, correction_lb)

#############################################################################################################
##### For Single Site - Test Graph
# Site 1 Data
#site_1 = file.groupby('Site').get_group(1)
#voltage = site_1['Voltage']
#empirical_offsets = site_1['Offset']*10e9
#predicted_offsets = site_1['Offset_Predict']*10e9
#corrected_offsets = site_1['Corrections']*10e9
#vmin = np.arange(len(voltage))[0]
#vmax = np.arange(len(voltage))[len(voltage)-1]
#vmin = voltage[0]
#vmax = voltage[len(voltage)-1]

#x_label = voltage
#plt.plot(empirical_offsets, color='green')
#plt.plot(predicted_offsets, color='red')
#plt.plot(corrected_offsets, color='orange')
#plt.hlines(correction_ub*10e9, vmin, vmax, linestyles='dashed', color='blue')
#plt.hlines(correction_lb*10e9, vmin, vmax, linestyles='dashed', color='blue')
#plt.hlines(correction_lb*10e9, vmin, vmax, linestyles='dashed', color='blue')

#plt.hlines(correction_ub, vmin, vmax, linestyles='dashed', color='blue')
#plt.hlines(correction_lb, vmin, vmax, linestyles='dashed', color='blue')

# Title
#plt.title('Error Offset/Corrections vs Voltage for Site 1')

# Labels
#vaxis = range(vmin, vmax+1)
#plt.xlabel('Voltage (V)')
#plt.ylabel('Error Offset/Corrections (nA)')
#plt.xticks(vaxis, voltage)

# Legend
#green_patch = mpatches.Patch(color='green', label='empirical_offset')
#red_patch = mpatches.Patch(color='red', label='predicted_offsets')
#orange_patch = mpatches.Patch(color='orange', label='corrections')
#blue_patch = mpatches.Patch(color='blue', label='spec limits: =/- 2stdev')
#plt.legend(handles=[green_patch, red_patch, orange_patch, blue_patch])

# Grid / Horizontal Axis Outline
#plt.axhline(y=0, xmin=vmin, xmax=vmax)
#plt.grid(True)

#plt.show()

#print("Analysis complete")
#############################################################################################################
print("Debug")
## Iterate for all Sites and Print Graphs
for site, data in file.groupby('Site'):
    print(site)
    if site > 0:
        voltage = data['Voltage']
        empirical_offsets = data['Offset']*10e9
        predicted_offsets = data['Offset_Predict']*10e9
        corrected_offsets = data['Corrections']*10e9
        index = np.arange(len(voltage)).tolist()
        volts = voltage.to_list()
        vmin = volts[0]
        vmax = volts[len(volts)-1]
        print("debug volts")
        print(type(volts))
       # voltage.set_index(index)
        x_label = volts

        plt.figure()

        plt.plot(volts, empirical_offsets, color='green')
        plt.plot(volts, predicted_offsets, color='red')
        plt.plot(volts, corrected_offsets, color='orange')
        plt.hlines(correction_ub*10e9, vmin, vmax, linestyles='dashed', color='blue')
        plt.hlines(correction_lb*10e9, vmin, vmax, linestyles='dashed', color='blue')
        plt.hlines(correction_lb*10e9, vmin, vmax, linestyles='dashed', color='blue')

        # Title
        title = "Error Offset/Corrections vs Voltage for Site " + str(site)
        plt.title(title)

        # Labels
        plt.xlabel('Voltage (V)')
        plt.ylabel('Error Offset/Corrections (nA)')
        #plt.xticks(np.arange(len(voltage)), voltage)
        plt.xticks(volts, volts)

        # Legend
        green_patch = mpatches.Patch(color='green', label='empirical_offset')
        red_patch = mpatches.Patch(color='red', label='predicted_offsets')
        orange_patch = mpatches.Patch(color='orange', label='corrections')
        blue_patch = mpatches.Patch(color='blue', label='spec limits: =/- 2stdev')
        plt.legend(handles=[green_patch, red_patch, orange_patch, blue_patch])

        # Grid / Horizontal Axis Outline
        plt.axhline(y=0, xmin=vmin, xmax=vmax)
        plt.grid(True)

        filename = 'Offsets4Site' + str(site) + '.png'
        plt.savefig(filename)
        plt.show()





##### Using existing data from Excel Spreadsheet for comparison

# Calculate median, std and +/- 2 std for 95% distribution
#correction_median = file['Residuals'].median()
#correction_stdev = file['Residuals'].std()
#print("Correction Factor, Median: %d", correction_median)
#print("Correction Factor, Stdev:  %d", correction_stdev)

# Graph the line +/- 2 std on the residual plot, along with y, y_predict
#pos_boundary = correction_median + 2 * correction_stdev
#neg_boundary = correction_median - 2 * correction_stdev
#print("Boundaries for Correction: +/- 2*stdev = [%d, %d]", pos_boundary, neg_boundary)




#########################################################################################################
### OVERVIEW - Plot of All Sites showing 'Voltage vs Error Offset' Relationship

### #file.groupby(['Voltage', 'Site']).sum()['Error Offset'].unstack().plot()
### #plt.legend(bbox_to_anchor = (1.05,1), loc = 'upper left', borderaxespad=0.)
### #plt.show()
### #plt.savefig('voltage_vs_offset.png')


print("hello")
