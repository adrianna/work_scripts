import csv
import math
import re
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import pdb
import datetime
import argparse


######################################
### readRawFile(dFile)
######################################
def readRawData(dFile):
    #print("  :readRawData")
    
    f = open(dFile, "r")
    if f.mode == 'r':
        contents = f.readlines()

    rows=[]
    site_start = False
    site = 'XX'
    xCoord=1
    yCoord=1
    num_results = 0
    labels = ['Program Name', 'Site', 'X', 'Y', 'Reg', 'Value']
    #labels = ['X', 'Y', 'Reg', 'Value']
#    import pdb; pdb.set_trace()
    for l in contents:
        l = l.rstrip()
        if l.startswith("ProgramName"):
            program = l.split("=")[1]
        if l.startswith("[Site-Section]"):
            site_start = True
            num_results = 0
        if l.startswith("ParamSiteId"):
            site = l.split("=")[1]
        if l.startswith("XCoordNo"):
            site_start = True
            xCoord = l.split("=")[1]
        if l.startswith("YCoordNo"):
            yCoord = l.split("=")[1]
        if l == '\n':
            site_start = False
        if (site_start):
            if l.startswith("d",0):
                single_row = ()
                num_results += 1
                reg = l.split(",")[0].split("=")[1]
                value = l.split(",")[1]
                single_row = (program, site, xCoord, yCoord, reg, value)
#                single_row = (xCoord, yCoord, reg, value)
                rows.append(single_row)

    df = pd.DataFrame.from_records(rows, columns=labels)

    return df

######################################
### readRefFile(refFile)
######################################
def readRefFile(rFile):

    [rfname, ext] = rFile.split(".")
    sheet=re.search(r'(Z11[B|N])',rfname).group(1)
    if ext == "csv":
        rf = readCsvFile(rFile)
    elif ext == "xlsx" or ext == "xls":
        rf = readExcelFile(rFile, sheet)
    else:
        print("Bad file type")


        
    return rf

######################################
### readExcelFile(refFile, sheet)
######################################
def readExcelFile(rFile, sheet):
    with pd.ExcelFile(rFile) as xls:
        df = pd.read_excel(xls,sheet)

    df.rename(columns={'Reg Num' : 'Reg'}, inplace=True)
    df['Reg'].replace('R0*','', regex=True, inplace = True)
    df = df.drop(['Register'], axis=1)
    df.rename(columns={'Mean':'Ref_Mean', 'STD': 'Ref_Std', 'Reg': 'Register'}, inplace=True)
    
    return df


######################################
### readCsvFile(csvFile)
######################################
def readCsvFile(dFile):
    df = pd.read_csv(dFile)
    #pdb.set_trace()
    df.rename(columns={'Reg Num' : 'Reg'}, inplace=True)
    df['Reg'].replace('R0*','', regex=True, inplace = True)
    df = df.drop(['Register'], axis=1)
    df.rename(columns={'Mean':'Ref_Mean', 'STD': 'Ref_Std', 'Reg': 'Register'}, inplace=True)
    
    return df

######################################
### mergeData(d1,d2)
######################################

def mergeData(d1,d2):

    d1[['Value']] = d1[['Value']].apply(pd.to_numeric)
    d1[['Reg']] = d1[['Reg']].apply(pd.to_numeric)
    #d1['Value'].groupby(['Reg']).agg(np.mean)
    
    d1 = d1.groupby(['Reg'], as_index=False).agg({'Value': ['mean','std'],
                                                  'Program Name' : 'first'})
    d1.to_csv("d1_unsorted.csv")
    d1.columns = [' '.join(col).strip() for col in d1.columns.values]
    #import pdb; pdb.set_trace()
    #print("DEBUG")    
    oldNames = sorted(d1.columns.values.tolist())
    newNames = ['Program Name', 'Reg', 'Mean', 'Std']
    
    d1 = d1.rename(columns=dict(zip(oldNames,newNames), inplace=True))
    #    d1.columns = d1[['Program Name', 'Reg','Mean', 'Std']]    
    d1.to_csv("d1_preliminary.csv", index=False)

    allD = pd.concat([d1,d2], axis=1)
    allD = allD.drop(['Reg'],axis=1)
    #d1.merge(d2, how='left', left_on='Reg', right_on='Reg')
    allD.to_csv("all.csv", index=False)
    #    d1.columns = d1.columns.get_level_values(0)

    return allD

######################################
### msCompute(dd)
######################################
def msCompute(dd):
    ms = 0
    if dd['Ref_Std'] == 0:
        dd['Ref_Std'] = 0.1
        print("WARNING: Divide by zero, reference std: {}".format(dd['Ref_Std']))
        
    if (abs(dd['Mean']-dd['Ref_Mean'])/dd['Ref_Std']) <= 1.1:
        ms = 1
    return ms

######################################
### srCompute(d1,d2)
######################################
def srCompute(dd):
    sr = 0
    if dd['Ref_Std'] == 0:
        dd['Ref_Std'] = 0.1
        print("WARNING: Divide by zero, reference std: {}".format(dd['Ref_Std']))

    print("{}, {}".format(dd['Std'], dd['Ref_Std']))    
    if (dd['Std']/dd['Ref_Std']) <= 1.2:
        sr = 1
    
    return sr

######################################
### mssrCompute(dd)
######################################
def mssrCompute(dd):
    return dd['MS']*dd['SR']

######################################
### computeStats(dd)
######################################
def computeStats(dd):
    dd['MS'] = dd.apply(msCompute, axis=1)
    dd['SR'] = dd.apply(srCompute, axis=1)
    dd['MSSR'] = dd.apply(mssrCompute, axis=1)

    return dd

######################################
### filterOnAlgorithm(dd,filter)
######################################
def filterOnAlgorithm(dd, f):
    dd = dd[dd['Algorithm']==f]

    return dd


######################################
#### Default Input Files
######################################
#dataFile = "./ref/20180521200130_353490L.00L_490L-23_FRP"
#refFile = "./ref/Z11_HistoricalData.xls"
dataFile = "./ref/short_parse"
#dataFile = "./ref/Micron_Demo_20180911_151248.csv"
refFile = "./ref/new_ref.xls"





ap = argparse.ArgumentParser(description='Post-process Micron raw data')
ap.add_argument("-i", "--input", help="input raw file", required = True, type=str)
#ap.add_arguemnt("-r", "--read", help="read Only")
ap.add_argument("-r", "--ref", help="optional, use alternative Reference File", type=str)
ap.add_argument("-o", "--output", help="optional, specify output file name over the default generated name", type=str)
ap.add_argument("-f", "--filter", help="optional, Filter on specific algorithm, default: all algorithms are postprocessed", type=str)
args = vars(ap.parse_args())

print(args)

datafile = args["input"]
print("1. Parsing Raw Data:\n\t{}".format(datafile))
rawdata = readRawData(datafile)
#print(rawdata)
rawdata.to_csv("Micron_0911.csv")

if args["ref"]:
    refFile = args["ref"]
    print("2. Using Historical Reference:\n\t{}".format(refFile))
else:
    print("2. Using Default Historical Reference:\n\t{}".format(refFile))
z11r = readRefFile(refFile)    

print("3. Merging Data and Computing Stats")
alldata = mergeData(rawdata,z11r)
alldata = computeStats(alldata)

select = "all"
filtName = ""
if (args["filter"]):
    
    print("4. Filtering on Algorithm:\n\t{}".format(select))
    import pdb; pdb.set_trace()
    alldata = filterOnAlgorithm(alldata,select)
    filtName = select
else:
    print("4. Skip Filtering of Data")
    
if args["output"]:
    filename = args["output"]
else:
    timestamp = '{:%Y%m%d_%H%M%S}'.format(datetime.datetime.now())
    filename = "Micron_Results_" + timestamp
    if (len(filtName) > 0):
        filename = filename + "_" + select + "_" + ".csv"
    else:
        filename = filename + ".csv"
        
        print("5. Printing to Output File:\n\t{}".format(filename))
        alldata.to_csv(filename)
        



    
