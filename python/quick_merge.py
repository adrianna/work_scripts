import pandas
import csv

dfile = ""
rfile = ""



df.rename(columns={'Test':'Reg'}, inplace=True)
rf.rename(columns={'Median': 'RefMedian','Std Dev': 'RefStd'}, inplace=True)
tmp = pd.concat([df,rf],axis=1)

tmp2 = df.merge(rf, how='left', left_on='Reg', right_on='Reg')
tmp3 = tmp2[tmp2['Algorithm']=="GEN_SVT"]
tmp3.to_csv("svt_review.csv", index=False)
