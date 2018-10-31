import numpy as np
import pandas as pd
import os
from utils import *

os.chdir('C:/Users/Grejell/AnacondaProjects/IDMatchingProject/')

yearOneData = pd.read_csv("./dta/yearOne.csv", engine='python')
yearTwoData = pd.read_csv("./dta/yearTwo.csv", engine='python')
yearOneData = yearOneData[["10DigitCardNumber", "FirstName", "Surname", "Position", "Email", "CompanyName"]]
yearTwoData = yearTwoData[["10DigitCardNumber", "FirstName", "Surname", "Position", "Email", "CompanyName"]]

yearOneData = function_1(yearOneData)
yearOneData = function_2(yearOneData)
yearOneData = function_3(yearOneData)

yearTwoData = function_1(yearTwoData)
yearTwoData = function_2(yearTwoData)
yearTwoData = function_3(yearTwoData)

data = function_4(yearOneData, yearTwoData)

data = function_5(data)
