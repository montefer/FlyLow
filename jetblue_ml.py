# -*- coding: utf-8 -*-
"""
Created on Sat Dec  2 10:18:13 2017

@author: Fernando
"""

import sklearn as sk
import pandas as pd
import csv
            #####################       ML Script in Python to be passed to R file        ######################

#Begin code
with open('some_csv.csv', 'w') as csvfile:
    spamwriter = csv.writer(csvfile, delimiter=' ',
                            quotechar='|', quoting=csv.QUOTE_MINIMAL)
    spamwriter.writerow(['Spam'])
    spamwriter.writerow(['Spam', 'Lovely Spam', 'Wonderful Spam'])