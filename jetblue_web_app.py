# -*- coding: utf-8 -*-
"""
Created on Sat Dec  2 01:27:49 2017

@author: Fernando
"""
                    
                            ####################    OBJECTIVES    ####################

#       1) Use RStudio to clean data and process it
#       2) Use Python to predict when low fares to a given location are going to appear given data from R
#       3) Output important information (graphs, charts, etc) to some web app built using Flask
#       4) Make the web app look nice with HTML, CSS and JavaScript



#import rpy2
from flask import Flask, render_template


app = Flask(__name__)

@app.route("/")
def main():
    return render_template('index.html')

@app.route('/showSignUp')
def showSignUp():
    return render_template('signup.html')

if __name__ == "__main__":
    app.run()