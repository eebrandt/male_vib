#!/usr/bin/python

# to process annotation files, and wav data once it's enabled
import maleviban as vib
# holds variables that will be shared with maleviban
import config as cfg
# for displaying file/folder choosers and message boxes to user
import Tkinter, Tkconstants, tkFileDialog, tkMessageBox
# lets us do file and folder operations within the operating system of the user
import os
# lets us make plots
import matplotlib.pyplot as plt
# for numerical operations and numpy arrays
import numpy as np
# for reading/writing csv files
import csv
# lets us transpose arrays
import itertools as it
# gets current date and time for timestamp
import datetime

startime = datetime.datetime.now()

vib.importanns("/home/eebrandt/projects/temp_trials/male_only/data/666/45-666/45-666.labels.txt")
vib.importwav("/media/eebrandt/Erin1/Erin_Berkeley/male_temp_vids/666/45-666.wav")
#figure(figsize=(3,10))
p1 = plt.plot(cfg.t,cfg.y)
plt.show()	

vib.featurefinder(cfg.lengths_output, "buzz", 2, cfg.wavdata, .25)
print "\a"

endtime = datetime.datetime.now()

print endtime - startime

