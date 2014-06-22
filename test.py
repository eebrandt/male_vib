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
import wave
import scipy

startime = datetime.datetime.now()


#print max(wav[1])
#print float(wav[1][48000 * 46.043])/32768
#vib.importanns("/home/eebrandt/projects/temp_trials/male_only/data/929/12-929/12-929.labels.txt")
vib.importwav("/home/eebrandt/projects/temp_trials/test/b1.wav", normalize = True)
#figure(figsize=(3,10))
#p1 = plt.plot(cfg.t,cfg.y)
#plt.show()	

#vib.featurefinder(cfg.lengths_output, "buzz", 0, cfg.wavdata, .25)
#print cfg.wavdata
#p1 = plt.plot(cfg.feature[0][0],cfg.feature[0][1],'r') # plotting the spectrum
#plt.show()
#vib.getfreq(cfg.feature[1][1], cfg.rate, 10000000)
#print cfg.feature[0][1]
#print cfg.feature[0][1]
#print cfg.wavdata
mockfeature = [[[], []], [[],[]]]
#print mockfeature
mockfeature[0][0] = cfg.wavdata[0] 
mockfeature[0][1] = cfg.wavdata[1]
mockfeature[1][0] = 0
mockfeature[1][1] = 0
#print mockfeature[0]
rms = vib.rms_feature(mockfeature[0][1]) 
print rms
#print cfg.rms
#vib.getpeaks(cfg.fft_dat[0], cfg.fft_dat[1], .10, "plot", True)
#print cfg.final_peaks
endtime = datetime.datetime.now()

#print endtime - startime

