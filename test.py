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
from scipy.signal import blackmanharris
from numpy.fft import rfft, irfft
import tkSimpleDialog



vib.importwav("/media/eebrandt/Erin1/Erin_Berkeley/male_temp_vids/929/12-929.wav", normalize = False, plot = False)
labelfilename = "/home/eebrandt/projects/dissertation/chapter_1/male_only/data/929/12-929/12-929.labels.txt"
vib.importanns(labelfilename)
vib.featurefinder(cfg.lengths_output, "buzz", 4, cfg.wavdata, .25)

vib.getfreq(cfg.feature[1][1], cfg.rate, True, 10000000)

vib.getpeaks(cfg.fft_dat[0], cfg.fft_dat[1], .10,  True, smooth = 10, plot_title = "Your plot, fine sir/madam: ", plotraw = False)

print cfg.fund
