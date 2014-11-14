#!/usr/bin/python

#Oscillogram.py
# Erin Brandt 11/14/14

# This script will pull either a feature or an entire vibratory song out into a .csv file that can then be turned into a pretty graph with an R script (oscillogram.R).  This is pretty stripped down, and you'll have to specify which file you want to use within the script itself.
# Feel free to add a file chooser if this will make it easier for the intended user.
# Note that the csv file only has one column, which is the amplitude of each point.  The R script will add the time component.

# functions that handle annotation data and .wav files
import maleviban as vib
# holds variables that will be shared with maleviban
import config as cfg
# for reading/writing csv files
import csv

# here we import an annotation file and a .wav file.  You can comment out the annotation file line if you only want to import the entire file
vib.importanns("/home/eebrandt/projects/temp_trials/male_only/data/679/27-679/27-679.labels.txt")
vib.importwav("/home/eebrandt/projects/temp_trials/test/27-679.wav", normalize = True)

# this will help you specify which specific feature you want to capture.  It is commented out if you want the entire song
#vib.featurefinder(cfg.lengths_output, "scrape", 11, cfg.wavdata, .25)

# makes a .csv file and writer object with which to write the .csv file.  Again, this is specified by hand here.
# might need another line if we only want to look at one feature.
resultFile = open("/home/eebrandt/projects/temp_trials/male_only/analysis/output.csv",'wb')
wr = csv.writer(resultFile, dialect='excel')

# writes the wav data to the .csv file
for value in cfg.wavdata[1]:
    	wr.writerow([value])
	
