#!/usr/bin/python
import maleviban as vib
import config as cfg
import Tkinter, Tkconstants, tkFileDialog
import os

wavpath = '/home/eebrandt/projects/temp_trials/test/5-41.wav'
annwav = '/home/eebrandt/projects/temp_trials/test/5.41.test.labels'
Fs = 48000.0
# 1) file chooser.  Choose folder that contains all of the data-containing folders
# TODO: get all data folders and wavs together somehow
# TODO: finish data analysis
annotation_folder = tkFileDialog.askdirectory(initialdir= "/home/eebrandt/projects/temp_trials/test/data", title = "Choose the folder that contains annotations")
wav_folder = tkFileDialog.askdirectory(initialdir = "/media/eebrandt/Erin1/Erin_Berkeley/temp_vids", title = "Choose the folder that contains .wav files")
#print wav_folder
print annotation_folder

individuals =  os.listdir(annotation_folder)
#loop to go through each folder
for individual in individuals:
	print annotation_folder + "/" + individual
	print os.path.isdir(annotation_folder + "/" + individual)
	if os.path.isdir(annotation_folder + "/" + individual):
		# make sure each "individual" is a folder
		trials = os.listdir(annotation_folder + "/" + individual)
		# loop to go through each individual
		for trial in trials:
			# make sure each "trial" is a folder
			if os.path.isdir(annotation_folder + "/" + individual + "/" + trial):
				# run importanns
				labelfilename =  annotation_folder +"/"+ individual + "/" + trial + "/" + trial + ".labels.txt"
				vib.importanns(labelfilename)
				# run plotlengths, pull out: 
				vib.plotlengths(cfg.lengths_output[0], cfg.lengths_output[1], cfg.lengths_output[2])
					# all scrapes, thumps and buzzes 
					# average scrape duration
					# average thump duration	
					# average buzz duration
					# avg. scrape each quarter
					# avg thump each quarter
					# avg. buzz each quarter
				# run rates, pull out:
					# rate for each scrape-rate period
					# average rates for each quarter
				# run importwav
				# for each feature type (scrape, thump, buzz):
					# for each feature
						# run featurefinder
						# run getpeaks
						# show peaks, get confirmation that they're ok
						# threshold (to add later)
						# pull out peaks
				# get pertinent info. (treatment, temp, date, weight, ct length) from other .csv file
				# *** export summary stats into one export file with summary info
				# *** export file that has x, y for time domain
				# *** export file that has x, y for frequency domain
			
		
#vib.importanns(annwav)
#vib.plotlengths(lengths_output[0], lengths_output[1], lengths_output[2])
#vib.newrates(lengths_output[3])
#vib.plot_durs(srtot)

#vib.importwav(Fs, wavpath)
#vib.featurefinder(lengths_output,2,3,wavdata, .25)
#vib.getfreq(features[1][1], Fs)
#vib.getpeaks(fft_dat[0], fft_dat[1], .10)
