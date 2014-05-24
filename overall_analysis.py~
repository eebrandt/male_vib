#!/usr/bin/python
import maleviban as vib
import config as cfg
import Tkinter, Tkconstants, tkFileDialog
import os

#wavpath = '/home/eebrandt/projects/temp_trials/test/5-41.wav'
annwav = '/home/eebrandt/projects/temp_trials/test/5.41.test.labels'
Fs = 48000.0
# 1) file chooser.  Choose folder that contains all of the data-containing folders
# TODO: get all data folders and wavs together somehow
# TODO: finish data analysis
annotation_folder = tkFileDialog.askdirectory(initialdir= "/home/eebrandt/projects/temp_trials/test/data", title = "Choose the folder that contains annotations")
wav_folder = tkFileDialog.askdirectory(initialdir = "/home/eebrandt/projects/temp_trials/test/data", title = "Choose the folder that contains .wav files")
#print wav_folder
#print annotation_folder

individuals =  os.listdir(annotation_folder)
#loop to go through each folder
for individual in individuals:
	#print os.path.isdir(annotation_folder + "/" + individual)
	if os.path.isdir(annotation_folder + "/" + individual):
		# make sure each "individual" is a folder
		trials = os.listdir(annotation_folder + "/" + individual)
		# loop to go through each individual
		for trial in trials:
			#print wavpath
			# make sure each "trial" is a folder
			#print annotation_folder + "/" + individual + trial
			if os.path.isdir(annotation_folder + "/" + individual + "/" + trial):
				# run importanns
				labelfilename =  annotation_folder +"/"+ individual + "/" + trial + "/" + trial + ".labels.txt"
				vib.importanns(labelfilename)
				# run plotlengths, pull out: 
				vib.plotlengths(cfg.lengths_output[0], cfg.lengths_output[1], cfg.lengths_output[2], trial)
				vib.rates(cfg.lengths_output[3])
				vib.plot_rates(cfg.srtot, trial)
				# compile scrape durations and midpoints
				#finalarray[0] = 
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
				wavpath = wav_folder + "/" + individual + "/" + trial + ".wav"
				#print wavpath
				vib.importwav(wavpath)
				#print cfg.wavdata
				# for each feature type (scrape, thump, buzz):
				
					# for each feature
						# run featurefinder
				vib.featurefinder(cfg.lengths_output, "buzz", 1, cfg.wavdata, .25)
				#print len(cfg.wavdata[0])
				#print len(cfg.feature[1][0])
						# run getpeaks
						# show peaks, get confirmation that they're ok
						# threshold (to add later)
						# pull out peaks
				#print cfg.feature[1]
				#print cfg.feature[1][1]
				vib.getfreq(cfg.feature[1][1], cfg.rate, 10000000)
				vib.getpeaks(cfg.fft_dat[0], cfg.fft_dat[1], .02, trial + " buzz 2")
				# get pertinent info. (treatment, temp, date, weight, ct length) from other .csv file
				# *** export summary stats into one export file with summary info
				# *** export file that has x, y for time domain
				# *** export file that has x, y for frequency domain
			
		
#vib.importanns(annwav)
#vib.plotlengths(lengths_output[0], lengths_output[1], lengths_output[2])
#vib.newrates(lengths_output[3])
#vib.plot_durs(srtot)



#vib.getfreq(features[1][1], Fs)
#vib.getpeaks(fft_dat[0], fft_dat[1], .10)
