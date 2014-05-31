#!/usr/bin/python
import maleviban as vib
import config as cfg
import Tkinter, Tkconstants, tkFileDialog, tkMessageBox
import tkMessageBox
import os
import matplotlib.pyplot as plt
import numpy as np
import csv
import itertools as it
import datetime

# get timestamp for files we'll save
timestamp = datetime.datetime.now().strftime("%Y-%m-%d_%H:%M:%S")

# ask for file that contains general animal info. (treatment, temp., size & weight, etc.)
animal_information_file = tkFileDialog.askopenfilename(initialdir = "/home/eebrandt/projects/temp_trials/male_only/data/", title = "Choose the file that contains animal information")    
if animal_information_file == "":
	tkMessageBox.showerror(
            "Open file",
            "You need to choose a file with animal information." )
	raise SystemExit

# opens animal_info file and puts it in numpy array NOTE: "names" and "missing values" needs to change if you add or subtract columns to this file

kwargs = dict(delimiter=",",
               dtype="S10, S10, S10, S10, S10, S10, f20, f20, f20, S20, S200",
               names= "tape, video_number, individual, date, treatment, rank, temp, weight, ceph_width, comments, complete",
               missing_values={0:"N/A", 1:"N/A", 2:"",3:"N/A", 4:" ", 5:"N/A", 6:"N/A", 7:"N/A", 8:"N/A", 9: " ", 10: " "},)
try:
	animal_info = np.genfromtxt(animal_information_file, **kwargs)
except:
	tkMessageBox.showerror(
        "Open file",
        "Something is wrong with your animal information file.  Check documentation to fix your file.")
	raise SystemExit

# sets up an array so we can search through animal_info to find the individuals we're analyzing in our annotation files
trialname = []
readvar = 1
trialname.append("trial")
while readvar < animal_info.shape[0]:
	trialname.append(animal_info["video_number"][readvar] + "-" + animal_info["individual"][readvar])
	readvar = readvar + 1
ndtrialname = np.array(trialname)

# gets the folder that contains all the annotation files.  Should be a directory (such as "data") that contains each individual, then trial underneath it.
annotation_folder = tkFileDialog.askdirectory(initialdir= "/home/eebrandt/projects/temp_trials/test/data", title = "Choose the folder that contains annotations")

# asks the user if they want to do spectral analysis.  If so, asks user for folder where .wav files can be found.
wav_load = tkMessageBox.askyesno("Spectral Analysis", "Do you want to load .wav files to do spectral analysis?")
if wav_load:
	tkMessageBox.showerror(
	"Frequency Information",
	"We haven't written the frequency analysis yet.  Go directly to maleviban to get peak info.")
# gets the folder that contains all the .wav files (if doing spectral analysis).  This should be commented out once spectral analysis algorithms are finalized.
#if wav_load:
#	wav_folder = tkFileDialog.askdirectory(initialdir = "/home/eebrandt/projects/temp_trials/test/data", title = "Choose the folder that contains .wav files")


duration_plots =  tkMessageBox.askyesno("Duration Plots", "Do you want to view duration plots?")
rate_plots = tkMessageBox.askyesno("Rate Plots", "Do you want to view scrape rate plots?")

# gets the indivudals based on everything that's in the top-level folder (folders and files)
individuals =  os.listdir(annotation_folder)
#loop to go through each "individual"
for individual in individuals:
	# checks to see which "individuals" are actually folders
	if os.path.isdir(annotation_folder + "/" + individual):
		# make sure each "individual" is a folder
		trials = os.listdir(annotation_folder + "/" + individual)
		# loop to go through each individual
		for trial in trials:
			labelfilename =  annotation_folder +"/"+ individual + "/" + trial + "/" + trial + ".labels.txt"
			if os.path.isdir(annotation_folder + "/" + individual + "/" + trial) and os.path.isfile(labelfilename):
				trialindex = trialname.index(trial)
				outputarray = []
				an_info = []
				an_info.append(animal_info[trialindex]["tape"] + "-" + animal_info[trialindex][1])
				an_info.append(animal_info[trialindex]["complete"])
				an_info.append(animal_info[trialindex]["individual"])
				an_info.append(animal_info[trialindex]["treatment"])
				an_info.append(animal_info[trialindex]["rank"])
				an_info.append(animal_info[trialindex]["date"])
				tempf = (float(animal_info[trialindex]["temp"]))
				tempc = (tempf - 32) * 5.0/9.0
				an_info.append(tempc)
				an_info.append(animal_info[trialindex]["weight"])
				an_info.append(animal_info[trialindex]["ceph_width"])

				try:
					vib.importanns(labelfilename)
				except:
					tkMessageBox.showerror(
           				"Annotation File Error",
            				labelfilename + " doesn't look right. Look through the documentation to fix your file.")
					raise SystemExit

				# runs the rate analysis.  Gives the user an error if there's something wrong with the file
				try:
					vib.rates(cfg.lengths_output[3])
				except:
					tkMessageBox.showerror(
					"Rate Error",
					"Something about " + labelfilename + " is making rates not work.  See documentation to fix.")
					raise SystemExit

				#shows duration and rate plots if the user requested them
				if duration_plots:
					vib.plotlengths(cfg.lengths_output[0], cfg.lengths_output[1], cfg.lengths_output[2], trial)
				if rate_plots:
					vib.plot_rates(cfg.srtot, trial)
						
				# compiling info. to put into a csv.
				# makes an nparray the size and shape we need to eventually write the csv.  Note that this will make many empty spaces, since most of the data only have 
				Maxlen = max(len(cfg.lengths_output[0][0]), len(cfg.lengths_output[1][0]), len(cfg.lengths_output[2][0]), len(cfg.lengths_output[3][0]), )
				npoutput = np.zeros((19, Maxlen), dtype="S20")

				readvar = 0
				while readvar < 9:
					npoutput[readvar][0] = str(an_info[readvar])
					readvar = readvar + 1
				readvar = 0
				lenscrape = len(cfg.lengths_output[0][5]) 
				lenthump = len(cfg.lengths_output[1][5])
				lenbuzz = len(cfg.lengths_output[2][5])
				lensr = len(cfg.srtot[0])
				npoutput[9][0:lenscrape] = cfg.lengths_output[0][5]
				npoutput[10][0:lenscrape] = cfg.lengths_output[0][4]
				npoutput[11][0:lenthump] = cfg.lengths_output[1][5]
				npoutput[12][0:lenthump] = cfg.lengths_output[1][4]
				npoutput[13][0:lenbuzz]	 = cfg.lengths_output[2][5]
				npoutput[14][0:lenbuzz]  = cfg.lengths_output[2][4]
				npoutput[15][0:lensr]  = cfg.srtot[5]
				npoutput[16][0:lensr] = cfg.srtot[2]
				npoutput[17][0:lensr] = cfg.srtot[1]
				npoutput[18][0] = animal_info[trialindex][9]

				# this will be the header for the csv file
				durations_output_header = ["tape-video", "complete?", "individual", "treatment", "rank", "date", "temperature (C)", "weight", "ct_width", "scrape_pos", "scrape_dur", "thump_pos", "thump_dur", "buzz_pos", "buzz_dur", "srate_pos", "srate_dur", "srate_num", "comments"]
				# this zips our previous mess of an array so that we can have columns of unequal length	
				zipoutput = list(it.izip_longest(*npoutput, fillvalue=''))
				
				# this bit writes the file. 
				#fl = open(annotation_folder +"/"+ individual + "/" + trial + "/" + trial +"_" + "duration_data" + "_" + timestamp + '.csv', 'w')
				#writer = csv.writer(fl)
				#writer.writerow(durations_output_header)
				#writer.writerows(zipoutput) 
				#fl.close()   

				# handle frequency domain if the user requests it
				# note that this is all commented out, just laid out in pseudocode for now
				
		
					#run importwav
					#wavpath = wav_folder + "/" + individual + "/" + trial + ".wav"
					#print wavpath
					#vib.importwav(wavpath)
					# for each feature type (scrape, thump, buzz):
						# for each feature
							# run featurefinder
							# vib.featurefinder(cfg.lengths_output, "buzz", 1, cfg.wavdata, .25)
							#vib.getfreq(cfg.feature[1][1], cfg.rate, 10000000)
							# run getpeaks
							# vib.getpeaks(cfg.fft_dat[0], cfg.fft_dat[1], .02, trial + " buzz 2")
							# show peaks, get confirmation that they're ok
							# threshold (to add later)
							# append buzz, thump or scrape array with peak, threshold info 
			
					# get pertinent info. (treatment, temp, date, weight, ct length) from other .csv file
					# *** export summary stats into one export file with summary info
					# *** export file that has x, y for time domain
					# *** export file that has x, y for frequency domain
			
