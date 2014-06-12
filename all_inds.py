#!/usr/bin/python

"""
Erin Brandt - 31/5/2013
This function does several things.
Input: .csv files of all individuals' duration information
Process: (1) calculates average durations for each quartile of the song for each feature
(2) Calculates scrape rates for each quartile of the song and overall
Output: 1 csv file with information about each individual.  This only includes averages, so the source files should be retained
for more detailed analysis of individual features
"""

# for moving around in folders
import os
# for making file and folder dialog boxes and  boxes
import Tkinter, Tkconstants, tkFileDialog, tkMessageBox
# for getting the contents of a directory
from os import listdir
# for doing numerical calcuations and for making numpy arrays
import numpy as np
# for reading and writing csv files
import csv
# for getting current date and time for timestamp
import datetime

# gets a timestamp to identify data files
timestamp = datetime.datetime.now().strftime("%Y-%m-%d_%H:%M:%S")

# function to gather a list of the names of all csv files in the folder.
def find_csv_filenames(path_to_dir, suffix=".csv"):
	filenames = listdir(path_to_dir)
	return [ filename for filename in filenames if filename.endswith(suffix)]

# Asks user for data folder.  Should be the same as "annotation_folder" for overall_analysis.py
data_folder = tkFileDialog.askdirectory(initialdir= "/home/eebrandt/projects/temp_trials/male_only/data", title = "Choose the folder that contains data files.")

# Defines the header for the .csv file we're going to make
durations_output_header = ["tape_video", "complete", "individual", "treatment" , "rank", "date", "temperature", "weight", "ct_width", "scrape_q1", "scrape_q2", "scrape_q3", "scrape_q4", "scrape_avg", "thump_q1", "thump_q2", "thump_q3", "thump_q4", "thump_avg", "buzz_q1", "buzz_q2", "buzz_q3", "buzz_q4","buzz_avg", "srates_q1", "srates_q2", "srates_q3", "srates_q4", "srates_avg", "buzzfreq_q1","buzzfreq_q2","buzzfreq_q3","buzzfreq_q4","buzzfreq_avg", "comments"]

# Defines and opens a .csv file that we'll write our file to
fl = open(data_folder + "/" + "duration_summary" + "_" + timestamp + '.csv', 'w')
writer = csv.writer(fl)
# writes header to csv file
writer.writerow(durations_output_header)

# looks in each individual folder for trial folders
individuals =  os.listdir(data_folder)
for individual in individuals:
	# make sure each "individual" is a folder
	if os.path.isdir(data_folder + "/" + individual):
		trials = os.listdir(data_folder + "/" + individual)
		# loop to go through each individual
		for trial in trials:
			# defines the folders that we'll be looking in for csvs.
			trial_folder = data_folder + "/" + individual + "/" + trial
			# makes sure each trial is a folder
			if os.path.isdir(trial_folder):
				# looks for all the csvs. in a trial folder
				csvs = find_csv_filenames(trial_folder, suffix = ".csv")
				# checks to make sure there is at least one csv file in the folder.  If not, moves to next one and alerts the user to this
				if not csvs:
					tkMessageBox.showinfo(
					"Missing .csv file",
					"Duration file for " + trial + " was not found.  Moving to next one")
					print "Duration file for " + trial + " was not found.  Moving to next one"
					break
				creation_time = []
				# gets creation time for each file, so we only use the most recent one				
				for csv in csvs:
					creation_time.append(os.path.getmtime(trial_folder + "/" +  csv))
				recent_time = max(creation_time)
				max_index = creation_time.index(recent_time)
				# csv file that we'll actually load
				use_file = csvs[max_index]
				# arguments that we'll use to open the folder.  Specifically: we're setting the data type for each column and naming each column.
				kwargs = dict(delimiter=",",
               				dtype= "S10, S10, S10, S10, S10, S10, f20, f20, f20, f20, f20, f20, f20, f20, f20, f20, f20, f20, f20, f20, S200",
              				names= "video, complete, individual, treatment, rank, date, temperature, weight, ct_width, scrape_pos, scrape_dur, thump_pos, thump_dur, buzz_pos, buzz_dur, srate_pos, srate_dur, srate_num, buzz_freq, buzz_peak, comments",
					skip_header=1)
				# load the data file once we know which is the proper one 
				data_file = np.genfromtxt(trial_folder + "/" + use_file, **kwargs)
				# list of feature types
				feature_type = ["scrape", "thump", "buzz"]
				# counts which feature we're on (could probably technically do this by finding index of feature_type)
				feature_counter = 0
				# gets overall averages for the entire song
				allmeanfeatures = []
				# this loop goes through each "feature type" (scrape, thump, and buzz) and figures out average duration of features for each quarter of the song plus and overall average
				
				for feature in feature_type:
					# holds average for the entire song
					average_count = []
					#holds durations for each quartile
					mean_quartile = []
					# keeps track of which quartile we're on
					quartile_count = 1
					
					while quartile_count < 5:
						# holds the mean for each quartile
						quartile_durs = []
						readvar = 0
						for item in data_file[feature + "_pos"]:
							# check to make sure that there is a number here (not nan) and within quartile range.
							if not np.isnan(item) and  (quartile_count * .25) > float(item) > ((quartile_count * .25) - .25):
								average_count.append(data_file[feature + "_dur"][readvar])
								quartile_durs.append(data_file[feature +"_dur"][readvar])
							readvar = readvar + 1
						# makes sure there's something in the quartile_durs array.  It will throw an error (but not abort the program) if this isn't included
						if quartile_durs:
							mean_quartile.append(np.mean(quartile_durs))
						else:
							mean_quartile.append(" ")
						quartile_count = quartile_count + 1
					# also makes sure there's something in average_count, so we don't get weird errors.  Note that we add empty string if it doesn't have a number so things don't get appended over necessary blanks.
					if average_count:
						mean_quartile.append(np.mean(average_count))
					else:
						mean_quartile.append(" ")
					allmeanfeatures.append(mean_quartile)
					feature_counter = feature_counter + 1	
				# this loop gets scrape rates for 1st, 2nd, 3rd, and 4th quartiles of the song.  It does this by creating a running total of time, and a running total of scrape number to get those averages (ie: it doesn't calculate each rate independently and then average them).
				# variable to hold running total of time (durations)
				overall_dur = 0.0
				# variable to hold running total of scrape counts
				overall_count = 0.0
				# this makes a list that will hold all of the rate data (each quartile plus an overall average).  It is a fixed size (5) so that if one of them is empty (as often happens with buzzes), subsequent rates won't get appended over intended empty slots.
				rates = [0.0] * 5
				# counts the number of quartiles we go through (4)
				quartile_count = 0
				# this loop actually gets the rates for each quartile
				while quartile_count < 5:
					readvar = 0.0
					sdur = 0.0
					scount = 0.0
					for item in data_file["srate_pos"]:
						# this checks each "srate_pos" item to see if it's within the quartile range and also exists (not nan). If it's nan, it will throw an error but not break the program.  If everything looks ok, it adds to the scount and sdur running totals
						if not np.isnan(item) and  (quartile_count * .25) > float(item) > ((quartile_count * .25) - .25):
							sdur = sdur + data_file["srate_dur"][readvar]
							scount = scount + data_file["srate_num"][readvar]
						readvar = readvar + 1
					# this calculates the average.  First it makes sure neither scount or sdur are zero (to avoid division by zero errors) and then figures out the averages, which are then added to the rates list. Note that the overall_count and overall_dur variables get appended throughout all quartiles to give an overall average.
					savg = 0.0
					if float(scount) != 0 and float(sdur) != 0:
						savg = round(float(scount)/float(sdur),7)
						rates[quartile_count -1] = savg
						overall_dur = float(overall_dur + sdur)
						overall_count = float(overall_count + scount)
					else:
						rates[quartile_count -1] = ""

					quartile_count = quartile_count + 1
					
				#calculates the overall average, again checking for /0 errors and
				if overall_count != 0 and overall_dur != 0:
					overall_avg = overall_count/overall_dur
					rates[4] = round(overall_avg, 7)
				else:
					rates[4] = ""

				# handles buzz stuff.  for now, this just handles frequency, and not peak data.  Eventually, we'll have the peak based on real values gotten from the vibrometer
				# holds average for the entire song
				#freq_count = []
				#holds frequencies for each quartile
				mean_quartile = []
				# keeps track of which quartile we're on
				quartile_count = 1
				quartile_freq = [0.0] * 5
				allcount = []
				
				while quartile_count < 5:
					# holds the mean for each quartile	
					readvar = 0
					freq_count = []
					for item in data_file["buzz_pos"]:
						# check to make sure that there is a number here (not nan) and within quartile range.
						if not np.isnan(item) and  (quartile_count * .25) > float(item) > ((quartile_count * .25) - .25) and (data_file["buzz_peak"][readvar]) > (max(data_file["buzz_peak"]) * .10):
							print quartile_count					
							freq_count.append(data_file["buzz_freq"][readvar])
							allcount.append(data_file["buzz_freq"][readvar])
							print quartile_freq
							readvar = readvar + 1
						if freq_count:
							quartile_freq[quartile_count - 1] = np.mean(freq_count)
						else:
							quartile_freq[quartile_count - 1] = " "
					
				
					# makes sure there's something in the quartile_freq array.  It will throw an error (but not abort the program) if this isn't included
					#print quartile_freq
					#if quartile_freq:
					#	mean_quartile.append(np.mean(quartile_freq))
					#else:
					#	mean_quartile.append(" ")
					quartile_count = quartile_count + 1
				
				# also makes sure there's something in freq_count, so we don't get weird errors.  Note that we add empty string if it doesn't have a number so things don't get appended over necessary blanks.
				if allcount:
					quartile_freq[4] = np.mean(allcount)
				else:
					quartile_freq[4] = (" ")
				#mean_quartile.append(mean_quartile)
			
				
				
				
				# these two conditionals put empty string instead of nan if weight or ct is missing.  Easier for later import								
				if data_file["weight"][0] == False:
					weight = ""
				else:
					weight = data_file["weight"][0]

				if data_file["ct_width"][0] == False:
					ct = ""
				else:
					ct = data_file["ct_width"][0]
				#print mean_buzz_quartile
				# defines the row that we're going to write to the csv.  
				row = [data_file["video"][0], data_file["complete"][0], data_file["individual"][0], data_file["treatment"][0] , data_file["rank"][0], data_file["date"][0], data_file["temperature"][0], weight, ct, allmeanfeatures[0][0], allmeanfeatures[0][1], allmeanfeatures[0][2], allmeanfeatures[0][3], allmeanfeatures[0][4], allmeanfeatures[1][0], allmeanfeatures[1][1], allmeanfeatures[1][2], allmeanfeatures[1][3], allmeanfeatures[1][4], allmeanfeatures[2][0], allmeanfeatures[2][1], allmeanfeatures[2][2], allmeanfeatures[2][3], allmeanfeatures[2][4], rates[0], rates[1], rates[2], rates[3], rates[4], quartile_freq[0], quartile_freq[1], quartile_freq[2], quartile_freq[3], quartile_freq[4], data_file["comments"]]
				# writes the row, for each trial
				writer.writerow(row) 
# closes the csv writer
fl.close() 

#
