#!/usr/bin/python
import numpy as np
from pylab import*
import matplotlib.pyplot as plt
import ctypes
import config as cfg


def importanns(wavpath):
	"""
    	This function opens a file containing duration information for vibratory song components.  It takes a path name (string), and outputs a numpy array that contains the following columns: component labels, component start times, component end times, component midpoints(useful for plotting components against total song length), length of component, percent of the total song at which the midpoint of a component occurs.
	"""
	# imports the label file with the specified path/file name
	loadarray = np.array(np.loadtxt(open(wavpath),dtype = "string", delimiter = "\t,", skiprows = 0))

	#variable to count through loop
	readvar = 0
	# defines three arrays, start time and end time for a given vibratory feature
	startarray = []
	endarray = []
	labelarray = []

	# loops through loadarray, converting starts and ents to floats. We end up with startarray (starting times), endarray(ending times), and labelarray (labels (s, t, b, and r)
	while readvar < loadarray.shape[0]:
		# splits a given line of loadarray into a separate string
		readstring = loadarray[readvar].split()	
		# converts all numbers to floats, appends to starting and ending arrays
		startarray.append(float(readstring[0]))
		endarray.append(float(readstring[1]))
		labelarray.append(readstring[2])
		readvar = readvar + 1
	songlength = (max(startarray) - min(startarray))
	# now we need to make three separate arrays, one each for scrapes, thumps, buzzes, and rate regions

	# this array holds the names of all the strings we're interested in iterating (in this case, scrape, thump, buzz, rate)
	strings = ["s", "t", "b", "r"]
	# variable for looping trhough each string we want to test
	stringloop = 0
	
	# sets the output variable as global so it can be returned
	#global lengths_output
	# defines the output variable
	

	# loops through each string we want to test
	while stringloop < len(strings):
		readvar = 0
		divlabels = []
		divstart = []
		divlength = []
		divend = []
		divmid = []
		divpercent = []
		songstart = min(startarray)
		songend = max(endarray)
		songlength = songend - songstart
		# loops through entire array
		while readvar < len(labelarray):
			# asks whether a given element matches our string of interest.  If it does, adds information to the relevant arrays
			if strings[stringloop] in labelarray[readvar]:	
				divlabels.append(labelarray[readvar])
				start = (startarray[readvar])
				divstart.append(start)
				end = (endarray[readvar])
				divend.append(end)
				length = (end - start)
				divlength.append(length)
				mid = (length / 2) + songstart + (start - songstart)
				divmid.append(mid)
				mid_to_start = mid - start
				mid_to_end = songend - mid
				percent = (mid - songstart)/songlength			
				divpercent.append(percent)
			readvar = readvar +1
		# combines all of our separate arrays into one variable
		divarray = np.array([divlabels, divstart, divend, divmid, divlength, divpercent])
		# sets our current divnparray to the relevant portion of the output variable	
		cfg.lengths_output[stringloop] = divarray
		# resets all our arrays so we don't just keep appending them
		stringloop = stringloop + 1
	# returns our output when we're all done
	#print cfg.lengths_output	
	return cfg.lengths_output



def plotlengths (scrapedur, thumpdur, buzzdur, plot_durs_title):
	""" 
	This function takes three arrays that contain duration info for scrapes, thumps, and buzzes.  It generates a scatterplot that plots the duration of a component over the total length of the song (length given in percent length).
	"""
# make plots for an individual that compare where the signal occurs in time to its length.  This is a good quick visual check to make sure everything's working well.  It also gives a good indication of how song elements are distributed temporally.
	import matplotlib.pyplot as plt
	import pylab

	fig = plt.figure(figsize=(5, 4))
	ax = fig.add_subplot(1,1,1)
	# we're just looking at scrapes, thumps, and buzzes here.  Keep in mind that the x, y axes are backwards.
	p1 = ax.plot(scrapedur[5], scrapedur[4], color = "red", label = "scrapes", marker='o', linestyle = 'None')
	p2 = ax.plot(thumpdur[5], thumpdur[4], color = "green", label = "thumps", marker='o', linestyle = 'None')
	p3 = ax.plot(buzzdur[5], buzzdur[4], color = "blue", label = "buzzes", marker='o', linestyle = 'None')
	# sets labels on plot axes
	ax.set_xlabel('% of song at which feature begins')
	ax.set_ylabel('Length of feature (s)')
	# sets up legend
	plt.legend(loc='upper left', shadow=True, numpoints = 1)
	#ax.legend([p2, p1, p3], ["scrapes", "thumps", "buzzes"], 2, numpoints = 1)
	# sets axis limits
	plt.xlim(0, 1)
	plt.ylim(0, 10)
	plt.title(plot_durs_title)
	# show plot
	plt.show()

def rates(readarray):
	labelarray = []	
	countarray=[]
	lengtharray = []
	ratearray = []
	midarray = []
	percentarray = []
	readvar = 0
	while readvar < readarray.shape[1]:
		r_count_string = readarray[0, readvar]
		labelarray.append( r_count_string.split('_')[0])
		counts = float(r_count_string.split('_')[1])
		countarray.append(counts)
		# length of comonent
		lengtharray.append(float(readarray[4, readvar]))
		# counts/second
		rate = counts/float(readarray[4, readvar])
		ratearray.append(float(rate))
		# midpoint of component
		mid = readarray[3, readvar]
		midarray.append(float(mid))
		# percent of song at which midpoint of component happens
		percent = readarray[5, readvar]
		percentarray.append(float(percent))
		readvar = readvar + 1
	#global srtot
	# makes the final array that gets returned to whoever called the function
	cfg.srtot = [labelarray, countarray, lengtharray, ratearray, midarray, percentarray]
	ndsrtot = np.array(cfg.srtot)				
	return cfg.srtot

def plot_rates(durarray, plot_rates_title):
	import matplotlib.pyplot as plt
	# makes a figure of the rate data
	fig = plt.figure(figsize=(5, 4))
	ax = fig.add_subplot(1,1,1)
	ax.set_xlabel('% of song at which feature begins')
	ax.set_ylabel("Scrape Rate (scrapes/second)")
	x = durarray[5]
	y = durarray[3]
	ratefit = np.polyfit(x, y, 1)
	yfit = np.polyval(ratefit, x)
	p1 = plt.plot(x, y, color = "red", marker='o', linestyle = 'None', label = 'scrape rates')
	p2 = plt.plot(x, yfit, label = 'linear fit line')
	plt.xlim(0, 1)
	plt.ylim(min(y) - (.05 * min(y)), max(y) + (.10 * max(y)))
	plt.legend(loc='upper center', shadow=True, numpoints = 1)
	plt.title(plot_rates_title)
	#plt.legend([p1, p2], ["scrape rate", "linear fit line"], 2, scatterpoints = 1)
	plt.show()
		
def importwav(wavpath):
	from scipy import signal
	import matplotlib.pyplot as plt
	from pylab import plot, show, title, xlabel, ylabel, subplot, savefig
	from scipy.io.wavfile import read,write
	from numpy import sin, linspace, pi

	rate,data=read(wavpath)
	cfg.rate = float(rate)
	cfg.y=data
	# gets total length of "y" array, which amounts to the total number of samples in the clip
	lungime=len(cfg.y)
	#figures out  total time of feature(in seconds).  Does this by dividing the total number of y-values by the sample rate.
	# it's important for the frame rate to be a float (with decimal point), otherwise it will give a divide by 0 error as int.
	timp=len(cfg.y)/48000.
	# generates equally-spaced units along the time domain, starting with zero and ending with the previously generated total time
	cfg.t=linspace(0,timp,len(cfg.y))
	#p1 = plt.plot(t,y)
	#plt.xlim(0, timp)
	#show()	
	cfg.wavdata = [cfg.t, cfg.y, cfg.rate]
		
	return cfg.wavdata

def getfreq(y, Fs, normal):
	import scipy
	import pylab
	import math
	from pylab import plot, show, title, xlabel, ylabel, subplot, savefig
	from scipy import fft, arange, ifft

	# number of samples
	n = len(y) 
	k = arange(n)
	T = n/Fs
	#two-sided frequency range
	cfg.frq = k/T
	# one side frequency range	
	cfg.frq = cfg.frq[range(n/2)] 
	
	# fft computation and normalization
	# take the fft
	cfg.Y = fft(y)
	# extract the real component of the fft array only
	cfg.Y = cfg.Y.real
	# don't know what this does
	cfg.Y = cfg.Y[range(n/2)]
	# normalizes it 
	if normal == -1:
		normal = max(Y)
		#print max(Y)	
	cfg.Y = (cfg.Y/normal) * 100
	cfg.Y = abs(cfg.Y)
	
	
	p1 = plt.plot(cfg.frq,abs(cfg.Y),'r')
	pylab.xscale('log')
	pylab.xlim([0,4000])
	pylab.ylim([0,max(abs(cfg.Y))])
	#show()	
	cfg.fft_dat = [cfg.frq, cfg.Y]
	return cfg.fft_dat

def getpeaks(frq, Y, cutoff, plot_title):
	from pypeaks import Data, Intervals
	import pylab
	import numpy.ma as ma
	import numpy as np
	from scipy import signal
	import matplotlib.pyplot as plt
	from pylab import plot, show, title, xlabel, ylabel, subplot, savefig
	from scipy import fft, arange, ifft
	from numpy import sin, linspace, pi
	from scipy.io.wavfile import read,write
	# first step of getting peaks
	peaks_obj = Data(frq, Y, smoothness=20)
	#second part of getting peaks
	peaks_obj.get_peaks(method='slope')
	#pull data out of peaks data object for filtering
	#plt.plot(peaks_obj.peaks["peaks"][0], peaks_obj.peaks["peaks"][1], 'rD', ms=10 )
	#plt.show()

	peaks = peaks_obj.peaks["peaks"]
	#print peaks

	#peakplot = peaks_obj.plot()
	
	#p3 = plt.plot(peaks)
	#show()
	peaksnp = np.zeros((2, len(peaks[0])))
	peaksnp[0] = peaks[0]
	peaksnp[1] = peaks[1] 
	maxpeaks = max(peaks_obj.peaks["peaks"][1])
	#print maxpeaks
	#plt.plot(peaks[1], peaks[0], linestyle = 'None', marker='o' )
	#plt.xlim(0,500)
	#plt.show()
	#plt.plot(peaksnp[1], peaksnp[0], linestyle = 'None', marker='o')
	#plt.show()

	# filtering function: removes peaks that are shorter than the cutoff specified in function
	filteredypeaks = []
	filteredxpeaks = []
	filter_thresh = cutoff * maxpeaks
	readvar = 0
	while readvar < len(peaks_obj.peaks["peaks"][1]):
		if peaks_obj.peaks["peaks"][1][readvar] > filter_thresh:
			filteredxpeaks.append(peaks_obj.peaks["peaks"][0][readvar])
			filteredypeaks.append(peaks_obj.peaks["peaks"][1][readvar])
		readvar = readvar +1
	filteredpeaks = [filteredxpeaks, filteredypeaks]

	# now we need to make a plot comparing the frequency chart to our found peaks
	global filter1_peaks
	
	filter1_peaks = np.array(filteredpeaks)
	filter1_peaksy = []
	absY = abs(Y)
	# Y
	#print absY
	readvar = 0
	#print len(filter1_peaks[1])
	while readvar < len(filter1_peaks[1]):
		ypeak = np.searchsorted(frq,[filter1_peaks[0][readvar,]],side='left')[0]
		filter1_peaksy.append(absY[ypeak])
		readvar = readvar + 1

	p1 = plt.plot(frq,abs(Y),'r') # plotting the spectrum
	#abs(Y)
	p2 = plt.plot(filter1_peaks[0], filter1_peaksy, linestyle = "none", marker = "o", color = "black")
	pylab.xlim([0,500])
	pylab.ylim([0,400])
	xlabel('Freq (Hz)')
	ylabel('|Y(freq)|')
	#plt.show()
	#print filter1_peaks
	
	# second filtering step.  Judders peaks back and forth along the x-axis of the frequency plot until they reach the true local max (y)
	rangeleft_arr = []
	rangeright_arr = []
	readvar = 0
	#print filter1_peaks
	while readvar < len(filter1_peaks[0]):
		# figure out the x-distance to the next closest peak, then 
		if readvar == 0:
			xdist =  abs(filter1_peaks[0][readvar +1] - filter1_peaks[0][readvar])
			 
		elif readvar == len(filter1_peaks[0]) - 1:
			xdist = abs(filter1_peaks[0][readvar - 1] - filter1_peaks[0][readvar])
		else:
			distright = abs(filter1_peaks[0][readvar +1] - filter1_peaks[0][readvar])
			distleft = abs(filter1_peaks[0][readvar - 1] - filter1_peaks[0][readvar])
			xdist = min(distright, distleft)

		xdist2 = xdist/2
		rangeleft = max(0, filter1_peaks[0][readvar] - xdist2)
		rangeright = min(filter1_peaks[0][readvar] + xdist2, max(frq))
		rangeright_arr.append(rangeright)
		rangeleft_arr.append(rangeleft)
		readvar = readvar + 1
	finalpeaksx = []
	finalpeaksy = []

	readvar = 0
	while readvar < len(rangeright_arr):
		xmin = np.searchsorted(frq,[rangeleft_arr[readvar]],side='left')[0]
		xmax = np.searchsorted(frq,[rangeright_arr[readvar]],side ='right')[0]
		finalpeaky = max(abs(Y)[xmin:xmax])
		indexy = np.where(abs(Y)==finalpeaky)
		finalpeakx = frq[indexy]
		
		finalpeaksy.append(finalpeaky)
		finalpeaksx.append(finalpeakx)
		#print finalpeaksy
		maxarray =  max([frq[xmin:xmax]])
		readvar = readvar + 1
	maxpeak = round(max(finalpeaksx),0)
	maxpeakstr = str(maxpeak) + " Hz"
	p3 = plt.plot(finalpeaksx, finalpeaksy, linestyle = "none", marker = "o", color = "green")
	plt.title(plot_title + " - max peak at: " + maxpeakstr)
	plt.show()
	final_peaks = []
	final_peaks = [finalpeaksx, finalpeaksy]
	return final_peaks

def featurefinder(lengths_output, featuretypestr, featureindex, wavdata, crop):
	featuretype = cfg.featurekey[featuretypestr]

	start = float(lengths_output[featuretype][1][featureindex])
	end = float(lengths_output[featuretype][2][featureindex])
	closestbegin =  np.searchsorted(wavdata[0],[start,],side='left')[0]
	closestend = np.searchsorted(wavdata[0],[end,],side = 'right')[0]
	cropamount = int(float(closestend - closestbegin) * crop)
	closestbegin_cr = closestbegin + cropamount
	closestend_cr = closestend - cropamount
	

	feature_whole = [wavdata[0][closestbegin:closestend], wavdata[1][closestbegin:closestend]]
	feature_buzz = [wavdata[0][closestbegin_cr:closestend_cr], wavdata[1][closestbegin_cr:closestend_cr]]
	cfg.feature = [feature_whole, feature_buzz]
	#print cfg.feature
	return cfg.feature	

#wavpath = "/home/eebrandt/projects/temp_trials/test/buzz.wav"
#importwav(wavpath)
#featurefinder(lengths_output[2][3],wavdata, .25)
#getfreq(wavdata[1], cfg.rate, 10000000)
#cutoff = .05
#getpeaks(cfg.frq, cfg.Y, cutoff)	
