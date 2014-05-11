#!/usr/bin/python

import pickle
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

def plotSpectru(y,Fs):
	n = len(y) # lungime semnal
	k = arange(n)
	T = n/Fs
	#two-sided frequency range
	frq = k/T
	# one side frequency range	
	frq = frq[range(n/2)] 
	# fft computation and normalization
	Y = fft(y)/n # fft computing and normalization
	Y = Y[range(n/2)]
	global spectraloutput
	spectraloutput = [frq,Y]
	return spectraloutput

def peakfinding():
	# first step of getting peaks
	peaks_obj = Data(frq, abs(Y), smoothness=11)
	# second part of getting peaks
	peaks_obj.get_peaks(method='slope')
	# pull data out of peaks data object for filtering
	peaks_obj.peaks["peaks"]
	peaks = peaks_obj.peaks["peaks"]
	peaks_obj.plot()
	show()
	peaksnp = np.zeros((2, len(peaks[0])))
	peaksnp[0] = peaks[0]
	peaksnp[1] = peaks[1] 
	maxpeaks = max(peaks[1])

	# filtering function: removes peaks that are shorter than 10% of the max peak
	filteredpeaksnp = []
	cutoff = .05
	filtered_peaks = ma.masked_less(peaksnp[1], (cutoff * maxpeaks))	
	indeces = ma.nonzero(filtered_peaks)
	indeces = indeces[0]
	final_peaks = np.zeros((3,len(indeces)))

	i = 0
	while i < len(indeces):
		final_peaks [0,i] = frq[i]
		final_peaks[1,i] = peaksnp[1, indeces[i]]
		final_peaks[2,i] = peaksnp[0, indeces[i]]
		i = i + 1
 
def plotstuff(frq, Y):
	plot(frq,abs(Y),'r') # plotting the spectrum
	#plot(final_peaks[0],final_peaks[1])
	pylab.xlim([0,500])
	pylab.ylim([0, 500])
	xlabel('Freq (Hz)')
	ylabel('|Y(freq)|')
	show()	
	
def old_importwav():
	Fs= 48000.0  # sampling rate
	rate,data=read('/home/eebrandt/projects/temp_trials/test/buzz.wav')
	y=data
	# gets total length of "y" array, which amounts to the number of samples in the clip
	lungime=len(y)
	#figures out  total time fo clip(in seconds).  Does this by dividing the total number of y-values by the sample rate.
	# it's important for the frame rate to be a float (with decimal point), otherwise it will give a divide by 0 error as int.
	timp=len(y)/48000.
	print timp
	# generates equally-spaced units along the time domain, starting with zero and ending with the previously generated total time
	t=linspace(0,timp,len(y))
	p1 = plt.plot(t,y)
	plt.xlim(0, timp)
	#print t
	global export
	export = [t,y]
	return export
	#show()
def importwav(Fs, wavpath):
	from scipy import signal
	import matplotlib.pyplot as plt
	from pylab import plot, show, title, xlabel, ylabel, subplot, savefig
	from scipy.io.wavfile import read,write
	from numpy import sin, linspace, pi
	rate,data=read(wavpath)
	y=data
	# gets total length of "y" array, which amounts to the number of samples in the clip
	lungime=len(y)
	#figures out  total time fo clip(in seconds).  Does this by dividing the total number of y-values by the sample rate.
	# it's important for the frame rate to be a float (with decimal point), otherwise it will give a divide by 0 error as int.
	timp=len(y)/48000.
	#print timp
	# generates equally-spaced units along the time domain, starting with zero and ending with the previously generated total time
	t=linspace(0,timp,len(y))
	#p1 = plt.plot(t,y)
	#plt.xlim(0, timp)
	#show()	
	global wavdata
	wavdata = [t, y]
	return wavdata

#subplot(3,1,1)
#plot(t,y)
#xlabel('Time')
#ylabel('Amplitude')
#subplot(3,1,2)
Fs = 48000
importwav(Fs, "/home/eebrandt/projects/temp_trials/test/5-41-buzz1.wav")
plotSpectru(wavdata[1],Fs)
plotstuff(spectraloutput[0], spectraloutput[1])
