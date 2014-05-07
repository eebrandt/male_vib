#!/usr/bin/python

import pickle
from pypeaks import Data, Intervals
import pylab
import numpy.ma as ma
import numpy as np
from scipy import signal
from pylab import plot, show, title, xlabel, ylabel, subplot, savefig
from scipy import fft, arange, ifft
from numpy import sin, linspace, pi
from scipy.io.wavfile import read,write

def plotSpectru(y,Fs):
	n = len(y) # lungime semnal
	#print n
	k = arange(n)
	T = n/Fs
	#print k
	print n
	print Fs
	#print T
	print n/Fs
	frq = k/T # two sides frequency range
	frq = frq[range(n/2)] # one side frequency range

	Y = fft(y)/n # fft computing and normalization
	Y = Y[range(n/2)]

	

	peaks_obj = Data(frq, abs(Y), smoothness=11)
	peaks_obj.get_peaks(method='slope')
 	subplot(3,1,3)
	peaks_obj.plot(new_fig = False)
	# pull data out of peaks data object for filtering
	peaks_obj.peaks["peaks"]
	peaks = peaks_obj.peaks["peaks"]
	peaksnp = np.zeros((2, len(peaks[0])))
	peaksnp[0] = peaks[0]
	peaksnp[1] = peaks[1] 
	maxpeaks = max(peaks[1])

	
	
	# filtering function: removes peaks that are shorter than 10% of the max peak
	filteredpeaksnp = []
	cutoff = .05
	#print peaksnp[1]
	filtered_peaks = ma.masked_less(peaksnp[1], (cutoff * maxpeaks))
	#print filtered_peaks
	#filtered_peaks = filtered_peaks[~filtered_peaks.mask]

	#print filtered_peaks
	#print bools	
	indeces = ma.nonzero(filtered_peaks)
	indeces = indeces[0]
	#print indeces
	final_peaks = np.zeros((3,len(indeces)))
	#print final_peaks
	#print len(indeces)

	i = 0
	while i < len(indeces):
		#print i
		#print Y
		#print indeces[item]
		#final_peaks[0,i] = abs(Y[i] * frq[i])
		final_peaks [0,i] = frq[i]
		final_peaks[1,i] = peaksnp[1, indeces[i]]
		final_peaks[2,i] = peaksnp[0, indeces[i]]
		i = i + 1
 	print final_peaks
	
	plot(frq,abs(Y),'r') # plotting the spectrum
	plot(final_peaks[0],final_peaks[1])
	pylab.xlim([0,500])
	pylab.ylim([0, 500])
	xlabel('Freq (Hz)')
	ylabel('|Y(freq)|')

Fs= 48000.0  # sampling rate
rate,data=read('/home/eebrandt/projects/temp_trials/test/buzz.wav')
y=data

lungime=len(y)

timp=len(y)/48000.

t=linspace(0,timp,len(y))

subplot(3,1,1)
plot(t,y)
xlabel('Time')
ylabel('Amplitude')
subplot(3,1,2)
plotSpectru(data,Fs)

show()

