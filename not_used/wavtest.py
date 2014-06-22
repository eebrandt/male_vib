#!/usr/bin/python
import pyaudio
import wave
import sys
import maleviban as vib
import config as cfg
import scikits.audiolab

vib.importwav(48000, "/home/eebrandt/projects/temp_trials/test/5-41.wav")

scikits.audiolab.play(cfg.wavdata[1], fs=44100)

#song = pyglet.media.load(cfg.wavdata)
#song.play()
#pyglet.app.run()
