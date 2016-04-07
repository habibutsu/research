from __future__ import print_function, division, unicode_literals

import wave
import numpy as np
# compatibility with Python 3

import matplotlib.pyplot as plt

def plot_data(num, data, ampl):
    plt.figure(1)
    a = plt.subplot(211)
    r = 2**16/2
    a.set_ylim([-r, r])
    a.set_xlabel('time [s]')
    a.set_ylabel('sample value [-]')
    x = np.arange(44100)/44100
    print(x)
    print(data)
    plt.plot(x, data)
    b = plt.subplot(212)
    b.set_xscale('log')
    b.set_xlabel('frequency [Hz]')
    b.set_ylabel('|amplitude|')
    plt.plot(abs(ampl))
    plt.savefig('sample-graph_%s.png' % num)

# Created input file with:
# mpg123  -w 20130509talk.wav 20130509talk.mp3
wr = wave.open('recorded.wav', 'r')
par = list(wr.getparams()) # Get the parameters from the input.
# This file is stereo, 2 bytes/sample, 44.1 kHz.
par[3] = 0 # The number of samples will be set by writeframes.

# Open the output file
ww = wave.open('recorded-filtered.wav', 'w')
ww.setparams(tuple(par)) # Use the same parameters as the input file.

lowpass = 21 # Remove lower frequencies.
highpass = 9000 # Remove higher frequencies.

sz = wr.getframerate() # Read and process 1 second at a time.
c = int(wr.getnframes()/sz) # whole file
for num in range(c):
    print('Processing {}/{} s'.format(num+1, c))
    da = np.fromstring(wr.readframes(sz), dtype=np.int16)
    left, right = da[0::2], da[1::2] # left and right channel
    lf, rf = np.fft.rfft(left), np.fft.rfft(right)

    plot_data(num, left, lf)

    lf[:lowpass], rf[:lowpass] = 0, 0 # low pass filter
    lf[55:66], rf[55:66] = 0, 0 # line noise
    lf[highpass:], rf[highpass:] = 0,0 # high pass filter

    nl, nr = np.fft.irfft(lf), np.fft.irfft(rf)
    ns = np.column_stack((nl,nr)).ravel().astype(np.int16)
    ns2 = ns * 2
    ww.writeframes(ns2.tostring())
# Close the files.
wr.close()
ww.close()