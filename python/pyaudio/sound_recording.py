import pyaudio
import wave
import audioop

import numpy as np
import matplotlib.pyplot as plt


THRESHOLD_LEVEL = 0

CHUNK = 1024



CHANNELS = 2
RATE = 44100

WAVE_OUTPUT_FILENAME = "recorded.wav"

p = pyaudio.PyAudio()

# see http://portaudio.com/docs/v19-doxydocs/portaudio_8h.html#a4582d93c2c2e60e12be3d74c5fe00b96

# WIDTH = 2
# FORMAT = p.get_format_from_width(WIDTH)

FORMAT = pyaudio.paInt16
WIDTH = p.get_sample_size(FORMAT)

stream = p.open(format=FORMAT,
                channels=CHANNELS,
                rate=RATE,
                input=True,
                frames_per_buffer=CHUNK)

print("* recording")

wf = wave.open(WAVE_OUTPUT_FILENAME, 'wb')
wf.setnchannels(CHANNELS)
wf.setsampwidth(p.get_sample_size(FORMAT))
wf.setframerate(RATE)

try:
    while True:
        frames = []
        for i in range(0, int(RATE / CHUNK)):
            data = stream.read(CHUNK)
            frames.append(data)
        frame_data = b''.join(frames)
        wf.writeframes(frame_data)

        rms = audioop.rms(data, WIDTH)
        print("RMS ", rms)

except KeyboardInterrupt:
    pass


stream.stop_stream()
stream.close()
p.terminate()

wf.close()