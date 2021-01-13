library (phonTools)
# data (sound)                 ## use the example 'sound' object provided
sound = loadsound("./data/Mono XC386310 - Northern Goshawk - Accipiter gentilis.wav")        ## or run this line to use you own sound


# Northern Goshawk (Accipiter gentilis) Lunenburg Nova Scotia
par (mfrow = c(3,1), mar = c(4,4,1,1))
spectrogram (sound)
spectrogram (sound, color = 'alternate')
spectrogram (sound, color = FALSE)