library(phonTools)

# Load audio file
sound <- loadsound("Mono_XC386310__Northern_Goshawk__Accipiter_gentilis.wav")

# Make 3 spectrograms
par(mfrow = c(3,1), mar = c(4,4,1,1))
spectrogram(sound)
spectrogram(sound, color = 'alternate')
spectrogram(sound, color = FALSE)