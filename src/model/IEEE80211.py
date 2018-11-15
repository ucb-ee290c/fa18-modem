##
# Sourced from https://github.com/grebe/ofdm/src/main/scala/ieee80211/IEEE80211.scala
#

import numpy as np


zero = 0.+0.j
one  = 1.+0.j
pp   = 1.+1.j
pm   = 1.-1.j
mp   = -1.+1.j
mm   = -1.-1.j


class IEEE80211:
    # indexed from bin -26 -> bin 26
    stfFreq = np.array([
            zero, zero, zero, zero, mm,   zero, zero, zero,
            mm,   zero, zero, zero, pp,   zero, zero, zero,
            pp,   zero, zero, zero, pp,   zero, zero, zero,
            pp,   zero, zero, zero, zero, zero, zero, zero,
            zero, zero, zero, zero, zero, zero, zero, zero,
            pp,   zero, zero, zero, mm,   zero, zero, zero,
            pp,   zero, zero, zero, mm,   zero, zero, zero,
            mm,   zero, zero, zero, pp,   zero, zero, zero
    ]) * np.sqrt(13.0 / 6.0)
    stf64 = np.fft.ifft(stfFreq)

    stf = np.concatenate([stf64, stf64, stf64])[0:160]

    ltfFreq = np.array([
            zero,
            one, -one, -one,  one,  one, -one,  one, -one,
            one, -one, -one, -one, -one, -one, one,  one,
            -one, -one,  one, -one,  one, -one,  one,  one,
            one,  one,

            zero, zero, zero, zero, zero,
            zero, zero, zero, zero, zero, zero,

            one, one,
            -one, -one, one, one, -one, one, -one, one,
            one, one, one, one, one, -one, -one, one,
            one, -one, one, -one, one, one, one, one
    ])

    ltf64 = np.fft.ifft(ltfFreq)

    ltf = np.concatenate([ltf64, ltf64, ltf64])[32:160+32]

    def __init__(self):
        pass

    def addCFO(self, iq, cfo=0.0, sampleRate=20.0e6):
        coeff = 2 * np.pi * cfo / sampleRate
        return map(lambda idx, samp: (np.cos(coeff * idx) + np.sin(coeff * idx)*1j) * samp, enumerate(iq))

    def main(self, args):
        print(self.stf64)
        print("Length is {}".format(self.stf64.size))

        print("LTF is {}".format(self.ltf64))

if __name__ == "__main__":
    IEEE80211().main([])
