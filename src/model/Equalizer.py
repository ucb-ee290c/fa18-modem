#!/usr/bin/env python

import numpy as np
from IEEE80211 import IEEE80211

class TestVectors:
    def __init__(self):
        tvClean1 = self.buildPacket(1, [1+0j]*64)
        tvClean2 = self.buildPacket(2, [1+0j]*64)
        tvHalfGain = self.buildPacket(2, [0.5+0j]*64)
        tvRotate = self.buildPacket(2, [np.exp(2*np.pi*1j*1)]*64)
        tvFade = self.buildPacket(2, self.fadingChannel(20, 0.25+0.25j))
        self.tvList = [tvClean1, tvClean2, tvHalfGain, tvRotate, tvFade]
        for tv in self.tvList:
            # Remove guard bands and DC carrier
            tv[0][:, 27:38] = 0+0j
            tv[1][:, 27:38] = 0+0j
            tv[0][:, 0] = 0+0j
            tv[1][:, 0] = 0+0j

    def buildPacket(self, nSymbols, impairment):
        data = np.random.randint(0, 2, (64, nSymbols))
        data = 1 - 2*data  # convert to {-1,1}
        LTF = np.expand_dims(IEEE80211.ltfFreq, 1)
        reference = np.vstack([LTF.T, LTF.T, data.T])
        print("reference {}".format(reference))
        chanIQ = impairment * reference
        print("Impaired IQ {}".format(chanIQ))
        return chanIQ, reference

    def fadingChannel(self, carrier, gain):
        chan = [1+0j]*64
        chan[carrier] *= gain
        return chan


class Equalizer:
    def __init__(self, mu=0.25, pilots=[5, 21, 43, 59], nSubcarriers=64):
        self.LTF = IEEE80211.ltfFreq
        self.ltf = IEEE80211.ltf64
        self.mu = mu
        self.pilots = pilots
        self.nSubcarriers = nSubcarriers
        self.channelInverse = [1+0j]*self.nSubcarriers
        self.prevLTF = None
        self.isLTF = False

    def push(self, iq, newPkt=False):
        """
        Push in a symbol of IQ, specifying when each frame starts
        Expects frequency-domain data
        """
        if newPkt:
            self.prevLTF = iq
            return None
        if self.prevLTF is not None:
            estimate = 0.5 * (self.prevLTF + iq) * self.LTF
            estimate[0] = estimate[1]  # DC offset
            estimate[27:38] = 1+0j  # guard band == don't care
            self.channelInverse = 1/estimate
            self.prevLTF = None
            return None
        return self.channelInverse * iq


def main():
    eq = Equalizer()
    tvs = TestVectors().tvList
    for tv in tvs:
        chan, ref = tv
        # Push LTF
        rv = eq.push(chan[0], True)
        assert rv is None
        rv = eq.push(chan[1])
        assert rv is None
        # Push body
        eqIQ = []
        for iq in chan[2:]:
            eqIQ.append(eq.push(iq))
        eqIQ = np.array(eqIQ)
        # Check output
        assert len(eqIQ) == len(ref)-2
        print(eqIQ.shape)
        print(ref.shape)
        for isymb in range(len(eqIQ)):
            assert np.linalg.norm(eqIQ[isymb, :] - ref[2+isymb, :]) < 0.0001, "eq {}\n\nref {}".format(eqIQ[isymb, :], ref[2+isymb, :])
        print("Passed test vector...")
    print("Passed all test vectors!")

if __name__ == "__main__":
    main()
