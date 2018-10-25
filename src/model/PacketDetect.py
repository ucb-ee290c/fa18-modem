#!/usr/bin/env python


class PacketDetect:
    sPkt=0
    sNoPkt=1
    def __init__(self, powerWindow, powerThresh, doCorrThresh, corrThresh,
            corrWindow, corrStride):
        self.powerWindow = powerWindow
        self.powerThresh = powerThresh
        self.doCorrThresh = doCorrThresh
        self.corrThresh = corrThresh
        self.corrWindow = corrWindow
        self.corrStride = corrStride
        self.buf = [0+0j] * (corrWindow + corrStride)
        self.state = self.sNoPkt

    def push(self, iq):
        self.buf = [iq] + self.buf[0:-1]
        return self.process()

    def process(self):
        powers = [abs(iq) > self.powerThresh for iq in
                  self.buf[self.corrStride:self.corrStride+self.powerWindow]]
        powerHigh = all(powers)
        powerLow = not any(powers)
        print("Packet: {}".format(self.state == self.sPkt))
        print()
        corrNum = sum([(self.buf[i] * self.buf[i+self.corrStride].conjugate()) / 4 for i in range(self.corrWindow)])
        print("corrNum {}".format(corrNum))
        corrDenom = sum([(self.buf[i+self.corrStride] * self.buf[i+self.corrStride].conjugate()) / 4 for i in range(self.corrWindow)])
        corrComp = corrNum.real > 0.75 * corrDenom.real
        print("corrDenom {}".format(corrDenom))
        print("corrComp {}".format(corrComp))
        # state update
        if powerHigh:
            if self.doCorrThresh and corrComp:
                self.state = self.sPkt
            elif self.doCorrThresh and not corrComp:
                self.state = self.state
            else:
                self.state = self.sPkt
        if powerLow:
            self.state = self.sNoPkt

        return self.buf[-1] if self.state == self.sPkt else None


tvNoPkt = [.1+.1j, .1-.1j] * 32
tvPwrOnly = [0+0j]*10 + [1-1j]*15 + [1+1j]*15 + [1-1j]*15 + [1+1j]*15 + [1-1j]*15 + [1+1j]*15 + \
            [0+0j]*32
tvPwrCorr = [0+0j]*10 + [1-1j]*8 + [1+1j]*8 + [1-1j]*8 + [1+1j]*8 + [1-1j]*8 + [1+1j]*8 + [1-1j]*8 + \
            [1+1j]*8 + [1-1j]*8 + [1+1j]*8 + [1-1j]*8 + [1+1j]*8 + [0+0j]*32
tvPwrOnlyOut = [1-1j]*15 + [1+1j]*15 + [1-1j]*15 + [1+1j]*15 + [1-1j]*15 + [1+1j]*15
tvPwrCorrOut = [1-1j]*8 + [1+1j]*8 + [1-1j]*8 + [1+1j]*8 + [1-1j]*8 + [1+1j]*8 + [1-1j]*8 + \
               [1+1j]*8 + [1-1j]*8 + [1+1j]*8 + [1-1j]*8 + [1+1j]*8
tvList = [tvNoPkt, tvPwrOnly, tvPwrCorr]
tvRefList = [[], tvPwrOnlyOut, tvPwrCorrOut]

def main():
    print("Running without correlation...")
    for (tv, ref) in zip(tvList, tvRefList):
        pd = PacketDetect(4, .75, False, .75, 4, 16)
        iqOut = []
        # Push iq through
        for iq in tv:
            retval = pd.push(iq)
            if retval is not None:
                iqOut.append(retval)
        # Check output
        print("Finished run...")
        if not ref:
            assert len(iqOut) == 0, "iqOut len was {} not 0".format(len(iqOut))
        else:
            assert len(iqOut) == len(ref), "len(iqOut) {} != {} len(ref)".format(len(iqOut), len(ref))
            for iqr, iqo in zip(ref, iqOut):
                assert iqr == iqo, "iqr {} != {} iqo".format(iqr, iqo)
        print("Passed checks for this run...")
    print("Running with correlation...")
    for (tv, ref) in zip(tvList, [tvRefList[0], None, tvRefList[2]]):
        pd = PacketDetect(4, .75, True, .75, 4, 16)
        iqOut = []
        # Push iq through
        for iq in tv:
            retval = pd.push(iq)
            if retval is not None:
                iqOut.append(retval)
        # Check output
        print("Finished run...")
        if not ref:
            assert len(iqOut) == 0, "iqOut len was {} not 0".format(len(iqOut))
        else:
            assert len(iqOut) == len(ref), "len(iqOut) {} != {} len(ref)".format(len(iqOut), len(ref))
            for iqr, iqo in zip(ref, iqOut):
                assert iqr == iqo, "iqr {} != {} iqo".format(iqr, iqo)
            print("All {} samples matched".format(len(ref)))
        print("Passed checks for this run...")
    print("Passed all runs!")

if __name__ == "__main__":
    main()
