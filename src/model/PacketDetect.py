#!/usr/bin/env python


class PacketDetect:
    def __init__(self, powerWindow, powerThresh, doCorrThresh, corrThresh,
            corrWindow, corrStride):
        self.powerWindow = powerWindow
        self.powerThresh = powerThresh
        self.doCorrThresh = doCorrThresh
        self.corrThresh = corrThresh
        self.corrWindow = corrWindow
        self.corrStride = corrStride
        self.buf = [0+0j] * (corrWindow + corrStride)

    def push(self, iq):
        self.buf = [iq] + self.buf[1:]
        return self.process()

    def process(self):
        power = all([iq > self.powerThresh for iq in
            self.buf[self.corrStride:self.corrStride+self.powerWindow])
        corrNum = sum([
