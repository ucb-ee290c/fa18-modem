# Carrier Frequency Offset Estimation/Correctoion
### Sean Huang <sehuang@berkeley.edu>

## Overview
The CFO Estimator uses the short and long training preambles in the Wi-Fi 802.11a OFDM frame to perform coarse and fine frequency offset estimation. The coarse estimation is performed using the short training field, which is made up of 10 repeating groups of 16 samples each. The estimation uses the fact that each sample should be the same as one delayed by 16 samples to determine the phase shift between the two samples, and then averages this shift over the entire training field to get a coarse per sample phase offset estimation. The fine offset estimation works in the same way, acting on the long training field, which is made up of 2 groups of 64 samples each.

The estimator drops the short training field as it is the last block to use it, and delays the `pktStart` signal accordingly.

The CFO correction is performed with a CORDIC rotation of the input I/Q and has a per sample phase correction supplied by the Estimator. The block rotates each incoming sample by the relative phase error from the last sample to correct for the CFO.

## Parameters
- `protoIQ: DspComplex[T]` - prototype for IQ data
- `stLength: Int` - length of short training field in samples
- `ltLength: Int` - length of long training field in samples (including GI)
- `giLength: Int` - length of guard interval
- `preamble: Boolean = true` - is CFO Estimation preamble based? (always true)
- `protoXY: T` - prototype CORDIC XY type
- `protoZ: T` - prototype CORDIC Z type
- `correctGain: Boolean` - does CORDIC have gain correction?
- `nStages: Int` - stages of CORDIC
- `stagesPerCycle: Int` - logic depth of CORDIC, extent of unrolling

## Inputs
Wrapped in a Decoupled interface:
- `iq: Vec[DspComplex[T <:Data:Real:BinaryRepresentation]]` - always width 1
- `pktStart: Bool()` - get from packet detector
- `pktEnd: Bool()` - get from packet detector

## Outputs
Wrapped in a Decoupled interface:
- `iq: Vec[DspComplex[T <:Data:Real:BinaryRepresentation]]` - always width 1
- `pktStart: Bool()` - high at first sample of LTF
- `pktEnd: Bool()` - passed through from packet detector

## Tests
`sbt test`, `sbt testOnly modem.CFOCorrectionSpec`

Tests use training field vectors defined by IEEE802.11a standard and a sequence of random IQ values. It tests:
- Whether the CFO Estimation has estimated the correct per sample phase error

## TODOS
- Pilot tracking CFO Estimation
- STF dropping tests
