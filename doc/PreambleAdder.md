# Preamble Addition
### Sean Huang (<sehuang@berkeley.edu>)

## Overview
The preamble adder attaches the IEEE802.11a specified short and long training preambles to the transmitter OFDM frame. This is done simply by appending the STF and LTF samples to the head of the output frame from the rest of the transmitter chain. The input to the block is buffered by the sample length of the preamble and once the preamble transmission is complete, the block simply passes through the samples of the rest of the frame.

## Parameters
- `protoIQ: DspComplex[T]` - prototype for IQ data
- `stLength: Int` - length of short training field in samples
- `ltLength: Int` - length of long training field in samples

## Inputs
Wrapped in a Decoupled interface:
- `iq: Vec[DspComplex[T <:Data:Real:BinaryRepresentation]]` - always width 1
- `pktStart: Bool()` - start of packet flag
- `pktEnd: Bool()` - end of packet flag

## Outputs
Wrapped in a Decoupled interface:
- `iq: Vec[DspComplex[T <:Data:Real:BinaryRepresentation]]` - always width 1
- `pktStart: Bool()` - start of packet flag
- `pktEnd: Bool()` - end of packet flag

## Tests
`sbt test`, `sbt testOnly modem.PreambleAdditionSpec`

Tests use training field vectors defined by IEEE802.11a standard and a sequence of random IQ values. It tests:
- If the preamble has been properly appended
- If the data following the preamble exactly matches data input during preamble addition
