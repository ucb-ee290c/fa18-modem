# Equalizer
### Josh Sanz <jsanz@berkeley.edu>

## Overview
This blocks implements a zero-forcing preamble based equalizer. It expects two preamble symbols which it averages and compares to the provided ideal preamble.

```
dB
  |**               **
  |  **   *****   **
  |    ***     ***
0 |----------------------- f
  |    ...     ...
  |  ..   .....   ..
  |..               ..

Channel: ...
Equalizer: ***
```

Takes LTF field on

Drops LTF, sends equalized body



## Parameters
- `width: Int` - inherited from PacketBundleParams, unused
- `protoIQ: DspComplex[T]` - prototype for IQ data
- `carrierMask: Seq[Boolean]` - boolean mask of subcarriers which should be equalized
- `nSubcarriers: Int` - how many subcarriers to expect from the FFT
- `preambleSymbol: DenseVector[Complex]` - frequency domain vector of the values in each preamble symbol

## Inputs
Decoupled interface
- `iq: Vec[DspComplex[T]]` - input IQ samples
- `pktStart: Bool` - start of packet flag
- `pktEnd: Bool` - end of packet flag

## Outputs
Decoupled interface
- `iq: Vec[DspComplex[T]]` - output equalized IQ samples
- `pktStart: Bool` - start of packet flag
- `pktEnd: Bool` - end of packet flag

## TODOs
- MMSE equalization algorithm
- Make number of preamble symbols a parameter
- Handle large dynamic range better.
