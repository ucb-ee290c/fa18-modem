# Equalizer
### Josh Sanz <jsanz@berkeley.edu>

## Overview
This blocks implements a zero-forcing preamble based equalizer. It expects two preamble symbols which it averages and uses to calculate the channel correction vector.

### Zero Forcing
The zero forcing algorithm, illustrated below, tries to simply invert the channel transfer function `H(w)`.

`Correction(w) = 1 / H(W)`

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

### Architecture
The channel inversion uses two CORDICs and a divider to pipeline the channel inversion and reduce latency. The first CORDIC splits a sub-carrier's IQ value into a magnitude and phase. The divider then inverts the magnitude. Finally, the second CORDIC combines the new magnitude and minus the phase found by the first CORDIC to obtain the correction value.

While the subcarriers are streaming through the correction calculation `ready` is held low on the input. After the correction has been calculated, `ready` goes high and the rest of the packet can stream through. The correction is applied directly to the output of the FFT and passed through to following blocks. The LTF symbols are never sent on. After the correction function has been calculated they are dropped.

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

## Tests
`sbt test`
Test vectors are static and found in `EqualizerSpec.scala`. Tests include:
- Whether packets passed through an identity channel make it through the equalizer unchanged and the LTF symbols are dropped
- Whether a constant gain across all subcarriers is corrected
- Whether a constant phase rotation across all subcarriers is corrected

Additionally, the channel inversion block is tested for gain and phase changes together.

## TODOs
- MMSE equalization algorithm
- Make number of preamble symbols a parameter
- Handle large dynamic range better.
