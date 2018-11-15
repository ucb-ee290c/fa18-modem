# OFDM Baseband Modem

[![Build Status](https://travis-ci.org/ucberkeley-ee290c/fa18-modem.svg?branch=master)](https://travis-ci.org/ucberkeley-ee290c/fa18-modem)

## Implemented in Chisel and supporting 802.11 and LTE radios
## Authors: Sean Huang, Kunmo Kim, Paul Kwon, Josh Sanz, Meng Wei

## Dependencies
- scala 2.12
- rocket-dsptools

## Building
Not implemented yet.

## Testing
`sbt test`

## Simulating
Not implemented yet.

## Current Standard Support
### CFO
- Preamble based coarse correction
- Preamble based fine correction
- ? Pilot based correction

### Packet Detection
The packet detector uses a moving average power estimate to detect the beginning of a packet when average power exceeds a threshold and the end of a packet when the average power falls below a threshold.

The number of samples in the power averaging window and the threshold are both parameterizable, and the generator is generic to the type of IQ real and imaginary parts.

TODOS:
- CFAR power-based detection
- Correlation-based threshold and synchronization

### Cyclic Prefix
The cyclic prefix block can add or remove a cyclic prefix from symbols in a packet. The length of the previx and size of an OFDM symbol are parameterized in the generator, and addition/removal is controlled with a bool input.

### FFT
- Fixed 2^n FFT
- ? Fixed 2^n IFFT

### Equalizer
The equalizer uses preamble-based least-squares equalization to apply a fixed channel correction to a full packet. Pilot-based equalization is not yet supported. Currently, the preamble must be BPSK, but could easily be extended to arbitrary symbols.

The generator parameters are:
- FFT width, or number of subcarriers in the channel
- Mask of subcarriers which contain data and should be equalized.
- Training field symbols
- Type of IQ real and imaginary parts

TODOS:
- Preamble based MMSE equalization
- Pilot based equalization

### Modulator
- BPSK modulation/demodulation
- TODO: Interleaving/deinterleaving
- TODO: QPSK, 16QAM, 64QAM

### FEC
- Generator configurable
  - Constraint length
  - Generator polynomials
  - Coding ratio
  - Puncturing
  - Feedback polynomial for recursive systematic coding
  - Tailbiting scheme
  - Minimum bits per OFDM symbol
- Viterbi algorithm python model
- TODO: Chisel Viterbi decoder

### Modem
- Currently just passthrough dummy blocks
- TODO: replace with actual implementations
- TODO: write tests
