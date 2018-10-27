# OFDM Baseband Modem
## Implemented in Chisel and supporting 802.11 and LTE radios
## Authors: Sean Huang, Kunmo Kim, Paul Kwon, Josh Sanz, Meng Wei

## Dependencies
See `build.sbt`

## Building
Not implemented yet.

## Testing
`sbt test`

## Simulating
Not implemented yet.

## Current Standard Support
### CFO
- Preamble based coarse correction
- ? Preamble based fine correction
- ? Pilot based correction

### Packet Detection
- Fixed threshold power-based detection
- TODO: CFAR power-based detection
- TODO: Correlation threshold and synchronization

### FFT
- Fixed 2^n FFT
- ? Fixed 2^n IFFT

### Equalizer
- Preamble based least-squares equalization
- TODO: Preamble based MMSE equalization
- TODO: Pilot based equalization

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
