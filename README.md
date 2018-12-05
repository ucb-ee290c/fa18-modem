# OFDM Baseband Modem

[![Build Status](https://travis-ci.org/ucberkeley-ee290c/fa18-modem.svg?branch=master)](https://travis-ci.org/ucberkeley-ee290c/fa18-modem)

## Implemented in Chisel and supporting 802.11 and LTE radios
## Authors: Sean Huang, Kunmo Kim, Paul Kwon, Josh Sanz, Meng Wei

## Dependencies
- scala 2.12
- rocket-dsptools
- riscv-toolchain

## Building
```
cd tests
make
cd ../verisim
make debug   # debug required to log waveforms
```

## Testing
`sbt test`

## Simulating
Example C code for interacting with the transmit and receive chains is included under the `tests` directory in [rx.c](tests/rx.c) and [modem.c](tests/modem.c).

## Current Standard Support
### CFO
The CFO Estimation and Correction estimates the carrier frequency offset using the short and long training fields of the OFDM packet. The training samples are cross correlated to determine the phase offset of each sample, then the error is sent to a CORDIC phase correction to rotate the input IQ vectors accordingly.

The length of the training fields and the pipeline depth of the estimator/CORDIC are parameterizable. The generator is type-generic with regard to the IQ representation.

More information is available in [CFO.md](doc/CFO.md).

### Packet Detection
The packet detector uses a moving average power estimate to detect the beginning of a packet when average power exceeds a threshold and the end of a packet when the average power falls below a threshold.

The number of samples in the power averaging window and the threshold are both parameterizable, and the generator is generic to the type of IQ real and imaginary parts.

More information is available in [PacketDetect.md](doc/PacketDetect.md).

### Cyclic Prefix
The cyclic prefix block can add or remove a cyclic prefix from symbols in a packet. The length of the previx and size of an OFDM symbol are parameterized in the generator, and addition/removal is controlled with a bool input.

More information is available in [CyclicPrefix.md](doc/CyclicPrefix.md).

### FFT
The FFT uses the radix-2 Cooley-Tukey decimation-in-time (DIT) algorithm to compute the fast Fourier transform. The IFFT reuses the FFT, but swaps the real and imaginary components at the input to the FFT and scales the output by 1/N for an N-point IFFT. Currently, the supported FFT sizes are powers of 2, and sizes are fixed. FFTs with reconfigurable sizes are not yet supported.

More information is available in [FFT_IFFT.md](doc/FFT_IFFT.md).

### Equalizer
The equalizer uses preamble-based least-squares equalization to apply a fixed channel correction to a full packet. Pilot-based equalization is not yet supported. Currently, the preamble must be BPSK, but could easily be extended to arbitrary symbols.

More information is available in [Equalizer.md](doc/Equalizere.md).

### Modulator
The modulator supports BPSK, QPSK, and 16QAM. It also implements the interleaving scheme from IEEE 802.11a.

More information is available in [Modulator.md](doc/Modulator.md).

### Demodulator
The modulator supports BPSK, QPSK, and 16QAM. It implements the deinterleaving scheme from IEEE 802.11a.

More information is available in [Demodulator.md](doc/Demodulator.md).

### Convolutional Encoder
The encoder is composed of a convolutional encoder and a puncturing block.
The current encoder is fully compatible with 802.11a.

More information is available in [Encoder.md](doc/Encoder.md).

### Viterbi Decoder
The Viterbi decoder contains a low-latency fixed-length Viterbi decoder for PLCP headers and a sliding windows arbitrary length decoder for packet body data.

More information is available in [Decoder.md](doc/Decoder.md).

### Modem
The transmit chain is

[ENCODER](doc/Encoder.md) --> [MODULATOR](doc/Modulator.md) --> [IFFT](doc/FFT_IFFT.md) --> [CYCLIC PREFIX](doc/CyclicPrefix.md) --> [PREAMBLE ADDITION](doc/PreambleAdder.md) --> [RAISED COSINE](doc/RaisedCosineFilter.md)

The receive chain is

[CFO CORRECTION](doc/CFO.md) --> [PACKET DETECTOR](doc/PacketDetect.md) --> [CYCLIC PREFIX](doc/CyclicPrefix.md) --> [FFT](doc/FFT_IFFT.md) --> [EQUALIZER](doc/Equalizer.md) --> [DEMODULATOR](doc/Demodulator.md) --> [DECODER](doc/Decoder.md)