# FFT & IFFT
### Paul Kwon <hyunjaekwon@berkeley.edu>

## Overview
These blocks implement a fast Fourier transform (FFT) and its inverse based on the Cooley-Tukey algorithm (CTA) and the prime-factor algorithm (PFA). A direct form implementation can handle any FFT size, whereas the in-place implementation can only handle powers of 2.

### Direct Form
The direct form FFT implementation instantiates submodules as stages that recursively instantiate smaller FFTs. Two types of stages exist, CTA stages and PFA stages. The CTA stage handles any powers of numbers, recursively instantiating smaller CTA stages and butterflies. The PFA stage handles any FFT size, splitting the size into relatively prime factors and instantiating smaller CTA and PFA stages.

For example, consider a 360-point FFT. The prime factorization of 180 is 2<sup>3</sup>&sdot;3<sup>2</sup>&sdot;5<sup>1</sup>.

- The highest FFT stage will be a PFA implementation, creating 8-point FFT CTA stages and 45-point FFT PFA stages.
- The 45-point FFT PFA stage will create 9-point CTA stages and radix-5 butterflies.
- The 8-point FFT CTA stage will create 4-point FFT CTA stages and radix-2 butterflies, with twiddling factors in between. The 4-point FFT CTA stages will then create 2-point FFT CTA stages (which are simply radix-2 butterflies) and radix-2 butterflies, with twiddling factors in between.
- The 9-point FFT CTA stages will create 3-point FFT CTA stages (which are simply radix-3 butterflies) and radix-3 butterflies, with twiddling factors in between.

CTA stages are implemented with a decimation-in-time algorithm.

Pipeline registers can be added between stages to improve the throughput of the block.

### In-place Structure -- SDF
An in-place alternative to the direct form is often desired because by streaming data and reusing the hardware, computations are more efficient from a harware-utilization perspective at the cost of increased latency. For an in-place computation, the architecture used in this FFT is the radix-2 and radix-4 single-path delay feedback (SDF) architecture.

As per the classic SDF implementation, this FFT is composed of log N stages, where each stage contains a butterfly unit, delay line, and a twiddle factor multiplication. The delay line is used to buffer both the stage input and the butterfly output, as the butterfly requires two inputs spaced apart by some delay and outputs two samples spaced apart by the same delay. Thus, this implementation also has an FSM to generate the necessary control signals to switch between these two functionalities of the delay line, as well as to provide the correct twiddle factor. An unscrambler is instantiated to reorder the samples of this FFT.

Both decimation-in-time (DIT) and decimiation-in-frequency (DIF) decompositions are supported. The decomposition choice determines the ordering of the stages (smaller delay stages to larger delay stages or vice versa) and whether the unscrambling occurs at the input or the output. The two decomposition settings can be selected by the user, as well as a third setting -- "optimal".

The SDF chain of stages is serial input and serial output, but the unscrambler is parallel input and parallel output. Hence, some serialization and deserialization may be needed. The unscrambling happens at the output for DIF and input for DIT.

When the FFT wrapper module is serial input and parallel output, the data flow would be one of these two cases:

- DIF: SDF Chain &rarr; Deserializer &rarr; Unscrambler
- DIT: Deserializer &rarr; Unscrambler &rarr; Serializer &rarr; SDF Chain &rarr; Deserializer

When the FFT wrapper module is parallel input and serial output, the data flow would be one of these two cases:

- DIF: Serializer &rarr; SDF Chain &rarr; Deserializer &rarr; Unscrambler &rarr; Serializer
- DIT: Unscrambler &rarr; Serializer &rarr; SDF Chain

In each of these cases, one decomposition involves less hardware and has less latency than the other. Thus, when the optimal setting is chosen, the generator chooses DIF or DIT based on which decomposition is more suitable.

### IFFT Architecture -- Reusing the FFT
The IFFT can simply reuse the FFT architecture. Specifically, one can...

- Instantiate the FFT
- Swap the real and imaginary components of the IFFT inputs and pass them to the FFT
- Swap the real and imaginary components of the FFT outputs and apply a 1/N scaling factor (for an N-point IFFT). These are the IFFT outputs.

## Parameters
- `protoIQ: DspComplex[T]` - prototype for IQ data
- `protoTwiddle: DspComplex[T]` - prototype for twiddle factors
- `numPoints: Int` - number of points in FFT
- `fftType: String` - type of FFT to use. Options are `direct` and `sdf`
- `pipeline: Boolean` - if the direct FFT is used, indicates whether to pipeline the FFT stages
- `decimType: String` - if the SDF FFT is used, indicates whether to use DIT, DIF, or whichever is more hardware-optimal (see above in the Architecture section for more info). Corresponding options are `dit`, `dif`, and `opt`, respectively.
- `width: Int` - inherited from PacketBundleParams. **Note: this is internally set to `numPoints`, so do not use.**

## Inputs
### FFT
Decoupled interface

- `iq: Vec[DspComplex[T]]` - input IQ samples (in time domain) - **width 1**
- `pktStart: Bool` - start of packet flag
- `pktEnd: Bool` - end of packet flag

### IFFT
Decoupled interface

- `iq: Vec[DspComplex[T]]` - input IQ samples (in frequency domain) - **width `numPoints`**
- `pktStart: Bool` - start of packet flag
- `pktEnd: Bool` - end of packet flag

## Outputs
### FFT
Decoupled interface

- `iq: Vec[DspComplex[T]]` - output IQ samples (in frequency domain) - **width `numPoints`**
- `pktStart: Bool` - start of packet flag
- `pktEnd: Bool` - end of packet flag

### IFFT
Decoupled interface

- `iq: Vec[DspComplex[T]]` - output IQ samples (in time domain) - **width 1**
- `pktStart: Bool` - start of packet flag
- `pktEnd: Bool` - end of packet flag

<br/>
**Note**: The FFT and IFFT interfaces are similar, with the IFFT's output interface equivalent to the FFT's input interface and the IFFT's input interface equivalent to the FFT's output interface.  
**Note**: The time sample interfaces are serial, whereas the frequency sample interfaces are parallel. We decided to design it as such to make interfaces between adjacent blocks simpler at the system level, but one can easily change it to serial-serial or parallel-parallel through the use of serializers and deserializers also designed in this project.

## Tests
`sbt test`, `sbt "testOnly modem.FFTSpec"`  
Test cases are found in `FFTSpec.scala`. Testers running these test cases are found in `FFTTester.scala`.  
Test vectors are randomly generated, and tests cover FFT & IFFT functionality for the following FFT/IFFT sizes:

- Powers of 2: tests CTA direct form and SDF implementations
- Powers of 3: tests CTA direct form implementations. Should work for any other powers as well.
- Other numbers (e.g., 36): tests the combination of PFA and CTA direct form implementations

Other test cases in `FFTSpec` check `FFTUtil.scala`, which contains helpful constant generating functions, etc., primarily for handling Good's & CRT mapping in the PFA.

## TODOs
- Reconfigurable FFT/IFFT sizes during run-time (as opposed to fixed behavior based on user's parameters)
- In-place FFT/IFFT support of non-powers-of-2. This involves...
    - Radix-n SDF FFT/IFFT for n > 2
    - In-place implementation of PFA
- Rader's FFT for more efficient calculation of large prime numbers. This is not as important for our WiFi & LTE modem application, but it would be helpful for a more generic FFT/IFFT generator.
- Using RAM instead of a shift register for the SDF stage delay line. This is especially crucial for larger stage sizes.
- In-place implementation of the SDF FFT unscrambler.
