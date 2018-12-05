# Serializer & Deserializer
### Paul Kwon <hyunjaekwon@berkeley.edu>

## Overview
Some of the signal processing in the WiFi/LTE modem is done using a serial stream of data, while some is done with parallel data. Thus, a serializer and deserializer were implemented to be used as needed throughout the RX and TX chain as necessary.

There are two data types used for serialization and deserialization:

- IQ data, which is of datatype `DspComplex[T <: Data : Real : BinaryRepresentation]`
- Demodulated data, which is of datatype `T <: Data : Real : BinaryRepresentation`

No deserializer currently exists for the demodulated data simply because it is not needed in our design. This results in 3 different modules: `PacketSerializer` (IQ data), `PacketDeserializer` (IQ data), and `BitsSerializer` (demodulated data).

The only addition from a typical serializer and deserializer is the handling of packet start/end flags. Rather than deserializing/serializing those flags as well, the deserialized interface has 1 packet start flag, signifying whether a packet started in one of the deserialized samples. The same holds true for the packet end flag. An underlying assumption is that the length of the packet is an integer multiple of the deserialization ratio and is upheld in our design.

## Parameters
- Prototype for data, which is one of the following:
	+ `protoIQ: DspComplex[T]` - prototype for IQ data
	+ `protoBits: T` - prototype for demodulated data
- `ratio: Int` - deserialization/serialization ratio
- Unused parameter, based on data prototype. **In either case, this parameter value is internally set to `ratio`, so do not use.**
	+ `width: Int` - inherited from PacketBundleParams for IQ data.
	+ `bitsWidth: Int` - inherited from BitsBundleParams for demodulated data.

## Inputs
### PacketDeserializer
Decoupled interface

- `iq: Vec[DspComplex[T]]` - input IQ samples - **width 1**
- `pktStart: Bool` - start of packet flag
- `pktEnd: Bool` - end of packet flag

### PacketSerializer
Decoupled interface

- `iq: Vec[DspComplex[T]]` - input IQ samples - **width `ratio`**
- `pktStart: Bool` - start of packet flag
- `pktEnd: Bool` - end of packet flag

### BitsSerializer
Decoupled interface

- `bits: Vec[T]` - input demodulated samples - **width `ratio`**
- `pktStart: Bool` - start of packet flag
- `pktEnd: Bool` - end of packet flag

## Outputs

### PacketDeserializer
Decoupled interface

- `iq: Vec[DspComplex[T]]` - input IQ samples - **width 1**
- `pktStart: Bool` - start of packet flag
- `pktEnd: Bool` - end of packet flag

### PacketSerializer
Decoupled interface

- `iq: Vec[DspComplex[T]]` - input IQ samples - **width 1**
- `pktStart: Bool` - start of packet flag
- `pktEnd: Bool` - end of packet flag

### BitsSerializer
Decoupled interface

- `bits: Vec[T]` - input demodulated samples - **width `ratio`**
- `pktStart: Bool` - start of packet flag
- `pktEnd: Bool` - end of packet flag

## Tests
`sbt test`, `sbt "testOnly modem.SerDesSpec"`  
Test cases are found in `SerDesSpec.scala`. Testers running these test cases are found in `SerDesTester.scala`.  
Test vectors are randomly generated, and tests cover Serializer and Deserializer functionality for several deserialization ratios.

## TODOs
- Make serializer (& deserializer) more data type generic so that we don't need two versions of each, one for each data type. The actual code between the two versions is exactly the same except for the interfaces.
