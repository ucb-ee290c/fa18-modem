# Packet Detector
### Josh Sanz <jsanz@berkeley.edu>

## Overview
The packet detector uses a windowed power estimate to detect the beginning and end of a packet. When all samples in the window have power greater than the threshold a packet is deemed to have started. When all samples in the window have power less than the threshold a packet is deemed to have ended.

```
|
|   ...........
|..:           :........
-----------------------------
    ^          ^
    pktStart   pktEnd
```

Only samples which are part of a packet are passed on to following blocks. The rest are discarded.

## Parameters
- `protoIQ: DspComplex[T]` - prototype for IQ data
- `powerThreshVal: Double` - threshold for power detection
- `powerThreshWindow: Int` - size of window for power thresholding
- (unused) `correlationThresh: Boolean` - enables correlation thresholds
- (unused) `correlationThreshVal: Double` - threshold for correlation detection
- (unused) `correlationWindow: Int` - size of window for correlation detection
- (unused) `correlationStride: Int` - distance between samples which should be correlated

## Inputs
Decoupled interface
- `iq: DspComplex[T]`

## Outputs
Decoupled interface
- `iq: Vec[DspComplex[T <:Data:Real:BinaryRepresentation]]` - always width 1
- `pktStart: Bool()` - high at first sample of packet
- `pktEnd: Bool()` - hight at last sample of packet

## Tests
`sbt test`

Tests use static test vectors defined in the Spec file. Tests include:
- Whether detector ignores IQ data which does not exceed the threshold for a full window
- Whether detector finds a packet based on power alone and returns the correct samples
- (unused) Whether detector ignores IQ data which exceeds the power threshold but is uncorrelated
- (unused) Whether detector finds a packet with both power and correlation thresholds

## TODOS
- CFAR power-based detection
- Correlation-based threshold and synchronization