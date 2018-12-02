# Cyclic Prefix
## Josh Sanz <jsanz@berkeley.edu>

## Overview
The cyclic prefix block can add or remove cyclic prefixes from OFDM symbols. 
It uses the `pktStart` and `pktEnd` flags to determine where symbols occur for alignment purposes.

When removing prefixes, the block simply drops the first `prefixLength` samples it receives and
passes the next `symbolLength` samples in a cycle.

When adding prefixes, the block first buffers in `symbolLength` samples. It then sends out the 
final `prefixLength` samples of the buffer, then flushes the buffer from the beginning.

An example with `prefixLength`=2 and `symbolLength`=6 is shown below
```
Add = True
           |-------------|
fedcba --> |Cyclic Prefix| --> fedcbafe
           |-------------|
           
Add = False
             |-------------|
fedcbafe --> |Cyclic Prefix| --> fedcba
             |-------------|
``` 

## Parameters
- `protoIQ: DspComplex[T]` - prototype for IQ data
- `prefixLength: Int` - number of samples in the prefix/guard interval of an OFDM symbol
- `symbolLength: Int` - number of samples in an OFDM symbol

## Inputs
Decoupled interface
- `add: Bool` - block will add cyclic prefixes when true and remove them when false; the flag should only be changed between packets
- `iq: Vec[DspComplex[T]]` - incoming IQ, vector has width 1
- `pktStart: Bool` - packet start flag
- `pktEnd: Bool` - packet end flag

## Outputs
Decoupled interface
- `iq: Vec[DspComplex[T]]` - outgoing IQ, vector has width 1
- `pktStart: Bool` - packet start flag
- `pktEnd: Bool` - packet end flag


## Tests
The block's tests include whether it
- Removes the prefix from a single symbol
- Removes the prefixes from multiple symbols
- Adds a prefix to a single symbol
- Adds a prefix to multiple symbols
- Can switch between adding and removing prefixes betwen packets
