# Encoder
Kunmo Kim (kunmok@berkeley.edu)

## Overview 
Encoder is composed of a convolutional encoder and a puncturing block. 
The current encoder is fully compatible with 802.11a Tx 

#### Convolutional Coding
Convolutional coding is a type of error correction coding that generates parity symbols via the convoluting multiple signals stored in a bank of shift registers. 
Convolutional coding by nature creates time-invariant trellis and hence it allows the decoder to use maximum-likelihood sequence estimator/equalizer. 

#### Puncturing 
Puncturing block punctures selected data from the convolutional code to increase the coding rate at the cost of BER. Puncturing matrix must be chosen carefully so that it minimally affects the BER at given SNR. Puncturing block sets its coding rate to 1/2 when its processing header information. 
Below is the puncturing matrix from 802.11a specification 

| Code Rate | Puncturing Matrix | Free Distance |
|:-----------:|:-------------------:|:---------------:|
| 1/2 | [1,1,0,1], [0,1,0,1], [1,0,0,1] | 10 |
| 2/3 | [0,0,0,1] | 6 |
| 3/4 | [1,1,1,1], [0,1,1,1], [1,0,1,1], [0,0,1,1] | 5 |
| 1/2 | else | 10| 

#### Puncturing Matrix v.s. Coding Rate + Modulation Scheme 
| Puncturing Matrix | Rate (Mbps) | Coding Rate | Modulation Scheme |
|:-----------:|:-------------------:|:---------------:|:-----------:|
| [1,1,0,1] | 6  | 1/2 | BPSK |
| [1,1,1,1] | 9  | 3/4 | BPSK |
| [0,1,0,1] | 12 | 1/2 | QPSK |
| [0,1,1,1] | 18 | 3/4 | QPSK | 
| [1,0,0,1] | 24 | 1/2 | QAM-16 |
| [1,0,1,1] | 36 | 3/4 | QAM-16 |
| [0,0,0,1] | 48 | 2/3 | QAM-64 |
| [0,0,1,1] | 54 | 3/4 | QAM-64 |

## Parameters
* k: Int = number of input ports 
* n: Int = number of output ports. 
  + Coding rate is defined as 'k/n'
* K: Int = constraint length (depth of convolutional coding).
  + m: Int = number of memory elements. This is defined as m = K - 1 
* genPolynomial: List[Int] = generator polynomial for convolutional coding. 
  + Default: (7, 5) 
* tailBitingEn: Boolean = enable/disable tail-biting feature in the encoder. enabling tail-biting restores coding rate loss when its payload has small size

## Inputs
* in: Flipped(Decoupled(UInt(1.W)))
  + Input data stream is coming from MAC layer. Encoder takes 1 bit data at each clock cycle 
* mac: MacCtrl(params)
  + isHead: Bool()
    + indicate whether the current input is part of header information.
    + when isHead == true.B, coding rate & modulation scheme are fixed to 1/2 & BPSK.   
  + puncMatrix: Vec(4, UInt(1.W))
    + update puncturing matrix in real-time. Its change is applicable only when isHead is low.  
* pktStartIn: Bool()
  + indicate the start of packet (WIFI frame). 
* pktEndIn
  + indicate the end of packet (WIFI frame)
   
## Outputs
* out: Decoupled(BitsBundle(params)) 
  + BitsBundle includes three signals: 
    + pktStart: Bool()
    + pktEnd: Bool()
    + bits: Vec(params.bitsWidth, params.protoBits.cloneType)
* modCtrl: UInt(2.W) = controller signal for signal mapper
  + 0.U = BPSK 
  + 1.U = QPSK 
  + 2.U = QAM-16
  + 3.U = QAM-64   

