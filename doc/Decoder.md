#Decoder
Kunmo Kim (kunmok@berkeley.edu)

## Overview 
Maximum likelihood sequence estimator based on Viterbi algorithm is used for the 802.11a Rx decoder. 
When the decoder receives 48 bits (1 OFDM symbol), it checks if the OFDM symbol contains header information. If it does, then the Header extractor (a simple Viterbi decoder) extracts header information (coding rate and length). Coding rate information is used to update the puncturing matrix for the de-puncturing module, and length information is used to expect the arrival time of the last bit of PSDU. 
<br />
Header information decoding is done via "HeaderExtractor" block. Data decoding is done via "PathMetric", "BranchMetric", and "Traceback" blocks, which form a sliding-window Viterbi decoder. 
<br />


#### De-Puncturing 
De-puncturing block has two purposes: 1) store received input into a buffer 2) De-puncture the received PSDU 
<br /> Puncturing matrix is contained in a header frame. 'HeaderExtractor' extracts the header information and passes the coding rate information to the de-puncturing block. HeaderExtractor must decode OFDM header symbol within 64 clock cycles.
Once the de-puncturing receives the coding rate information, it updates its coding rate, and then pass the proper modulation scheme to the demodulator block. The modulation scheme is chosen based on the puncturing matrix as shown in the table below. <br />
Once it starts receiving PSDU, De-Puncturing block performs de-puncturing via filling up dummy bits (0), and then pass the 2-bit to path-metric 

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

