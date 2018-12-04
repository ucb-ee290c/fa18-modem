# Decoder
Kunmo Kim (kunmok@berkeley.edu)

## Overview 
802.11a decoder deploys ,aximum likelihood sequence estimator based on the Viterbi algorithm. 
The decoder receives 48 bits (1 OFDM symbol), then it checks if the OFDM symbol contains PLCP header information. If it does, then the Header extractor (a simple Viterbi decoder) extracts the coding rate and length. Coding rate information is used to update the puncturing matrix for the de-puncturing module, and length information is used to expect the arrival time of the last bit of PSDU. 
<br />
Header information decoding (PLCP decoding) is done via "HeaderExtractor" block. PSDU Data decoding is done via "PathMetric", "BranchMetric", and "Traceback" blocks, which form a sliding-window Viterbi decoder. 
<br />


#### De-Puncturing 
De-puncturing block has two purposes: 1) store received input into a buffer 2) De-puncture the received PSDU 
<br /> Puncturing matrix is contained in a PLCP frame. 'HeaderExtractor' extracts the header information and passes the coding rate information to the de-puncturing block. HeaderExtractor must decode PLCP within 64 clock cycles.
Once the de-puncturing receives the coding rate information, it updates its coding rate then updates the proper modulation scheme to the demodulator block. The modulation scheme is chosen based on the puncturing matrix as shown in the table below. <br />
Once it starts receiving PSDU, De-Puncturing block performs de-puncturing via filling up dummy bits (0), and then passes the 2-bit to path-metric at each clock cycle. 

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

   
#### Arbiter 
Arbiter block simply watches over incoming signals from demodulator, and identifies if it contains PLCP or PSDU. If it is identified as PLCP, Arbiter block informs De-Puncturing block to send data to HeaderExtractor. Otherwise, De-Puncturing re-order the input bits with dummy bits, and then passes data to path metric.  

#### HeaderExtractor
HeaderExtractor is a simple Viterbi decoder that is specifically designed to decode PLCP and extract header information. This block computes branch metric and path metric blocks and then stores survival path into SRAM. Storing survival path into SRAM takes 24 clock cycles and decoding takes another 24 clock cycles (PLCP header is 48 bits coded with 1/2 coding rate and BPSK modulation). HeaderExtractor is designed to decode data within 48 clock cycles since the next PSDU OFDM symbol will be available 62 clock cycles after PCLP header is received. 

#### Path Metric + Branch Metric 
Path metric block and branch metric block calculate the accumulated path metric over trellis. Inside of branch metric module, it instantiates an object called "Trellis". The "Trellis" object maps output values and next states as a function of input and current states, which eases the branch metric calculation. 
The accumulated path metric and calculated survival path information will be tossed to "Traceback" module, which performs sliding-window Viterbi Decoding. 

#### Traceback 
Traceback module performs sliding-window Viterbi decoding. Sliding window viterbi decoder uses the fact that the survival paths merge after 5*K sequence, and hence it can reduce required the memory size. The module receives data from inSP port every clock cycle and stores it into SRAM. The first decode must wait for D+L cycles to account for survival path memory length. After D+L cycles, traceback loads survival path from SRAM and starts decoding. Once the first decoding is performed, then it starts decoding every D clock cycles. To perform simultaneous writing and decoding, the module requires (L-2)/D + 2 read ports for SRAM.   

## Parameters
* k: Int = number of input ports 
* n: Int = number of output ports. 
  + Coding rate is defined as 'k/n'
* K: Int = constraint length (depth of convolutional coding).
  + m: Int = number of memory elements. This is defined as m = K - 1
* D: Int = decode length.
* L: Int = trace length for sliding window Viterbi decoder. This is usually defined as 5*K 
* genPolynomial: List[Int] = generator polynomial for convolutional coding. 
  + Default: (7, 5) 
* protoBitsWidth: Int = total bit width for FixedPoint. This FixedPoint is the input type for soft-input Viterbi decoder. 
* softDecision: Boolean = If enabled, Viterbi Decoder use soft decision branch metric
* FFTPoint: number of clock cycles for one OFDM symbol
* bitsWidth: length of input sequence that demodulator delivers per clock cycle
* pmBits: Int = bit width for path metric registers. Default is 5 (maximum path metric can go up to 31)
* BMoutdec: define branch-metric output type. FixedPoint for soft-decoding and UInt for hard-decoding
* BMout: This is obsolete. Currently not being used. 
* pmBitType: type of pmRegs. FixedPoint for soft-decoding and UInt for hard-decoding 


## Inputs
* in: Flipped(Decoupled(BitsBundle(params)))
  + BitsBundle includes three signals: 
      + pktStart: Bool()
      + pktEnd: Bool()
      + bits: Vec(params.bitsWidth, params.protoBits.cloneType) 
   
## Outputs
* Decoupled(Vec(params.D, UInt(params.k.W))) 
  + D bits are decoded every D clock cycles 
  + Output is hard-coded 