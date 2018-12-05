Demodulator is comprised of hard demodulator and soft demodulator. 

1. 

 1.1 Basics of deinterleaver

The deinterleaver, which performs the inverse relation of interleaver, is also defined by two permutations.

The first permutation is defined by

i = s x floor(j/s) + (j + floor(16 x j/Ncbps)) mod s j= 0,1,..., Ncbps-1

s = max(Nbpsc/2,1)

The first permutation is defined by

k = 16 x i - (Ncbps -1)floor(16 x i/ Ncbps)

Here j is the index of the original received bit, i is the index after the first permutation, and k is the index after the 
second permutation.

 1.2 Basics of soft-demapper

The Log-Likelihood Ratio(LLR) of decision bi,q is defined as LLR(bi,q) = log(P[bi,q =1| r[i])/P[bi,q =0| r[i])). In our design, a sub-optimal simplified LLR is utilized. 

Therefore, LLR(bi,q) = (|Gch(i)|^2/4){min|y[i]-α|^2 - min|y[i]-β|^2}

α ε S_(I,k)^1, β ε S_(I,k)^0, S_(I,k)^0 is compised the symbols with a '0' in position (I,k) and S_(I,k)^1 is the complementary




2. 

2.1 Hard Demodulator

Hard demodulator includes 16QAM hard demodulator , QPSK hard demodulator and BPSK hard demodulator. 16 QAM hard demodulator 
consists of serilizer, deinterleaver and demapper. QPSK hard modulator consists of serilizer, deinterleaver and demapper. 
BPSK hard modulator merely consists of deinterleaver and demapper. 

Parameters:

`val bitsWidth: Int` width of  outputs

`val dataWidth: Int` width of  inputs

`val hsmod: Int` hard demodulator and soft demodulator select signal


IO:

Inputs:

Val mod_ctrl: UInt ` modulation scheme control signal

`val in.bits.iq: DspComplex`  input

`val in.bits.pktStart: Bool`  package start signal

`val in.bits.pktEnd: Bool`  package end signal

`val in.valid: Bool`  valid signal

`val out.ready: Bool`  ready signal

Outputs:

`val out.bits.bits: SInt`  demodulated output

`val out.bits.pktStart: Bool`  package start signal

`val out.bits.pktEnd: Bool`  package end signal

`val out.valid: Bool`  valid signal

`val in.ready: Bool`  ready signal

Parameters:

`val bitsWidth: Int` width of inputs and outputs

`val dataWidth: Int` width of  outputs

`val hsmod: Int` hard demodulator and soft demodulator select signal

 2.2 Soft Demodulator

Soft demodulator includes 16QAM Soft demodulator , QPSK soft demodulator and BPSK soft demodulator. 16 QAM soft demodulator
consists of serilizer, deinterleaver and demapper. QPSK soft modulator consists of serilizer, deinterleaver and demapper. 
BPSK soft modulator merely consists of deinterleaver and demapper. Our soft demodulation is based on sub-optimal simplified 
LLR, which significantly reduces the complexity of hardware and features negligible soft decoding performance loss.

IO:

Inputs:

Val mod_ctrl: UInt ` modulation scheme control signal

`val in.bits.iq: DspComplex`  input

`val in.bits.pktStart: Bool`  package start signal

`val in.bits.pktEnd: Bool`  package end signal

`val in.valid: Bool`  valid signal

`val out.ready: Bool`  ready signal

Outputs:

`val out.bits.bits: FixedPoint`  demodulated output

`val out.bits.pktStart: Bool`  package start signal

`val out.bits.pktEnd: Bool`  package end signal

`val out.valid: Bool`  valid signal

`val in.ready: Bool`  ready signal

Tests:

Include tests for bpsk, qpsk and qam16 hard and soft demodulator. Run `testOnly modem.demodulatorSpec `
