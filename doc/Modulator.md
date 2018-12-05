Modulator includes 16QAM modulator , QPSK modulator and BPSK modulator. 16 QAM modulator consists of deserilizer, interleaver 
and mapper. QPSK modulator consists of deserilizer, interleaver and mapper. BPSK modulator merely consists of interleaver and 
mapper. The Interleaver operates at an OFDM symbol level with a block size of (48, 96, or 192) bits. The Mapper also operates 
at the OFDM symbol level with a block size of (48, 96, or 192) bits. Each block is divided into sub-blocks at the OFDM 
sub-carrier level. Sub-blocks are of size (1, 2, or 4) bits. The Mapper  first converts each sub-block into a complex number 
representing BPSK, QPSK, or 16-QAM constellation points. Note that the modulation type may be different for the header and 
data parts of the message. The resulting 48 complex pairs are then normalized by KMOD. The interleaver is defined by a two step permutation. The first permutation ensures that adjacent coded bits are mapped onto nonadjacent subcarriers. The second ensures that adjacent coded bits are mapped alternately onto less and more significant bits of the constellation.Specifically, the first permutation is defined by the rule

i = (NCBPS/16)(k mod 16) + floor(k/16) k=0,1,...,NCBPS-1

The second permutation is defined by the rule

j = s x floor(i/s) + (i+NCBPS-floor(16 x i/NCBPS)) mode s i=0,1,...,NCBPS-1

s= max(NBPSC/2,1)

k is the index of the coded bit before first permutation; i is the index after the first permutation and before second permutation; j is the index after the second permutation.

Parameters:

`val bitsWidth: Int` width of inputs 

`val dataWidth: Int` width of  outputs

`val Ncbps: Int` the number of bits in a single OFDM symbol

`val Nbpsc: Int` coded bits per subcarrier

IO:

Inputs:

Val mod_ctrl: UInt ` modulation scheme control signal

`val in.bits.bits: UInt`   input

`val in.bits.pktStart: Bool`  package start signal

`val in.bits.pktEnd: Bool`  package end signal

`val in.valid: Bool`  valid signal

`val out.ready: Bool`  ready signal

Outputs:

`val out.bits.iq: DspComplex`  modulated output

`val out.bits.pktStart: Bool`  package start signal

`val out.bits.pktEnd: Bool`  package end signal

`val out.valid: Bool`  valid signal

`val in.ready: Bool`  ready signal 

Tests:

Include tests for bpsk, qpsk and qam16 modulator. Run `testOnly modem.modulatorSpec `
