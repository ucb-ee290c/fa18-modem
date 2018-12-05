Demodulator is comprised of hard demodulator and soft demodulator.
1)Hard Demodulator
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

2)Soft Demodulator

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
