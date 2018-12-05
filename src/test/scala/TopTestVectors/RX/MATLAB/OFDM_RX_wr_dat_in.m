clear all
close all

NLOP = 2;    % number of loop
NFFT = 64;      % Number of FFT points
NC   = 48;      % Number of subcarriers
NDS  = 2;        % Number of Data symbol per frame
NS   = NDS*NLOP;   % number of symbols
NP   = 4;        % Number of pilots in symbol –21, –7, 7, and 21
CP   = 16;       % cyclic prefix length
PRE  = 4;        % preamble symbol = 2

N = 64;
M = N/2;

SNR = 25;
FOFF = 0;
toff = 129;
tcor = toff+33+3*M;

%OFDM TX Create NLOP frames for simulation ================================
%data
bit_symbols = round(3*rand(NC, NS));

%QPSK =================================================================
QPSK    = 2.*mod(bit_symbols,2)-1 + 1i *(2.*floor(bit_symbols/2)-1);
QPSK    = QPSK *(1/sqrt(2));   
dat_mod = QPSK;

% insert subcarriers & pilots =============================================
% pilots in symbol –21, –7, 7, and 21======================================

pilots_802_11;
Pil = repmat(pils(:,1:NDS),1,NLOP);
symbol = zeros(NFFT,NS);
symbol(1,:)     = zeros(1,NS);
symbol(2:7,:)   = dat_mod(1:6, :);
symbol(8,:)     = Pil(1,NS);
symbol(9:21,:)  = dat_mod(7:19, :);
symbol(22,:)    = Pil(2,NS);
symbol(23:27,:) = dat_mod(20:24, :);
symbol(39:43,:) = dat_mod(25:29, :);
symbol(44,:)    = Pil(3,NS);
symbol(45:57,:) = dat_mod(30:42, :);
symbol(58,:)    = Pil(4,NS);
symbol(59:64,:) = dat_mod(43:48, :);

%IFFT =================================================================
tx_d =  ifft(symbol, NFFT, 1);

%Add CP ===============================================================
tx_d = [tx_d(NFFT-CP+1: NFFT,:); tx_d];

%Add Preamble =========================================================
tx_out = zeros((NFFT+CP), (PRE + NDS)*NLOP);

preamble_802_11;   
preamble_nor = [short_pre long_pre]; 
preamb = reshape(preamble_nor, NFFT+CP, PRE);

for ii = 0:NLOP -1,
    for jj = 1:PRE,
        tx_out(:,(PRE + NDS)*ii+jj) = preamb(:,jj);
    end
    %tx_out(:,(PRE + NDS)*ii+2) = preamb(:,2);
    if (NDS ~=0 )
        for jj = 1:NDS,
            tx_out(:,(PRE + NDS)*ii+PRE+jj) = tx_d(:,ii*NDS+jj);            
        end
    end
end
tx_out = reshape(tx_out, (NFFT+CP)*(PRE + NDS)*NLOP,1);
%==========================================================================   

%frequency offset adding ==============================================
n=0:(CP+NFFT)*(PRE + NDS)-1;
freoffs = exp(1i*2*pi*FOFF*(n.'./NFFT));    
tx_temp = reshape(tx_out, (CP+NFFT)*(PRE + NDS), NLOP);
tx_temp = tx_temp .* repmat(freoffs,1,NLOP);   
tx_out  = reshape(tx_temp,1,length(tx_out));

%AWGN channel simulation ==============================================
%rx_in = tx_out;  
rx_in = reshape(tx_out,(CP+NFFT)*(PRE + NDS), NLOP);
%rx_in = [rx_in(length(rx_in)- toff + 1 : length(rx_in)) rx_in(1:length(rx_in))];
toff_mat = zeros(toff,NLOP);
rx_in = [toff_mat; rx_in];
rx_in = reshape(rx_in,1,((CP+NFFT)*(PRE + NDS) + toff) * NLOP);
rx_in = awgn(rx_in ,SNR,'measured');   
rx_in = 0.9*(rx_in ./ max([max(real(rx_in)) max(imag(rx_in))]));
%rx_in = rx_in .*2;

%write data to file =======================================================
fid = fopen('OFDM_RX_bit_symbols.txt', 'w');
fprintf(fid, '%d ', bit_symbols);
fclose(fid);

Len = length(rx_in);
fid = fopen('OFDM_RX_datin_Re.txt', 'w');
fprintf(fid, '%f ', real(rx_in));
fclose(fid);
fid = fopen('OFDM_RX_datin_Im.txt', 'w');
fprintf(fid, '%f ', imag(rx_in));
fclose(fid);

datin_rtl = rx_in(1:Len) .*(2^15);
datin_Re = typecast(int16(real(datin_rtl)),'uint16');
datin_Im = typecast(int16(imag(datin_rtl)),'uint16');

SNR_w = round(SNR);
if (SNR >15), SNR_w = 15; end
Flen = toff + (NFFT+CP) *(PRE+NDS);
fid = fopen('RTL_OFDM_RX_datin_len.txt', 'w');
fprintf(fid, '%d %d %d %d', NLOP, Flen, SNR_w, toff);
fclose(fid);
fid = fopen('RTL_OFDM_RX_datin_Re.txt', 'w');
fprintf(fid, '%4x ', datin_Re);
fclose(fid);
fid = fopen('RTL_OFDM_RX_datin_Im.txt', 'w');
fprintf(fid, '%4x ', datin_Im);
fclose(fid);

% data cofficient for system synthesis ==================================== 

known_coeff = 2*(imag(long_pre(1:64))<0) + 1*(real(long_pre(1:64)<0));
known_coeff_rtl = typecast(int8(known_coeff(1:64)),'uint8');
fid = fopen('../MY_SOURCES/Synch_known_coeff_802_11.txt', 'w');
fprintf(fid, '%x ', known_coeff_rtl);
fclose(fid);

known_coeff = 2*(imag(long_sym([2:27, 39:64]))<0) + 1*(real(long_sym([2:27, 39:64]))<0);
known_coeff_rtl = typecast(int8(known_coeff),'uint8');
fid = fopen('../MY_SOURCES/ChEstEqu_lpre.txt', 'w');
fprintf(fid, '%x ', known_coeff_rtl);
fclose(fid);