close all

NLOP = 1;    % number of loop
NFFT = 64;      % Number of FFT points
NC   = 48;      % Number of subcarriers
NDS  = 2;        % Number of Data symbol per frame
NS   = NDS*NLOP;   % number of symbols
NP   = 4;        % Number of pilots in symbol –21, –7, 7, and 21
CP   = 16;       % cyclic prefix length
PRE  = 4;        % preamble symbol = 2

N = 64;
M = N/4;
L = 16;
C = 2*M; %length of computed received samples for Mp

% Read data in ============================================================
Para_fid = fopen('RTL_OFDM_RX_datin_len.txt', 'r');
Para = fscanf(Para_fid, '%d ');
NLOP = Para(1);
Flen  = Para(2);
SNR  = Para(3);
toff = Para(4);
fclose(Para_fid);

t_peak = toff + 16 + 3 * 16;
t_cor = toff + (PRE-1)*(NFFT+CP)+1;

NDS = (Flen-toff)/(NFFT+CP) - PRE; %number of Data symbol excluding preamble

datin_fid = fopen('OFDM_RX_bit_symbols.txt', 'r');
bit_symbols_in = fscanf(datin_fid, '%d ');
fclose(datin_fid);

datin_fid = fopen('OFDM_RX_datin_Re.txt', 'r');
dat_Re = fscanf(datin_fid, '%f ');
fclose(datin_fid);

datin_fid = fopen('OFDM_RX_datin_Im.txt', 'r');
dat_Im = fscanf(datin_fid, '%f ');
fclose(datin_fid);

rx_in =  dat_Re + 1i*dat_Im;
rx_in = rx_in.';
rx_in = reshape(rx_in, Flen, NLOP);

% % Read data out of RTL ====================================================
datout_fid = fopen('RTL_OFDM_RX_datout.txt', 'r');
OFDM_RX_datout_rtl = fscanf(datout_fid, '%d ');
fclose(datout_fid);

datout_fid = fopen('RTL_OFDM_RX_Synch_datout_Re.txt', 'r');
Synch_datout_Re_rtl = fscanf(datout_fid, '%d ');
fclose(datout_fid);
datout_fid = fopen('RTL_OFDM_RX_Synch_datout_Im.txt', 'r');
Synch_datout_Im_rtl = fscanf(datout_fid, '%d ');
fclose(datout_fid);
Synch_datout_rtl = (Synch_datout_Re_rtl./2^14) + 1i*(Synch_datout_Im_rtl./2^14);


% datout_fid = fopen('RTL_OFDM_RX_FreComp_datout_Re.txt', 'r');
% FreComp_datout_Re_rtl = fscanf(datout_fid, '%d ');
% fclose(datout_fid);
% datout_fid = fopen('RTL_OFDM_RX_FreComp_datout_Im.txt', 'r');
% FreComp_datout_Im_rtl = fscanf(datout_fid, '%d ');
% fclose(datout_fid);
% FreComp_datout_rtl = (FreComp_datout_Re_rtl./2^14) + 1i*(FreComp_datout_Im_rtl./2^14);
% 
% datout_fid = fopen('RTL_OFDM_RX_FreComp_phase_rot.txt', 'r');
% FreComp_phase_rot_rtl = fscanf(datout_fid, '%d ');
% fclose(datout_fid);
% Phase_rot_comp_rtl = FreComp_phase_rot_rtl ./ 2^13;
% 
datout_fid = fopen('RTL_OFDM_RX_RemoveCP_datout_Re.txt', 'r');
RemoveCP_datout_Re_rtl = fscanf(datout_fid, '%d ');
fclose(datout_fid);
datout_fid = fopen('RTL_OFDM_RX_RemoveCP_datout_Im.txt', 'r');
RemoveCP_datout_Im_rtl = fscanf(datout_fid, '%d ');
fclose(datout_fid);
RemoveCP_datout_rtl = (RemoveCP_datout_Re_rtl./2^14) + 1i*(RemoveCP_datout_Im_rtl./2^14);


datout_fid = fopen('RTL_OFDM_RX_FFT_datout_Re.txt', 'r');
FFT_datout_Re_rtl = fscanf(datout_fid, '%d ');
fclose(datout_fid);
datout_fid = fopen('RTL_OFDM_RX_FFT_datout_Im.txt', 'r');
FFT_datout_Im_rtl = fscanf(datout_fid, '%d ');
fclose(datout_fid);
FFT_datout_rtl = (FFT_datout_Re_rtl./2^11) + 1i*(FFT_datout_Im_rtl./2^11);

datout_fid = fopen('RTL_OFDM_RX_iCFO_EstComp_datout_Re.txt', 'r');
iCFO_EstComp_datout_Re_rtl = fscanf(datout_fid, '%d ');
fclose(datout_fid);
datout_fid = fopen('RTL_OFDM_RX_iCFO_EstComp_datout_Im.txt', 'r');
iCFO_EstComp_datout_Im_rtl = fscanf(datout_fid, '%d ');
fclose(datout_fid);
iCFO_EstComp_datout_rtl = (iCFO_EstComp_datout_Re_rtl./2^11) + 1i*(iCFO_EstComp_datout_Im_rtl./2^11);

datout_fid = fopen('RTL_OFDM_RX_Ch_EstEqu_datout_Re.txt', 'r');
Ch_EstEqu_datout_Re_rtl = fscanf(datout_fid, '%d ');
fclose(datout_fid);
datout_fid = fopen('RTL_OFDM_RX_Ch_EstEqu_datout_Im.txt', 'r');
Ch_EstEqu_datout_Im_rtl = fscanf(datout_fid, '%d ');
fclose(datout_fid);
Ch_EstEqu_datout_rtl = (Ch_EstEqu_datout_Re_rtl./2^6) + 1i*(Ch_EstEqu_datout_Im_rtl./2^6);

datout_fid = fopen('RTL_OFDM_RX_PhaseTrack_datout_Re.txt', 'r');
PhaseTrack_datout_Re_rtl = fscanf(datout_fid, '%d ');
fclose(datout_fid);
datout_fid = fopen('RTL_OFDM_RX_PhaseTrack_datout_Im.txt', 'r');
PhaseTrack_datout_Im_rtl = fscanf(datout_fid, '%d ');
fclose(datout_fid);
PhaseTrack_datout_rtl = (PhaseTrack_datout_Re_rtl./2^0) + 1i*(PhaseTrack_datout_Im_rtl./2^0);

% Simulate with data in ===================================================
datin_fid = fopen('../MY_SOURCES/Synch_known_coeff_802_11.txt', 'r');
known_coeff = fscanf(datin_fid, '%d ');
fclose(datin_fid);
known_coeff = repmat(known_coeff,1,NLOP);

preamble_802_11;   
preamble_nor = [short_pre long_pre]; 

SynLen = toff+ PRE * (NFFT+CP);

P_sim           = zeros(SynLen,NLOP);
P_Abs_sim    = zeros(SynLen,NLOP);
P_diff          = zeros(SynLen,NLOP);
ACR_Mult        = zeros(SynLen,NLOP);
ACR_Mult_d96   = zeros(SynLen,NLOP);
rx_in_d16       = [zeros(16,NLOP); rx_in];

R_sim = zeros(SynLen,NLOP);  
R_thr_sim = zeros(SynLen,NLOP);  
rx_AbsSqr = abs(rx_in(1:SynLen,:)).^2;
rx_AbsSqr_d96 = [zeros(96,NLOP); abs(rx_in(1:SynLen,:)).^2];

CR_Reg     = zeros(1,63);
Pre_ref =  1 - 2.*mod(known_coeff,2)  - 1i *(1- 2.*floor(known_coeff/2));
rx_in_sb =  ((1-2*(real(rx_in)<0))+ 1i * (1-2*(imag(rx_in)<0)));

for d = 1: SynLen,
    ACR_Mult(d,:) = conj(rx_in(d,:)) .* rx_in_d16(d,:);    
    if (d==1),  P_sim(d,:) = ACR_Mult(d,:);        
    else        P_sim(d,:) =  P_sim(d-1,:) + ACR_Mult(d,:) - ACR_Mult_d96(1,:);
    end  
    for m = 1:96,
        ACR_Mult_d96(m,:) = ACR_Mult_d96(m+1,:);                     
    end
    ACR_Mult_d96(96,:)  = ACR_Mult(d,:);
    P_Abs_sim(d,:)   = abs(P_sim(d,:));
    
    if (d<97),  P_diff(d,:) = P_Abs_sim(d,:);        
    else        P_diff(d,:) = P_Abs_sim(d,:) - P_Abs_sim(d-96,:);
    end

    if (d==1),  R_sim(d,:) =  rx_AbsSqr(d,:);        
    else        R_sim(d,:) =  R_sim(d-1,:) + rx_AbsSqr(d,:) - rx_AbsSqr_d96(d,:);
    end  
    R_thr_sim(d,:) = R_sim(d,:)/2;
    
     
    %cross correlation
    CR_out_sim(d) = (rx_in_sb(d) * Pre_ref(64))/2 + CR_Reg(63);
    for m = 63:-1:2,
       CR_Reg(m) =  (rx_in_sb(d) * Pre_ref(m))/2 + CR_Reg(m-1);                     
    end   
    CR_reg(1) = (rx_in_sb(d) * Pre_ref(1))/2;
end
% figure (100)
% plot(1:SynLen,P_AbsSqr_sim,'.-b');
% hold on
% plot(1:SynLen,R_thr_sim,'.-r');
% figure (101)
% plot(1:SynLen,abs(CR_out_sim),'o-b');
% figure (102)
% plot(1:SynLen,abs(CR_out_sim)./max(CR_out_sim),'o-b');
% hold on
% plot(1:SynLen,P_AbsSqr_sim ./ max(P_AbsSqr_sim),'o-g');
% plot(1:SynLen,P_diff ./ max(P_AbsSqr_sim),'.-c');
% plot(1:SynLen,R_thr_sim./ max(P_AbsSqr_sim),'o-k');
% plot(1:SynLen,rx_in(1:SynLen) ./ max(rx_in(1:SynLen)),'o-r');



% Timing synchronisation toff_est = t_cor =================================
% & remove short preamble 
Synch_datout = rx_in(t_cor:(Flen),:);
foff_est_sim = angle(P_sim(t_peak,1))/M;
nn = ((0:(NFFT+CP)*(NDS+1)-1))';
nn = repmat(nn,1,NLOP);
Phase_rot_comp = -(foff_est_sim).*nn;
FreComp_datout = Synch_datout .* (exp(1i*Phase_rot_comp));
Synch_datout_sim = reshape(FreComp_datout, (NFFT+CP)*(NDS+1)*NLOP, 1);

% Remove Cyclic Prefix  ===================================================
RemoveCP_datout  = reshape(FreComp_datout,(NFFT+CP),NDS+1,NLOP);
RemoveCP_datout(1:CP,:,:)= [];
RemoveCP_datout  = reshape(RemoveCP_datout,(NDS+1)*NFFT,NLOP);
RemoveCP_datout_sim = reshape(RemoveCP_datout,(NDS+1)*NFFT*NLOP,1);

% FFT for OFDM symbols demodulation =======================================
FFT_symbol = reshape(RemoveCP_datout_sim,NFFT,NDS+1, NLOP);
FFT_symbol = fft(FFT_symbol,NFFT,1);
FFT_datout = reshape(FFT_symbol,(NDS+1)*NFFT,NLOP);
FFT_datout_sim = reshape(FFT_datout,(NDS+1)*NFFT*NLOP,1);

% Rotate symbol to compensate integer CFO =================================
% n_rot = -preoff + noff;
% OFDM_symbol_IFO_Comp = zeros(NFFT, NDS+1, NLOP);
% OFDM_symbol_IFO_Comp(1:256-n_rot,:,:) = FFT_symbol(n_rot+1:256,:,:);
% OFDM_symbol_IFO_Comp(256-n_rot+1:256,:,:) = FFT_symbol(1:n_rot,:,:);
% OFDM_symbol = reshape(OFDM_symbol_IFO_Comp,(NDS+1)*256,NLOP);
% iCFO_EstComp_datout_sim = reshape(OFDM_symbol, (NDS+1)*NFFT*NLOP,1);

% Channel Estimation & Compensation =======================================
preamble_802_11;
FFT_datout = reshape(FFT_datout,NFFT,NDS+1, NLOP);
Ch_EstEqu_datout = zeros(NDS*(NC+NP),NLOP);
for ii = 1:NLOP,
    ch_est = long_sym .* conj(FFT_datout(:,1,ii));
    ch_datout = FFT_datout(:,2:NDS+1,ii) .* repmat(ch_est,1,NDS);
    ch_datout(28:38,:)=[];
    ch_datout(1,:)=[];
    Ch_EstEqu_datout(:,ii) = reshape(ch_datout,(NC+NP)*NDS,1);                         
end
Ch_EstEqu_datout_sim = reshape(Ch_EstEqu_datout,NDS*(NC+NP)*NLOP,1);
%==========================================================================

Pil_ref = [1 -1 1 1].';
Pil_ref = repmat(Pil_ref,1,NDS);
Pil_ind = [7 21 32 46].';
PhaseTrack_datin = reshape(Ch_EstEqu_datout,(NC+NP),NDS,NLOP);
PhaseTrack_datout = zeros(NDS*(NC+NP),NLOP);
for ii = 1:NLOP,
    Rx_Pil = PhaseTrack_datin(Pil_ind,:,ii);
    %Rx_Pil = Rx_Pil ./ (abs(Rx_Pil));
    Ph_est = 1/4*(sum(Rx_Pil .* Pil_ref));
    Ph_comp = ones((NC+NP),1) * conj(Ph_est);
    PhaseTrack_comp = PhaseTrack_datin(:,:,ii) .* Ph_comp;
    PhaseTrack_datout(:,ii) = reshape(PhaseTrack_comp,(NC+NP)*NDS,1);
end
PhaseTrack_datout_sim = reshape(PhaseTrack_datout, NDS*(NC+NP)*NLOP,1);

%==========================================================================
% QPSK symbol demodulation ================================================
bit_symbols_out = 2*(imag(PhaseTrack_datout_sim)>0) + 1*(real(PhaseTrack_datout_sim)>0);
bit_symbols_out = reshape(bit_symbols_out,(NC+NP),NDS,NLOP);
bit_symbols_out(Pil_ind,:,:) =[];
bit_symbols_sim = reshape(bit_symbols_out, NDS*(NC)*NLOP,1);

OFDM_RX_datout_rtl = reshape(OFDM_RX_datout_rtl,(NC+NP),NDS,NLOP);
OFDM_RX_datout_rtl(Pil_ind,:,:) =[];
OFDM_RX_datout_rtl = reshape(OFDM_RX_datout_rtl, NDS*(NC)*NLOP,1);
bit_symbols_rtl = OFDM_RX_datout_rtl;
%bit_symbols_sim = reshape(bit_symbols_sim,NDS*size(bit_symbols_sim,1),1);

% Compare Simulation vs RTL ===============================================
figure(1)
hold on 
plot(1:length(Synch_datout_sim), real(Synch_datout_sim),'o-b');
plot(1:length(Synch_datout_rtl), real(Synch_datout_rtl),'.-r');
title ('Synch\_datout\_sim vs Synch\_datout\_rtl')
legend('Synch\_datout\_sim','Synch\_datout\_rtl')
xlim([1 1000]);
hold off

figure(4)
hold on 
plot(1:length(RemoveCP_datout_sim), real(RemoveCP_datout_sim),'o-b');
plot(1:length(RemoveCP_datout_rtl), real(RemoveCP_datout_rtl),'.-r');
title ('RemoveCP\_datout\_sim vs RemoveCP\_datout\_rtl')
legend('RemoveCP\_datout\_sim', 'RemoveCP\_datout\_rtl')
hold off

figure(5)
hold on 
plot(1:length(FFT_datout_sim), real(FFT_datout_sim),'o-b');
plot(1:length(FFT_datout_rtl), real(FFT_datout_rtl),'.-r');
title ('FFT\_datout\_sim vs FFT\_datout\_rtl')
legend ('FFT\_datout\_sim','FFT\_datout\_rtl')
hold off

% figure(6)
% hold on 
% plot(1:length(iCFO_EstComp_datout_sim), real(iCFO_EstComp_datout_sim),'o-b');
% % plot(1:length(iCFO_EstComp_datout_rtl), real(iCFO_EstComp_datout_rtl),'.-r');
% title ('iCFO\_EstComp\_datout\_rtl vs iCFO\_EstComp\_datout\_sim')
% legend ('iCFO\_EstComp\_datout\_rtl','iCFO\_EstComp\_datout\_sim')
% hold off

figure(7)
hold on 
plot(1:length(Ch_EstEqu_datout_sim), real(Ch_EstEqu_datout_sim),'o-b');
plot(1:length(Ch_EstEqu_datout_rtl), real(Ch_EstEqu_datout_rtl),'.-r');
title ('Ch\_EstEqu\_datout\_sim vs Ch\_EstEqu\_datout\_rtl')
legend('Ch\_EstEqu\_datout\_sim', 'Ch\_EstEqu\_datout\_rtl')
hold off

figure(8)
hold on 
plot(1:length(PhaseTrack_datout_sim), real(PhaseTrack_datout_sim),'o-b');
plot(1:length(PhaseTrack_datout_rtl), real(PhaseTrack_datout_rtl),'.-r');
title ('PhaseTrack\_datout\_sim vs PhaseTrack\_datout\_rtl')
legend('PhaseTrack\_datout\_sim', 'PhaseTrack\_datout\_rtl')
hold off

figure(10)
hold on 
plot(1:length(bit_symbols_rtl), bit_symbols_rtl,'.-r');
plot(1:length(bit_symbols_in), bit_symbols_in,'o-b');
title ('bit\_symbols\_rtl vs bit\_symbols\_in')
legend('bit\_symbols\_rtl','bit\_symbols\_in')
hold off

figure(11)
hold on 
plot(1:length(bit_symbols_in), bit_symbols_in,'.-r');
plot(1:length(bit_symbols_sim), bit_symbols_sim,'o-b');
title ('bit\_symbols\_in vs bit\_symbols\_sim')
legend('bit\_symbols\_in','bit\_symbols\_sim')
ylim([-1 4]);
hold off

figure(12)
hold on 
plot(1:length(bit_symbols_sim), bit_symbols_sim,'o-b');
plot(1:length(bit_symbols_rtl), bit_symbols_rtl,'.-r');
title ('bit\_symbols\_sim vs bit\_symbols\_rtl')
legend('bit\_symbols\_sim', 'bit\_symbols\_rtl')
ylim([-1 4]);
hold off

% L_pre_pattern = abs((FFT_datout_sim(1:256)./6)).^2;
% figure(13)
% hold on 
% plot(1:length(peven), L_pre_pattern,'.-r');
% plot(1:length(peven), abs(peven).^2,'x-b');
% title ('L\_pre\_pattern vs abs(peven).^2')
% ylim([-1 5]);
% hold off




