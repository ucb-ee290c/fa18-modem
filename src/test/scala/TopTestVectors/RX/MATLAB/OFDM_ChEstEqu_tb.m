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

% Read data in ============================================================
Para_fid = fopen('RTL_ChEstEqu_datin_len.txt', 'r');
Para = fscanf(Para_fid, '%d ');
Flen  = Para(1);
fclose(Para_fid);

NDS = Flen/(NFFT) - 1; %number of Data symbol excluding preamble

datin_fid = fopen('ChEstEqu_sym_Re.txt', 'r');
ch_sym_Re = fscanf(datin_fid, '%f ');
fclose(datin_fid);
datin_fid = fopen('ChEstEqu_sym_Im.txt', 'r');
ch_sym_Im = fscanf(datin_fid, '%f ');
fclose(datin_fid);
ch_sym =  ch_sym_Re + 1i*ch_sym_Im;
ch_sym = ch_sym.';
ch_sym = reshape(ch_sym, NFFT, NDS+1);


datin_fid = fopen('ChEstEqu_datin_Re.txt', 'r');
dat_Re = fscanf(datin_fid, '%f ');
fclose(datin_fid);

datin_fid = fopen('ChEstEqu_datin_Im.txt', 'r');
dat_Im = fscanf(datin_fid, '%f ');
fclose(datin_fid);

ch_datin =  dat_Re + 1i*dat_Im;
ch_datin = ch_datin.';
ch_datin = reshape(ch_datin, NFFT, NDS+1);

% Read data out of RTL ====================================================
datout_fid = fopen('RTL_ChEstEqu_datout_Re.txt', 'r');
ChEstEqu_datout_Re_rtl = fscanf(datout_fid, '%d ');
fclose(datout_fid);
datout_fid = fopen('RTL_ChEstEqu_datout_Im.txt', 'r');
ChEstEqu_datout_Im_rtl = fscanf(datout_fid, '%d ');
fclose(datout_fid);
ChEstEqu_datout_rtl = (ChEstEqu_datout_Re_rtl./2^14) + 1i*(ChEstEqu_datout_Im_rtl./2^14);

% Simulate with data in ===================================================
ch_sym = reshape(ch_sym,NFFT,NDS+1);
ch_sym(:,1) =[];
ch_sym(28:38,:)=[];
ch_sym(1,:) = [];
ch_sym = reshape(ch_sym,1,(NC+NP)*NDS);

preamble_802_11;
ch_est = long_sym .* conj(ch_datin(:,1));
ch_datout = ch_datin(:,2:NDS+1) .* repmat(ch_est,1,NDS);
ch_datout(28:38,:)=[];
ch_datout(1,:)=[];
ChEstEqu_datout_sim = reshape(ch_datout,1,(NC+NP)*NDS);
% Compare Simulation vs RTL ===============================================
figure(1)
hold on 
plot(1:length(ChEstEqu_datout_sim), angle(ChEstEqu_datout_sim),'o-b');
plot(1:length(ChEstEqu_datout_rtl), angle(ChEstEqu_datout_rtl),'.-r');
title ('ChEstEqu\_datout\_sim vs ChEstEqu\_datout\_rtl')
legend('ChEstEqu\_datout\_sim','ChEstEqu\_datout\_rtl')
xlim([1 1000]);
hold off

figure(2)
hold on 
plot(1:length(ChEstEqu_datout_sim), angle(ChEstEqu_datout_sim),'o-b');
plot(1:length(ch_sym), angle(ch_sym),'.-r');
title ('ChEstEqu\_datout\_sim vs ch\_sym\_datin')
legend('ChEstEqu\_datout\_sim','ch\_sym\_datin')
xlim([1 1000]);
hold off

