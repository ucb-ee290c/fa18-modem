%this simulation is for IEEE802.11
clear all
%close all

%dur  = 3.2e-6;  
NLOP = 10000;        % number of loop
NFFT = 64;          % Number of FFT points
NC   = 48;          % Number of subcarriers
NDS  = 2;           % Number of Data symbol per frame
NS   = NDS*NLOP;    % number of symbols
NP   = 4;           % Number of pilots in symbol –21, –7, 7, and 21
CP   = 16;          % cyclic prefix length
PRE  = 4;           % preamble symbol = 2

%nSUI = 1:1:6;
M = 64;
L = 16;
C = 1*M; %length of computed received samples for Mp
LCR = 80; %length of computed received samples for correlation of FTO
Q = 32;


toff = 31;
tcor = toff  + (NFFT+CP)*2 + LCR;

testcnt = 0;
    
Mp_fail = zeros(1,20);
Mp_miss = zeros(1,20);
Mp_correct = zeros(1,20);

for FOFF = 0.1,
for nSUI = 0,
for SNR  = 0:15,
    testcnt = testcnt + 1;
%OFDM TX Create NLOP frames for simulation ================================
    %data
    bit_symbols = round(3*rand(NC, NS));
    %QPSK =================================================================
    QPSK = 2.*mod(bit_symbols,2) - 1 + 1i *(2.*floor(bit_symbols/2)-1);
    QPSK    = QPSK *(1/sqrt(2));   
    dat_mod = QPSK;
    %insert subcarriers & pilots ==========================================
    % pilot ===============================================================
    %Pil = pils(:,1:NS);
%     Pil = zeros(8,NS);
%     symbol = [ zeros(1,NS); QPSK(1  :12, :); ...
%                   Pil(1,:); QPSK(13 :36, :); ...
%                   Pil(2,:); QPSK(37 :60, :); ...
%                   Pil(3,:); QPSK(61 :84, :); ...
%                   Pil(4,:); QPSK(85 :96, :); ...
%                  zeros(NFFT-NC-NP-1,NS); ...
%                             QPSK(97 :108,:); ...    
%                   Pil(5,:); QPSK(109:132,:); ...
%                   Pil(6,:); QPSK(133:156,:); ...
%                   Pil(7,:); QPSK(157:180,:); ...
%                   Pil(8,:); QPSK(181:192,:); ];
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
    tx_d =  ifft(symbol, NFFT);
    
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
    
    tx_out = reshape(tx_out, 1, (NFFT+CP)*(PRE + NDS)*NLOP);
%==========================================================================   
       
    n=1:(CP+NFFT)*(PRE + NDS);
    freoffs = exp(1i*2*pi*FOFF*(n.'./NFFT));
    %freoffs_cp = [ freoffs(NFFT-CP+1: NFFT); freoffs];
    tx_temp = reshape(tx_out, (CP+NFFT)*(PRE + NDS), NLOP);
    tx_temp = tx_temp .* repmat(freoffs,1,NLOP);   
    tx_out  = reshape(tx_temp,1,length(tx_out));
    
    %SUI channel simulation ===============================================
    if (nSUI ~= 0)
        ['Simulate channel SUI=' num2str(nSUI) '==============================']
        tx_out = reshape(tx_out, (NFFT+CP)*(PRE + NDS),NLOP);
        rx_in  = zeros((NFFT+CP)*(PRE + NDS),NLOP);
        for ii= 1:NLOP,
            channel = channelSUI(nSUI,L/NFFT,20);
            rx_in(:,ii) = SUIChan_SIM(channel(:,1),tx_out(:,ii)')';
        end
        rx_in = reshape(rx_in, 1, (NFFT+CP)*(PRE + NDS)*NLOP);
    else
        ['Simulate channel AWGN with SNR=' num2str(SNR) '=====================']
        rx_in = tx_out;      
    end
    
% Receiver side and simulation of synchronisation==========================
        
    rx_in = reshape(rx_in,  (NFFT+CP)*(PRE + NDS),NLOP);
    % add toff to OFDM frame ==========================================
    rx_in = [zeros(toff,NLOP); rx_in];
    %AWGN channel simulation ==========================================
    rx_in = reshape(rx_in, 1, (toff + (NFFT+CP)*(PRE + NDS))*NLOP);
    rx_in = awgn(rx_in ,SNR,'measured'); 
    rx_in = reshape(rx_in,  toff + (NFFT+CP)*(PRE + NDS),NLOP);
        
    Flen = toff+ (NFFT+CP)*(PRE + NDS);
    fail_cnt = 0;
    
    Slen = toff + ((NFFT+CP)*PRE)+64;
    
    Pp = zeros(Slen,NLOP);  
    Rp = zeros(Slen,NLOP);  
    Mp = zeros(Slen,NLOP);
    Ep = zeros(Slen,NLOP);
    
    XCR_sb = zeros(Slen,NLOP);

    rx_syn = [rx_in; zeros(2*M,NLOP)];
    %known_pre = pre64;
    rx_syn = rx_syn ./ max([max(real(rx_syn)) max(imag(rx_syn))]);
    
    ['Calculate Metric']
    abs_pre = abs([long_pre(1:LCR)]).^2;
    known_coeff = abs_pre;
    pre_sb  = ((1-2*(real(long_pre(1:LCR))<0))+ 1i * (1-2*(imag(long_pre(1:LCR))<0)));
    %known_coeff = round((abs_pre./max(abs_pre)).*4)./4;
    
    round_coff = 2^Q;
    rx_abs = abs(rx_syn).^2;
    rx_abs_q = fix(rx_abs .* round_coff) ./round_coff;
    rx_in_sb =  ((1-2*(real(rx_syn)<0))+ 1i * (1-2*(imag(rx_syn)<0)));

    for d = 1: Slen,
            Pp(d,:) = zeros(1,NLOP); 
            for ii = 1:C,
                if (d-M-C+ii>1), P_q = conj(rx_syn(d-M-C+ii,:)) .* rx_syn(d-C+ii,:);
                else             P_q = zeros(1,NLOP); 
                end
                P_q = fix(P_q * round_coff) / round_coff;
                Pp(d,:) = Pp(d,:) + P_q;
            end

            Rp(d,:) = zeros(1,NLOP); 
            for ii = 1:C,
                if (d-C+ii > 1), R_q = rx_abs_q(d-C+ii,:);% * known_coeff(1+ii);
                else             R_q = zeros(1,NLOP); 
                end
                R_q = fix(R_q * round_coff) / round_coff;
                Rp(d,:) = Rp(d,:) + R_q;
            end
            Pp_Re = abs(real(Pp(d,:)));
            Pp_Im = abs(imag(Pp(d,:)));
            if(Pp_Re > Pp_Im),
                Pp_abs = Pp_Re + Pp_Im / 2;
            else
                Pp_abs = Pp_Im + Pp_Re / 2;
            end
            Mp(d,:) = (Pp_abs ./ Rp(d,:));  
            
            Ep(d,:) = 0;       
           
            for ii = 1:LCR,
                if (d - LCR +ii > 1),  E_q = rx_abs_q(d - LCR +ii,:) .* (known_coeff(ii)*ones(1,NLOP));               
                else                   E_q = zeros(1,NLOP);
                end
                E_q = fix(E_q * round_coff) / round_coff;
                Ep(d,:) = Ep(d,:) + E_q;
            end
            
            XCR_sb(d,:) = 0;
            for ii = 1:LCR,
                if (d - LCR +ii > 1),  XCR_acc = rx_in_sb(d - LCR +ii,:) .* (conj(pre_sb(ii)) *ones(1,NLOP));               
                else                   XCR_acc = zeros(1,NLOP);
                end
                XCR_sb(d,:) = XCR_sb(d,:) + XCR_acc;
            end
%         end
    end    
    Ep(3:Slen-2) = (2*Ep(3:Slen-2) - (Ep(1:Slen-4)+Ep(5:Slen)));
    Ep(Slen-1:Slen) = [0 0];
    ['Evaluate failure rate']      
    Mp_fail(testcnt) = 0;
    Mp_miss(testcnt) = 0;
    Mp_correct(testcnt) = 0;
    
    sb_fail(testcnt) = 0;
    sb_miss(testcnt) = 0;
    sb_correct(testcnt) = 0;
    for jj = 1:NLOP,
        mis=1;
        np = 8;
        for ii = 1:tcor +2,
            if ((Mp(ii,jj)> 0.5)&&(Rp(ii,jj) > 2)),
%                 ii
                np = np -1;
            end
            if(np ==0),
                
                %[val, id] = max(Rp_resh(ii:ii+2*CP-8,jj));
                [val, id] = max(Ep(ii+128:ii+192,jj));
%                 id
                if (id <= tcor-127 - ii), Mp_correct(testcnt) = Mp_correct(testcnt) + 1;
                else           Mp_fail(testcnt) = Mp_fail(testcnt) + 1; 
%                     
%                     Len_p = Slen; % length of plotting                   
%                     figure(jj)
%                     plot(1:Len_p, (abs(Pp(:,jj)) ./ max(abs(Pp(:,jj)))),'o-b')
%                     hold on 
%                     plot(1:Len_p, (Rp(:,jj) ./ max(Rp(:,jj))),'.-r')
%                     plot(1:Len_p, (Ep(:,jj) ./ max(Ep(:,jj))),'.-g')
%                     legend('Pp', 'Rp', 'Ep')
%                     hold off
                    [jj ii    id]
                end
                
                [val, id] = max(abs(XCR_sb(ii+128:ii+192,jj)));

                if (id == tcor-127 - ii), sb_correct(testcnt) = sb_correct(testcnt) + 1;
                else           sb_fail(testcnt) = sb_fail(testcnt) + 1; 
                end
                
                mis = 0;
                break;
            end          
        end
        if (mis == 1), Mp_miss(testcnt) = Mp_miss(testcnt) + 1; end        
    end 
    end  
end
end

jj= 1;
Len_p = Slen; % length of plotting
% Ep(3:Len_p-2) = (2*Ep(3:Len_p-2) - (Ep(1:Len_p-4)+Ep(5:Len_p)));
figure(1)
plot(1:Len_p, (abs(Pp(:,jj)) ./ max(abs(Pp(:,jj)))),'o-b')
hold on 
plot(1:Len_p, (Rp(:,jj) ./ max(Rp(:,jj))),'.-r')
plot(1:Len_p, (Ep(:,jj) ./ max(Ep(:,jj))),'.-g')
plot(1:Len_p, abs(XCR_sb(:,jj)) ./ max(abs(XCR_sb(:,jj))),'.-k')

legend('Pp', 'Rp', 'Ep')
hold off
