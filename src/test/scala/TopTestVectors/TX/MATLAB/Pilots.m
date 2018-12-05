function [Pil] = Pilots(NDS)
Pil = zeros(8,NDS);
init = [0 0 1 1 1 1 1 1 1 1 1];

for ii = 1:1:NDS,
    Wk =  init(11);
    Pil(1,ii)=1-2*(1-Wk);
    Pil(2,ii)=1-2*(1-Wk);
    Pil(6,ii)=1-2*(1-Wk);
    Pil(8,ii)=1-2*(1-Wk);
    
    Pil(3,ii)=1-2*Wk;
    Pil(4,ii)=1-2*Wk;
    Pil(5,ii)=1-2*Wk;
    Pil(7,ii)=1-2*Wk;
    init = [xor(init(9),Wk) init(1:10)];
end

Pil = Pil ./ sqrt(2);