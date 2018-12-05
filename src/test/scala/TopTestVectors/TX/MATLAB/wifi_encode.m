function cbits = wifi_encode(bits)
g1 = [1,0,1,1,0,1,1]; % 133
g2 = [1,1,1,1,0,0,1]; % 171

cbits = zeros(length(bits)*2, 1, 'int8');

for i = 1:length(bits)-6
    state = bits(i:i+6);
    a = g_xor(g1, state);
    b = g_xor(g2, state);
    cbits((i*2)-1:i*2) = [a;b];
end

    
function out = g_xor(g, x)
tmp = reshape(g, [], 1) .* reshape(x, [], 1);
out = mod(sum(tmp), 2);