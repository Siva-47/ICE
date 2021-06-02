function west = stochastic_block(G, h)
n = size(G, 1);

% h = round(log(n));

% Empirical Degree Sorting
d = mean(G);
[~, pos] = sort(d, 'descend');
A = G(pos, pos);

% Histogram
west = imfilter(A, ones(h)/h^2, 'symmetric');
west = west(1:h:end, 1:h:end);
west = imresize(west, [n, n], 'nearest');
