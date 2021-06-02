function h_opt = oracle_h(G,wtrue)
%%%%%%%%%%%%%%%%%%%%
% oracle_h.m returns the oracle binwidth for SBA algorithm
%
% This routine requires the ground truth graphon wtrue, and 
% so the estimated h_opt is oracle.
% 
% Idea: Sweep through a range of h and pick the one that minimizes 
%       the mean squared error (wrt ground truth)
%
% Remark: Oracle h is only useful for SBA.
%         Oracle h is NOT used for SAS.
%
% Input: G - grpah (size nxn)
%        wtrue - true graphon (size nxn)
% Output: h_opt - oracle binwidth
%
% Stanley Chan
% Harvard University
% Jan 10, 2014
%%%%%%%%%%%%%%%%%%%

n = size(G,1);
% Empirical Degree Sorting
d = mean(G);
[~, pos] = sort(d,'descend');
A = G(pos,pos);

num_h = 100;
h_set = linspace(1, n/4, num_h); %logspace(log10(1), log10(n), num_h);
Cost  = zeros(num_h,1);
for hidx = 1:num_h
    h = round(h_set(hidx));
    west = imfilter(A, ones(h)/h^2, 'symmetric');
    Cost(hidx)  = mean((west(:)-wtrue(:)).^2);
end
[~, pos] = min(Cost);
h_opt = max(1,round(h_set(pos)));
