path = 'C:\Users\yulin\Desktop\ICE-master\simulation\'; % your path

repeats = 3; % 3 repetitions for quick display 

for s=1:8

    sas_rmse  = zeros(1, repeats);
    sba_rmse  = zeros(1, repeats);

    for r=1:repeats
        A = csvread(sprintf('%s%s%d%s%s%d%s', path, 'data\gp', s, '_A', '_', r, '.csv'), 1, 0);
        P = csvread(sprintf('%s%s%d%s%s%d%s', path, 'data\gp', s, '_P', '_', r, '.csv'), 1, 0);

        % Empirical Degree Sorting
        d = mean(A);
        [~, pos] = sort(d, 'descend');
        P = P(pos, pos);
        A = A(pos, pos);

        [~, rec] = sort(pos, 'ascend'); 

        % SAS
        sas_res = sort_and_smooth(A);
        rmse_sas(1, r)  = sqrt(nanmean((sas_res(:)-P(:)).^2)); 

        % SBA
        if r==1
            h = oracle_h(A, P);
        end
        sba_res = stochastic_block(A, h);
        rmse_sba(1, r)  = sqrt(nanmean((sba_res(:)-P(:)).^2));

        sas_res = sas_res(rec, rec);
        sba_res = sba_res(rec, rec);

    end

    res = [rmse_sas;rmse_sba];

    writematrix(res, sprintf('%s%s%d%s', path, 'result/gp', s, '_SAS_SBA.csv'))
end