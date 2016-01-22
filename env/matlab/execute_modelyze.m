function d  = execute_modelyze( filepath,mozscript )
% Execute the modelyze script mozscript at the given filepath
% Returns struct of result or empty if it failed.

storefile = '_moz_result.txt'; % Storing result
d = [];

% Optimization: If storefile is newer than mozscript ignore running the
% mozscript and just load the data from storefile:
dr = dir(filepath);
storeInd = 0; scriptInd = 0;
for i = 1:length(dr)
    if strcmp(dr(i).name,storefile)
        storeInd = i;
    elseif strcmp(dr(i).name,mozscript)
        scriptInd = i;
    end
end
runscript = 1;  % Set to zero if you just want to read the stor file
if scriptInd == 0
    error('Couldn''t find the script file %s',mozscript);
elseif storeInd == 0
    runscript = 1;
elseif dr(scriptInd).datenum > dr(storeInd).datenum
    runscript = 1;
end

% Run modelyze script
if runscript
    tic;
    [s,r] = ...
        system(['cd "',filepath,'" && ./moz ',mozscript,' > ',storefile]);
    if s ~= 0
        disp(['ERROR: ',r]);
        return;
    end
    if ~isempty(r)
        disp(r);
    end
    fprintf('Modelyze computation time: %0.2f sec\n',toc);
end

d = importdata([filepath,'/',storefile]);


end

