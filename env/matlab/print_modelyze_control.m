function print_modelyze_control( Gfb,Gff )
% print control parameters for insertion into a modelyze
% controller

[NUMfb,DENfb] = tfdata(Gfb); NUMfb = NUMfb{1}; DENfb = DENfb{1};
[NUMff,DENff] = tfdata(Gff); NUMff = NUMff{1}; DENff = DENff{1};

if sum(abs(DENfb-DENff)) > 1e-3
    error('Gfb and Gff denominators not the same');
end
DEN = DENfb;

% Scale them
while abs(DEN(end)) < 1e-6
    DEN = DEN(1:end-1);
end
while abs(NUMfb(1)) < 1e-6
    NUMfb = NUMfb(2:end);
end
while abs(NUMff(1)) < 1e-6
    NUMff = NUMff(2:end);
end

% Print stuff
so = '';
maxlength = 50;
for i = 1:length(NUMfb)
    if isempty(so)
        so = strcat(so,sprintf('def s%d = %0.3f; ',length(NUMfb)-i,NUMfb(i)));
    else
        so = strcat(so,sprintf(' def s%d = %0.3f; ',length(NUMfb)-i,NUMfb(i)));
    end
    if length(so) > maxlength
        fprintf('%s\n',so); so = '';
    end
end
for i = 1:length(NUMff)
    if isempty(so)
        so = strcat(so,sprintf('def t%d = %0.3f; ',length(NUMff)-i,NUMff(i)));
    else
        so = strcat(so,sprintf(' def t%d = %0.3f; ',length(NUMff)-i,NUMff(i)));
    end
    if length(so) > maxlength
        fprintf('%s\n',so); so = '';
    end
end
for i = 2:length(DEN)
    if isempty(so)
        so = strcat(so,sprintf('def r%d = %0.3f; ',length(DEN)-i,DEN(i)));
    else
        so = strcat(so,sprintf(' def r%d = %0.3f; ',length(DEN)-i,DEN(i)));
    end
    if length(so) > maxlength
        fprintf('%s\n',so); so = '';
    end
end


if ~isempty(so)
    fprintf('%s\n',so);
end

end

