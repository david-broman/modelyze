%% import data
data = importdata('lrd2.dat');
t = data.data(:,1);
d = data.data(:,2:end);

%% plot through and across variables
myplot(t,d,data.colheaders(2:end))

%% plot power
P = d(:,2:2:end).*d(:,1:2:end);
Plabels = [{'P_D'}, {'P_L'}, {'P_R'}, {'P_V'}, {'P_{sum}'}];
myplot(t,[P,sum(P,2)],Plabels)

%% plot energy
W = cumtrapz(t,P);
Wlabels = [{'W_D'}, {'W_L'}, {'W_R'}, {'W_V'}, {'W_{sum}'}];
myplot(t,[W,sum(W,2)],Wlabels)

%% define functions
function myplot(t, d, l)
    figure
    hold on
    plot(t,d)
    legend(l)
    xlabel('t')
    hold off
end