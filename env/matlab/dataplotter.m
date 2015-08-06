% Plots generic output data from modelyze
clear all; close all; clc;
%%
filepath = '../../proj/modular-arm';
filename = 'frictionverification.moz';

useSaved = 0;   % Use a saved simulation instead of simulating
if useSaved
    load('QuintuplePendulum.mat'); clear L;
else
    d = ExecuteModelyze(filepath,filename);
    if isempty(d); return; end % ERROR
end
%%
scrz = get(0,'ScreenSize');
FIGURE_X = 600; FIGURE_Y = 300;

fig = figure;
set(fig,'Position',[scrz(3)*1/2-FIGURE_X/2, scrz(4)*1/2-FIGURE_Y/2, FIGURE_X, FIGURE_Y])
hold on;
xlabel('Time [sec]')

% Assumes time is in first column
t = d.data(:,1);

ca = {};
cm = hsv(size(d.data,2));
for i = 2:size(d.data,2)
    ca{end+1} = strrep(d.textdata{i},'_',' ');
    p = plot(t,d.data(:,i),'-');
    set(p,'Color',cm(i-1,:));
end
legend(ca);