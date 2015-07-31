% Animates a robotic arm in real time simulated by modelyze
clear all; close all; clc

filepath = '../../demo';
filename = 'modular-pendulums.moz';

useSaved = 1;   % Use a saved simulation instead of simulating
if useSaved
    load('QuintuplePendulum.mat');
else
    % Perform simulation
    d = ExecuteModelyze(filepath,filename);
    if isempty(d); return; end % ERROR
    % Arm lengths: TODO: make acquiring these automatic
    L(1) = 0.5; L(2) = 0.5;
end

N = size(d.data,2) - 1; % How many entries in addition to time
if N == 0; error('MODELYZE DATA CONTAINS ONLY TIME'); end;

% In order for this program to recognize the angles they have to be named
% th_1, th_2 etc. TODO: make more of these for forces, torques, positions
% etc.
iAngles = [];
for i = 2:(N+1)
    if regexpi(d.textdata{i},'th_\d') == 1
        n = strsplit(d.textdata{i},'_');
        iAngles(str2double(n{end})) = i;
    end
end
iAngles(iAngles == 0) = []; % Removes zeros

% TEMPORARY!
if length(iAngles) > length(L)
    disp('Not enough lengths defined, assuming everything is 0.5');
    L = 0.5*ones(size(iAngles));
end

if length(L) < length(iAngles)
    error('There is not enough arm lengths defined');
end
if isempty(iAngles)
    s = ''; for i = 2:(N+1); s = strcat(s,[' ',d.textdata{i},',']); end;
    error('No plottable data retrieved from modelyze (Got:%s)',s(1:end-1));
end
    

% Sets up main figure
fig = figure(1);
scrz = get(0,'ScreenSize');
FIGURE_X = 0.75*min(scrz(3),scrz(4)); FIGURE_Y = FIGURE_X;
set(fig,'Position',...
    [scrz(3)/2-FIGURE_X/2 scrz(4)/2-FIGURE_Y/2 FIGURE_X FIGURE_Y]);
clear FIGURE_X FIGURE_Y scrz

axis equal
axis manual
axis([-1.2*sum(L(1:length(iAngles))) 1.2*sum(L(1:length(iAngles)))...
    -1.2*sum(L(1:length(iAngles))) 1.2*sum(L(1:length(iAngles)))]);
hold on

% Sets up plot handles
p = [];
cm = hsv(length(iAngles));
for i = 1:length(iAngles)
    p(i) = plot(NaN,NaN);
    set(p(i),'Color',cm(i,:),'LineWidth',4,'LineStyle','-');
end

% Animates it
pause(0.1);
timescaling = 1;
tic;
while timescaling*toc < d.data(end,1) && ishandle(fig)
    i = find(timescaling*toc < d.data(:,1),1);
    
    % Plots the links!
    xl = 0; yl = 0;
    for il = 1:length(p)
        xn = xl + L(il)*cos(d.data(i,iAngles(il)));
        yn = yl + L(il)*sin(d.data(i,iAngles(il)));
        set(p(il),'XData',[xl xn],'YData',[yl yn]);
        xl = xn; yl = yn;        
    end
    
    title(sprintf('%0.2f',d.data(i,1)));
    
    drawnow;
    pause(0.01);
end







