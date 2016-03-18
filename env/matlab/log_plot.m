% Plots logged data from a control sequence
% Data has to be on the form of 'comma seperated values'
% and have column headers for naming
clear all; close all; clc

d = importdata('minicom.cap');
Fs = 100;
t = linspace(0,length(d.data)/Fs,length(d.data));

figure
hold on
scrz = get(0,'ScreenSize');
FIGURE_X = 0.75*scrz(3); FIGURE_Y = scrz(4)/scrz(3)*FIGURE_X;
set(gcf,'Position',...
    [scrz(3)/2-FIGURE_X/2 scrz(4)/2-FIGURE_Y/2 FIGURE_X FIGURE_Y]);
clear FIGURE_X FIGURE_Y scrz

lgnd_cell = {};
if any(strcmp(d.colheaders,'output'))
    p = plot(t,d.data(:,strcmp(d.colheaders,'output')),'-g');
    lgnd_cell{end+1} = 'output';
end
if any(strcmp(d.colheaders,'th1'))
    p = plot(t,d.data(:,strcmp(d.colheaders,'th1')),'-r');
    lgnd_cell{end+1} = 'th1';
end
if any(strcmp(d.colheaders,'th2'))
    plot(t,d.data(:,strcmp(d.colheaders,'th2')),'-m');
    lgnd_cell{end+1} = 'th2';
end
if any(strcmp(d.colheaders,'meas'))
    plot(t,d.data(:,strcmp(d.colheaders,'meas')),'-c');
    lgnd_cell{end+1} = 'meas';
end
if any(strcmp(d.colheaders,'rate'))
    plot(t,d.data(:,strcmp(d.colheaders,'rate')),'-b');
    lgnd_cell{end+1} = 'rate';
end
legend(lgnd_cell);
%plot(t,d.data(:,3),'-g',t,d.data(:,1),'-b',t,d.data(:,2),'-r');
xlabel('t'); ylabel('ang');

%legend('u','th1','th2');
axis([0,t(end),-50,50]);