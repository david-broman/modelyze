% Simualtes an furuta pendulum
clear all; close all; clc

filepath = '../../proj/modular-arm';
filename = 'furuta_pendulum_test.moz';
% Perform simulation
d = execute_modelyze(filepath,filename);
if isempty(d); return; end % ERROR
% Arm lengths: TODO: make acquiring these automatic
L1 = 0.2735; % Length of main arm
L2 = 0.533; % Length of pendulum

N = size(d.data,2) - 1; % How many entries in addition to time
if N == 0; error('MODELYZE DATA CONTAINS ONLY TIME'); end;

ind = struct('th1',0,'th2',0,'refth',0,'u',0,'i',0,'tau',0);
for i = 1:length(d.colheaders); 
    if strcmp(d.colheaders{i},'th_1'); ind.th1 = i;  end;
    if strcmp(d.colheaders{i},'th_2'); ind.th2 = i;  end;
    if strcmp(d.colheaders{i},'ref_th'); ind.refth = i;  end;
    if strcmp(d.colheaders{i},'u'); ind.u = i;  end;
    if strcmp(d.colheaders{i},'i'); ind.i = i;  end;
    if strcmp(d.colheaders{i},'T'); ind.tau = i;  end;
end
if ind.th1 == 0 || ind.th2 == 0
    error('th_1 and/or th_2 not found');
end

% Make main fig
fig = figure(1);
scrz = get(0,'ScreenSize');
FIGURE_X = 0.75*min(scrz(3),scrz(4)); FIGURE_Y = FIGURE_X;
set(fig,'Position',...
    [scrz(3)/2-FIGURE_X/2 scrz(4)/2-FIGURE_Y/2 FIGURE_X FIGURE_Y]);
axis equal
axis manual
grid on
scaling = 1.25;
axis([-scaling*L1 scaling*L1 -scaling*L1 scaling*L1 -scaling*L2 scaling*L2]);
hold on
clear FIGURE_X FIGURE_Y scrz scaling
xlabel('x'); ylabel('y'); zlabel('z');
%set(gca,'fontsize',24);

%%
% plot static base
a = axis;
p_base = plot3([0,0],[0,0],[a(5),0],'-k');
set(p_base,'LineWidth',6);
[x_sphere,y_sphere,z_sphere] = sphere;
surf(0.02*x_sphere,0.02*y_sphere,0.02*z_sphere,zeros(21,21,3));

% plot handles
c1_base = [L1,0,0]';
c2_base = [0,0,-L2]';

p(1) = plot3(NaN,NaN,NaN,'-r');
p(2) = plot3(NaN,NaN,NaN,'-b');
p(3) = plot3(NaN,NaN,NaN,'--k');
set(p(1),'LineWidth',4);
set(p(2),'LineWidth',4);
set(p(3),'LineWidth',1);


% Animate!
pause(0.1);
timescaling = 1;
while ishandle(fig)
    tic;
    plot_time = 0;
    while plot_time < d.data(end,1)
        i = find(plot_time < d.data(:,1),1);
        th1 = d.data(i,ind.th1)*180/pi; 
        th2 = d.data(i,ind.th2)*180/pi;
        c1 = rotz(th1)*c1_base;
        c2 = rotz(th1)*rotx(th2)*c2_base;

        if ~ishandle(fig)
            break;
        end

        set(p(1),'XData',[0 c1(1)],'YData',[0 c1(2)],'ZData',[0 c1(3)]);
        set(p(2),'XData',[c1(1) c1(1)+c2(1)],'YData',[c1(2) c1(2)+c2(2)],...
           'ZData',[c1(3) c1(3)+c2(3)]);
        if ind.refth
            c3 = rotz(th1)*rotx(d.data(i,ind.refth)*180/pi)*c2_base;
            set(p(3),'XData',[c1(1) c1(1)+c3(1)],'YData',[c1(2) c1(2)+c3(2)],...
           'ZData',[c1(3) c1(3)+c3(3)]);
        end

        tit = sprintf('t = %0.2f',d.data(i,1));
        if ind.u
            tit = strcat(tit,sprintf(', u = %0.2fV',d.data(i,ind.u)));
        end
        if ind.i
            if d.data(i,ind.i) > 0.1
                tit = strcat(tit,sprintf(', i = %0.2fA',d.data(i,ind.i)));
            else
                tit = strcat(tit,sprintf(', i = %0.2fmA',d.data(i,ind.i)/1000));
            end
        end
        if ind.tau
            tit = strcat(tit,sprintf(', tau = %0.2fNm',d.data(i,ind.tau)));
        end

        title(tit);
        drawnow;
        pause(0.01);
        plot_time = timescaling*toc;
    end
    pause(0.5);
end
clear c1 c2 i th alfa tit *_sphere

