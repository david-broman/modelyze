% Animates the sliding mass and it's pendulum found in the moz file
% slidingmass.moz
clear all; close all; clc

d = execute_modelyze('../../demo','inverted_pendulum.moz');
if isempty(d); return; end % ERROR


L = 3.0;
Lp = 0.25;

xind = 0;
thind = 0;
refthind = 0;
Find = 0;
rest_ind = zeros(size(d.colheaders));
for i = 1:length(d.colheaders); 
    if strcmp(d.colheaders{i},'x'); xind = i;
    elseif strcmp(d.colheaders{i},'th'); thind = i;
    elseif strcmp(d.colheaders{i},'refTh'); refthind = i;
    elseif strcmp(d.colheaders{i},'F'); Find = i;
    elseif ~strcmp(d.colheaders{i},'time'); rest_ind(i) = 1; end;
end;

% Make main fig
fig = figure(1);
scrz = get(0,'ScreenSize');
FIGURE_X = 0.75*min(scrz(3),scrz(4)); FIGURE_Y = FIGURE_X;
set(fig,'Position',...
    [scrz(3)/2-FIGURE_X/2 scrz(4)/2-FIGURE_Y/2 FIGURE_X FIGURE_Y]);
clear FIGURE_X FIGURE_Y scrz
axis equal
axis manual
axis([min(d.data(:,xind))-L max(d.data(:,xind))+L -1.2*L 1.2*L]);
hold on

% Plot handles
h = axis; plot([h(1) h(2)],[0 0],'--r');
pref = [];
if refthind > 0
    pref = plot([0 0],[0 -L],'--k');
    set(pref,'LineWidth',1);
end
p(1) = plot([0 0],[0 -L],'-b');
p(2) = plot([-Lp Lp],[0 0],'-b');
set(p(1),'LineWidth',2);
set(p(2),'LineWidth',4);


% Animate!
while ishandle(fig)
    pause(0.1);
    timescaling = 1.0;
    tic;
    plot_time = 0;
    while plot_time < d.data(end,1)
        i = find(timescaling*toc < d.data(:,1),1);
        th = d.data(i,thind); x = d.data(i,xind);
        link = [L*sin(th), -L*cos(th)]; %[x_end, y_end]
        
        if ~ishandle(fig)
            break;
        end

        set(p(1),'XData',[x link(1)+x],'YData',[0 link(2)]);
        set(p(2),'XData',[x-Lp x+Lp]);
        if ~isempty(pref)
            thref = d.data(i,refthind);
            ref = [L*sin(thref), -L*cos(thref)]; %[x_end, y_end]
            set(pref,'XData',[x ref(1)+x],'YData',[0 ref(2)]);
        end
        
        tit = sprintf('t = %0.2f',d.data(i,1));
        if thind
            tit = strcat(tit,sprintf(', th = %0.2f',d.data(i,thind)));
        end
        if refthind
            tit = strcat(tit,sprintf(', th_ref = %0.2f',d.data(i,refthind)));
        end
        if Find
            tit = strcat(tit,sprintf(', F = %0.2fN',d.data(i,Find)));
        end
        title(tit);
        
        drawnow;
        pause(0.01);
        plot_time = timescaling*toc;
    end
end



