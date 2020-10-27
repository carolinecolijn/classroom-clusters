function order_output=plot_outbreak(params,protoc,plotdata,graphdata,order)
% if order is [], then determine order from this run, then output it for later use 
% if order is an actual order, then use it and output it unchanged.

% some colours for later
grey=[0.69 0.69 0.69];
darkgrey=grey/2;

% sort data if needed
if isempty(order)
  % creat new stat that is minimal infection time in each group
  for k=1:params.class_size
    min_group_infection(k)=min(graphdata.time_infected(find(graphdata.real_group==graphdata.real_group(k))));
  end

  % order so that within each group, later infections are plotted higher
  tempTable=struct2table(graphdata);
  tempTable.grouptime=min_group_infection';
  % within each group sort by the time infected
  [~,order]=sortrows(tempTable,{'grouptime','time_infected'});
%    [~,order]=sortrows(tempTable,{'real_group','time_infected'});
end
order_output=order;


% reorder everything according to this order for plotting.
% we are actually renumbering the students
plotdata.S=plotdata.S(order,:);
plotdata.E=plotdata.E(order,:);
plotdata.I=plotdata.I(order,:);
plotdata.symp=plotdata.symp(order,:);
plotdata.R=plotdata.R(order,:);
graphdata.was_infected=graphdata.was_infected(order);
graphdata.who_infected=graphdata.who_infected(order);
graphdata.was_symptomatic=graphdata.was_symptomatic(order);
graphdata.child_generation=graphdata.child_generation(order);
graphdata.time_infected=graphdata.time_infected(order);
graphdata.time_symptomatic=graphdata.time_symptomatic(order);
graphdata.control_group=graphdata.control_group(order);

% fix up the who_infected variable since we've renumbered the students
for k=1:params.class_size
    temp=find(order==graphdata.who_infected(k));
    if ~isempty(temp)
      graphdata.who_infected(k)=min(temp);
    else
      graphdata.who_infected(k)=0;
    end
end

% every student has a location in the plot
% if the location is zero they are not plotted. some locations are not used
% in order to make spaces between rows.
class_size=params.class_size;
student_numbers=(1:class_size)';

% we plot students in order, with spaces between groups
plot_loc=student_numbers+graphdata.real_group-1;


subplot(2,1,1)
    


% colormap from ColorBrewer by Stephen Cobeldick
%color specifications and designs developed by Cynthia Brewer (http://colorbrewer.org/).
brew =[
    0.4039         0    0.1216
    0.6980    0.0941    0.1686
    0.8392    0.3765    0.3020
    0.9569    0.6471    0.5098
    0.9922    0.8588    0.7804
    0.8196    0.8980    0.9412
    0.5725    0.7725    0.8706
    0.2627    0.5765    0.7647
    0.1294    0.4000    0.6745
    0.0196    0.1882    0.3804];
map(1,1:3)=[1 1 1 ];
map(2:5,:)=brew([4 3 2  7],:);
timevect=plotdata.timevect;
timevect=[timevect timevect(end)];

axis([0 50 0.5 max(plot_loc)+.5])

% small number for fiddling with spacing
smidge=0.5;

% plot the coloured bars for each student
hold on
 for k=1:length(student_numbers) % exposed
      if ~isempty(find(plotdata.E(k,:),1))
     x1=timevect(find(plotdata.E(k,:),1)); x2=timevect(find(plotdata.E(k,:),1,'last')+1);
     y1=plot_loc(k)-smidge; y2=plot_loc(k)+smidge;
      rectangle('Position',[x1 y1 x2-x1 y2-y1],'FaceColor',map(2,:));
      end
     if ~isempty(find(plotdata.I(k,:),1))
      x1=timevect(find(plotdata.I(k,:),1)); x2=timevect(find(plotdata.I(k,:),1,'last')+1);
     y1=plot_loc(k)-smidge; y2=plot_loc(k)+smidge;
      rectangle('Position',[x1 y1 x2-x1 y2-y1],'FaceColor',map(3,:));
     end
     if ~isempty(find(plotdata.symp(k,:),1))
      x1=timevect(find(plotdata.symp(k,:),1)); x2=timevect(find(plotdata.symp(k,:),1,'last')+1);
     y1=plot_loc(k)-smidge; y2=plot_loc(k)+smidge;
      rectangle('Position',[x1 y1 x2-x1 y2-y1],'FaceColor',map(4,:));
     end
     if ~isempty(find(plotdata.R(k,:),1))
     x1=timevect(find(plotdata.R(k,:),1)); x2=timevect(find(plotdata.R(k,:),1,'last')+1);
     y1=plot_loc(k)-smidge; y2=plot_loc(k)+smidge;
      rectangle('Position',[x1 y1 x2-x1 y2-y1],'FaceColor',map(5,:));
     end
 end
 

set(gca,'ytick',[]);
set(gca,'ycolor',[1 1 1])
set(gca, 'YDir','normal')

 
 % plot a line for when the class is shut down
 ybar=[-10000 100000]';
xbar=plotdata.class_time/params.hour_per_day*ones(size(ybar));
 line(xbar,ybar,'linewidth',3,'color',grey,'linestyle','-')


% plot shutdown times in groups
for g=1:protoc.num_control_groups
    indsbar=find(graphdata.control_group==g);
    ybar=[ min(plot_loc(indsbar))-1/2 max(plot_loc(indsbar))+1/2]';
    gt=plotdata.group_time(g);
    if (gt>plotdata.class_time || protoc.num_control_groups==1)
        gt=1e8;
    end
    xbar=gt/params.hour_per_day*ones(size(ybar));
    x=linspace(xbar(1),xbar(2),10); y=linspace(ybar(1),ybar(2),10);
    plot(x,y,'.','color',darkgrey)
end

% plot infection arrows
for k=2:length(student_numbers)
    if graphdata.was_infected(k)
      x1=(graphdata.time_infected(k)-.75)/params.hour_per_day;
      x2=x1;
      y1=plot_loc(graphdata.who_infected(k));
      y2=plot_loc(k);
       quiver(x1,y1,x2-x1,y2-y1,0,'color','k','autoscale','off','linewidth',2,'maxheadsize',2/abs(y2-y1))
    end
    
end


end