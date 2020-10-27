

clear

% create some grey colours for plotting
grey=[0.69 0.69 0.69];
darkgrey=grey/2;

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

% basic beta - transmission rate
beta_base=0.003;

% one set of parameters, do a bunch of simulations
runstuff.maxDays=50; % number of days simulated
runstuff.seed=3443; % this seed gave a large outbreak that illustrated a lot of different points


params.hour_per_day=6; % how many contact hours per day
params.class_size=25; % class size
params.frac_asymp=0.4; % what fraction of those who are infected are asymptomatic
params.num_real_groups=5; % how many contact groups
params.TuesdayFriday=0; % set to 1 for class only meet Tuesdays and Fridays

params.mu_pip=2; % average length of presymptomatic infectious period
params.beta_aerosol_factor=.25; % how much less infectious outside groups
params.asymp_ratio=0.8; % how much less infectious are asymptotic people
  


   params.beta_index_factor=3; % how many more times infectious is index case 
   params.beta_base=beta_base*2; % especially bad environment for transmission so double.

    
for prot=1:4 % which protocol to use. See paper for explanations

      switch prot
        case 1 % baseline
            protoc.days_delay=100; protoc.num_control_groups=1;
            protoc.tests_to_shutdown_group=1000; protoc.tests_to_shutdown_class=1000; 
        case 2 % contact model
            protoc.days_delay=2; protoc.num_control_groups=5;
            protoc.tests_to_shutdown_class=1000; protoc.tests_to_shutdown_group=1;
        case 3 % two groups is an outbreak
            protoc.days_delay=2; protoc.num_control_groups=5;
            protoc.tests_to_shutdown_class=2; protoc.tests_to_shutdown_group=1;
        case 4 % whole class
            protoc.days_delay=2; protoc.num_control_groups=1;
            protoc.tests_to_shutdown_class=1; protoc.tests_to_shutdown_group=1;
      end

     
     jj=1; 
      
        params.is_asymp=jj; % is the index case aysmptomatic

rng(runstuff.seed)       % reset seed for simulation so we can see how protocols work
[stats,graphdata,plotdata]=class_sim_groups(runstuff,params,protoc); % do actual simulations

figure(prot) % separate figure for each protocol
clf

% plot outbreak. you get the order of the students from baseline case (so that later infected student appear higher) 
%, then freeze that order for the other plots
if prot==1
  order=plot_outbreak_new(params,protoc,plotdata,graphdata,[]); % plot the outbreak and determine the order
else 
  plot_outbreak_new(params,protoc,plotdata,graphdata,order); % plot the outbreak using order from baseline
end
  


end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Fix up labels and axes for each plot
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

figure(1)
%xlabel('days')

set(gca,'ycolor',[0 0 0])
set(gca,'ytick',[])
set(gca,'box','off')
set(gca,'xtick',[])
set(gca,'xcolor',[1 1 1])
set(gca,'ycolor',[1 1 1])

ylabel('Baseline','color','k')
 
figure(2)
set(gca,'box','off')
set(gca,'xtick',[])
set(gca,'ycolor',[1 1 1])
set(gca,'xcolor',[1 1 1])
ylabel('Contact','color','k')
xlabel('days') 

figure(3)
set(gca,'box','off')
set(gca,'xtick',[])
set(gca,'ycolor',[1 1 1])
set(gca,'xcolor',[1 1 1])
ylabel('Contact','color','k')
ylabel('Two groups is an outbreak','color','k')
 
figure(4)

xlabel('days')
set(gca,'ycolor',[1 1 1])
%set(gca,'xtick',[])
set(gca,'box','off')
set(gca,'xcolor',[0 0 0])
ylabel('Whole class','color','k')
 

% create the legend
figure(5)
clf

subplot(1,4,1)

rectangle('Position',[0 0 3 1 ],'faceColor',map(2,:))
text(0.1,-0.2,'Exposed')
rectangle('Position',[0 -2 3 1 ],'faceColor',map(3,:))
text(0.1,-2.2,'Presymptomatic')
rectangle('Position',[0 -4 3 1 ],'faceColor',map(4,:))
text(0.1,-4.2,'Symptomatic')
rectangle('Position',[0 -6 3 1 ],'faceColor',map(5,:))
text(0.1,-6.2,'Recovered')

hold on

x=linspace(0.2,0.2,10); y=linspace(-7,-9,10);
plot(x,y,'.','color',darkgrey);
text(0.5,-8,'Group Intervention')

line([0.2 0.2],[-10 -12],'linewidth',3,'color',grey,'linestyle','-')
text(0.5,-11,'Class Intervention')


axis([0 4 -3 1])
axis equal
axis off
box off

% print legend to file
print('outbreak_legend','-depsc')

% print plots to file
for prot=1:4
figure(prot)
filename=strcat('outbreak_protocol',num2str(prot));
print(filename,'-depsc')
end
