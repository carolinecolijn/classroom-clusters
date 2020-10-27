% this is almost the same file as the code in basic simulations
% see there for more detailed comments
function [stats,graphdata,plotdata]=class_sim_groups(runstuff,params,protoc)

num_days=runstuff.maxDays;

hours_per_day=params.hour_per_day;
is_asymp=params.is_asymp;
cl_size=params.class_size;
frac_asymp=params.frac_asymp;

beta_in=params.beta_base;
beta_out=beta_in*params.beta_aerosol_factor;

num_real_groups=params.num_real_groups;

beta_index_factor=params.beta_index_factor;
asymp_ratio=params.asymp_ratio;


daysDelay=protoc.days_delay;
tests_to_shutdown_group=protoc.tests_to_shutdown_group;
tests_to_shutdown_class=protoc.tests_to_shutdown_class;
num_control_groups=protoc.num_control_groups;

was_infected=zeros(cl_size,1);
who_infected=zeros(cl_size,1);

% is detected
was_symptomatic=zeros(cl_size,1);

% was disrupted
was_disrupted=zeros(cl_size,1);

diagnosis_delay=hours_per_day*daysDelay; % number of hours delay in testing


% pooled testing stuff
pooled_test_interval=params.testingDays*params.hour_per_day; % how frequently you test
time_to_pooled_shutdown=rand*pooled_test_interval+diagnosis_delay;  % randomize where in schedule infected person arrives

% build real groups
real_group=ceil(num_real_groups*(1:cl_size)/cl_size)';

% build control groups
control_group=ceil(num_control_groups*(1:cl_size)/cl_size)';
group_symp_count=zeros(num_control_groups,1);

huge=1e8; % big number to be infinity

% GROUP CONTROLS
% group_flag 1 if group is running, 0 if group is shut
%school_flag=1; % school is open
group_flag=ones(num_control_groups,1);
% flag_set is zero until first symptoms appear
group_flag_set=zeros(num_control_groups,1); % enoug symptomatic case?
group_flag_time=huge*hours_per_day*ones(num_control_groups,1); % time to shut down class is not set yet

% CLASS CONTROLS
class_flag=1;  % class running?
class_flag_set=0; % sufficient groups shut down
class_flag_time=huge*hours_per_day;


% children matrix with infected or not S, E, I, symp, R
child_S=ones(cl_size,1);        % susceptible
child_E=zeros(cl_size,1); time_E=huge*hours_per_day*ones(cl_size,1);   % exposed
child_I=zeros(cl_size,1); time_I=huge*hours_per_day*ones(cl_size,1);   % infectious
child_symp=zeros(cl_size,1); time_symp=huge*hours_per_day*ones(cl_size,1); % symptomatic       
child_R=zeros(cl_size,1); time_R=huge*hours_per_day*ones(cl_size,1);

% this keeps track of what generation of infection child is
% -1 is not infected yet
child_generation=-1*ones(cl_size,1);

% set durations of time between getting it and being able to infect others
% mean is 3.3 days, std is 1 day, 6 hours per day
latentperiod=hours_per_day*mygamma(3,1,cl_size);
dangerzone=hours_per_day*mygamma(params.mu_pip,0.5,cl_size);
% which students are asymptomatic
asymps=(rand(cl_size,1)<frac_asymp);
asymps(1)=is_asymp; % index case is determined

% modify so assymptotic fraction has dangerzone huge
dangerzone(asymps)=huge*hours_per_day;
infectiousperiod=hours_per_day*mygamma(10,5,cl_size);


% intial infection
child_S(1,1)=0; child_I(1,1)=1; 
child_generation(1,1)=0;
time_E(1,1)=0; % skip exposure
time_I(1,1)=0;
was_infected(1,1)=1;
who_infected(1,1)=-1;

% time interval
delta_t=1; % units is hours
tottime=num_days*hours_per_day;
numsteps=round(tottime/delta_t);

% matrices where we store everything
S_mat=zeros(cl_size,numsteps+1);
E_mat=zeros(cl_size,numsteps+1);
I_mat=zeros(cl_size,numsteps+1);
symp_mat=zeros(cl_size,numsteps+1);
R_mat=zeros(cl_size,numsteps+1);

S_mat(:,1)=child_S;
E_mat(:,1)=child_E;
I_mat(:,1)=child_I;
symp_mat(:,1)=child_symp;
R_mat(:,1)=child_R;


for kk=1:numsteps
  
   currentTime=kk*delta_t;
         
   if ~params.TuesdayFriday
     class_running=~isweekend(currentTime,hours_per_day);
   else
     class_running=isTuesdayFriday(currentTime,hours_per_day);
   end
   
   % loop over all children to be infected
   for k=1:cl_size
       
    
      % loop over all children who might infect
     for jj=1:cl_size
        
         
       % generate random number
       rand_num=rand;
             
       
       % is k susceptible
       if (child_S(k,1)==1 && group_flag(control_group(k)) && class_flag && class_running) % if susceptible, maybe infect, if school is running      
            
                % is jj infectious
                if (child_I(jj,1)==1 && group_flag(control_group(jj)))
                    if real_group(k)==real_group(jj)
                        beta=beta_in;
                    else
                        beta=beta_out;
                    end
                    if jj==1  % infectious child is index case
                        beta=beta*beta_index_factor;
                    end
                    if asymps(jj)
                        beta=beta*asymp_ratio;
                    end
                   
                    if rand_num<beta*delta_t
                     child_S(k,1)=0;
                     child_E(k,1)=1;
                     time_E(k,1)=currentTime;
                     child_generation(k,1)=child_generation(jj,1)+1;
                     was_infected(k,1)=1;
                     who_infected(k,1)=jj;
                    end                
                end
           end
       end
       
     if child_E(k,1)==1 % if exposed, maybe turn infectious
           
       if currentTime > time_E(k,1)+ latentperiod(k,1)
         child_E(k,1)=0;
         child_I(k,1)=1;
         time_I(k,1)=currentTime;
       end
     end
   
     if child_I(k,1)==1 % if infectious, maybe turn symptomatic
       if currentTime> time_I(k,1)+ dangerzone(k,1)
         child_I(k,1)=0;
         child_symp(k,1)=1;           
         time_symp(k,1)=currentTime;
         was_symptomatic(k,1)=1;
         was_disrupted(k,1)=1;
       end
     end
       
       
     if (child_I(k,1)==1 || child_symp(k,1)==1 ) % if infectious or symp might recover
       if currentTime> time_I(k,1)+ infectiousperiod(k,1)
                child_I(k,1)=0;
                child_symp(k,1)=0;
                child_R(k,1)=1;
                time_R(k,1)=currentTime;
       end
     end
       
       
       
   end
   
   % compute tot symptomatic from each group
   for g=1:num_control_groups
       group_symp_count(g)=sum(child_symp(control_group==g));
   end
   
   % compute total symptomatic in class
   %class_symp_count=sum(group_symp_count);
   
   % does a group get triggered?
   for g=1:num_control_groups
       if (group_symp_count(g)>=tests_to_shutdown_group && group_flag_set(g)==0)
           group_flag_time(g)=currentTime+diagnosis_delay;
           group_flag_set(g)=1;
           was_disrupted(control_group==g)=1;
       end
   end
   
    % do we shutdown a group?
   for g=1:num_control_groups
     if currentTime>=group_flag_time(g)
       group_flag(g)=0;
     end
   end
   
   % does class get triggered
  % if (class_symp_count>=tests_to_shutdown_class && class_flag_set==0)
  %     class_flag_time=currentTime+diagnosis_delay;
  %     class_flag_set=1;
  % end
   
   % do we shutdown class?
   if (sum(~group_flag)>=tests_to_shutdown_class && ~class_flag_set)
       class_flag_set=1;
       class_flag=0;
       class_flag_time=currentTime;
       was_disrupted(:)=1;
   end
   
   % shutdown class with pooled testing
   if currentTime>time_to_pooled_shutdown
       class_flag_set=1;
       class_flag=0;
       class_flag_time=currentTime;
       was_disrupted(:)=1;
   end
   
   S_mat(:,kk+1)=child_S;
   E_mat(:,kk+1)=child_E;
   I_mat(:,kk+1)=child_I;
   symp_mat(:,kk+1)=child_symp;
   R_mat(:,kk+1)=child_R;

   
end


%timevect=(0:numsteps)*delta_t/hours_per_day;
%was_infected=child_E+child_I+child_symp+child_R;
tot_infected=sum(was_infected);
shutdowntime=class_flag_time/hours_per_day;
%num_infectious=sum(I_mat,1).*(timevect<shutdowntime);

%total_infects=sum(E_mat+I_mat+symp_mat+R_mat,1);

%timings=time_E;
%generations=child_generation;

% for each person compute 1) total time assymptomatic 2) total time
% assymptomatic before shutdown of group or class
start_asymp=min([time_I tottime*ones(cl_size,1)],[],2);
end_asymp1=min([time_R time_symp tottime*ones(cl_size,1) ],[],2);
end_asymp2=min([time_R time_symp group_flag_time(control_group) tottime*ones(cl_size,1) class_flag_time*ones(cl_size,1)],[],2);
days_asymp1=(end_asymp1-start_asymp).*was_infected/hours_per_day;
days_asymp2=(end_asymp2-start_asymp).*was_infected/hours_per_day;
% fix to not have negative times
days_asymp1=days_asymp1.*(days_asymp1>0);
days_asymp2=days_asymp2.*(days_asymp2>0);



stats.days_asymp_lax=sum(days_asymp1);
stats.days_asymp_strict=sum(days_asymp2);
stats.shutdowntime=shutdowntime;
stats.total_infected=tot_infected;
%stats.total_infected=total_infects(end);
stats.total_not_detected=sum(was_infected & ~was_symptomatic);
stats.groups_shut_down=num_control_groups-sum(group_flag);
stats.class_shut_down=1-class_flag;
stats.students_affected=max([stats.groups_shut_down*cl_size/num_control_groups stats.class_shut_down*cl_size]);
stats.students_disrupted=sum(was_disrupted);

graphdata.was_infected=was_infected;
graphdata.who_infected=who_infected;
graphdata.was_symptomatic=was_symptomatic;
graphdata.child_generation=child_generation;
graphdata.real_group=real_group;
graphdata.control_group=control_group;
graphdata.time_infected=time_E;
graphdata.time_symptomatic=time_symp;

plotdata.S=S_mat;
plotdata.E=E_mat;
plotdata.I=I_mat;
plotdata.symp=symp_mat;
plotdata.R=R_mat;
plotdata.timevect=(0:numsteps)*delta_t/hours_per_day;
plotdata.group_time=group_flag_time;
plotdata.real_group=real_group;
plotdata.class_time=class_flag_time;


end




