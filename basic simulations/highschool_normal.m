% In this file I hacked the code for the other settings. I made it so that 
% for each day it summed together the results of four different simulations
% of a single class. Please see generate_spreadsheet.m for more detailed
% comments

clear

protocol_name{1}='I'; protocol_name{2}='II'; protocol_name{3}='III'; protocol_name{4}='IV';
panel_name{1}='env beta low, index same';
panel_name{2}='env beta low, index higher';
panel_name{3}='env beta high, index same';
panel_name{4}='env beta high, index higher';

world_name{1}='a'; world_name{2}='b'; world_name{3}='c'; world_name{4}='d';world_name{5}='e';

big_multi_stats=[];
big_NHS=[];

beta_base=0.003;

% one set of parameters, do a bunch of simulations
runstuff.num_sims = 4000;
runstuff.maxDays=50;
runstuff.seed=1234;

classes_per_day=4;

params.hour_per_day=1.25;
params.class_size=30;
params.frac_asymp=0.4;
params.num_real_groups=6;
params.TuesdayFriday=0;

for world=4:4
    
  switch world
    case 1
      params.mu_pip=1;
      params.beta_aerosol_factor=.05; % how much less infectious outside groups
      params.asymp_ratio=0.2; % how much less infectious are asymptotic people
    case 2
      params.mu_pip=2;
      params.beta_aerosol_factor=.05; % how much less infectious outside groups
      params.asymp_ratio=0.2; % how much less infectious are asymptotic people
    case 3
      params.mu_pip=2;
      params.beta_aerosol_factor=.05; % how much less infectious outside groups
      params.asymp_ratio=0.8; % how much less infectious are asymptotic people
    case 4
      params.mu_pip=2;
      params.beta_aerosol_factor=.25; % how much less infectious outside groups
      params.asymp_ratio=0.8; % how much less infectious are asymptotic people
    case 5
      params.mu_pip=2;
      params.beta_aerosol_factor=.5; % how much less infectious outside groups
      params.asymp_ratio=0.8; % how much less infectious are asymptotic people
  end

    
  for panel=1:4

    switch panel
        case 1
            params.beta_index_factor=1; % how many more times infectious is index case 
            params.beta_base=beta_base;
        case 2
            params.beta_index_factor=1; % how many more times infectious is index case 
            params.beta_base=beta_base*2;
        case 3
            params.beta_index_factor=3; % how many more times infectious is index case 
            params.beta_base=beta_base*1;
        case 4
            params.beta_index_factor=3; % how many more times infectious is index case 
            params.beta_base=beta_base*2;
    end
    
    for prot=1:4

      switch prot
        case 1
            protoc.days_delay=100; protoc.num_control_groups=1;
            protoc.tests_to_shutdown_group=1000; protoc.tests_to_shutdown_class=1000; 
        case 2
            protoc.days_delay=2; protoc.num_control_groups=6;
            protoc.tests_to_shutdown_class=1000; protoc.tests_to_shutdown_group=1;
        case 3
            protoc.days_delay=2; protoc.num_control_groups=6;
            protoc.tests_to_shutdown_class=2; protoc.tests_to_shutdown_group=1;
        case 4
            protoc.days_delay=2; protoc.num_control_groups=1;
            protoc.tests_to_shutdown_class=1; protoc.tests_to_shutdown_group=1;
      end

      for jj=0:1

        params.is_asymp=jj;
        multi_stats=make_multi_run_stats(runstuff,params,protoc);
        for k=1:runstuff.num_sims
            multi_stats(k).panel=panel;
            multi_stats(k).panelname=panel_name{panel};
            multi_stats(k).asymp_ratio=params.asymp_ratio;
            multi_stats(k).beta_aerosol_factor=params.beta_aerosol_factor;
            multi_stats(k).mu_pip=params.mu_pip;
            multi_stats(k).world=world_name{world};
            multi_stats(k).protocol=protocol_name{prot};
            multi_stats(k).simulation_number=k;
            multi_stats(k).betaenv=params.beta_base;
            multi_stats(k).betaindex=params.beta_index_factor;
            multi_stats(k).index_asymp=params.is_asymp;
        end

        big_multi_stats=[big_multi_stats; multi_stats];

        % combine things by rows to simulate multiple classes in one day
        % NHS= normal highschool stats
        num_actual_sims=runstuff.num_sims/classes_per_day;
        clear foo
        foo.days_asymp_lax=zeros(num_actual_sims,1);
        foo.days_asymp_strict=zeros(num_actual_sims,1);
        foo.total_infected=zeros(num_actual_sims,1);
        foo.total_not_detected=zeros(num_actual_sims,1);
        foo.students_affected=zeros(num_actual_sims,1);
        foo.students_disrupted=zeros(num_actual_sims,1);

        
        for j=1:num_actual_sims
   
          inds=(1+(j-1)*classes_per_day:j*classes_per_day);
          foo(j,1).days_asymp_lax=sum(vertcat(multi_stats(inds).days_asymp_lax));
          foo(j,1).days_asymp_strict=sum(vertcat(multi_stats(inds).days_asymp_strict));
          foo(j,1).total_infected=sum(vertcat(multi_stats(inds).total_infected));
          foo(j,1).total_not_detected=sum(vertcat(multi_stats(inds).total_not_detected));
          foo(j,1).students_affected=sum(vertcat(multi_stats(inds).students_affected));
          foo(j,1).students_disrupted=sum(vertcat(multi_stats(inds).students_disrupted));
          foo(j,1).panel=panel;
           foo(j,1).panelname=panel_name{panel};
            foo(j,1).asymp_ratio=params.asymp_ratio;
            foo(j,1).beta_aerosol_factor=params.beta_aerosol_factor;
            foo(j,1).mu_pip=params.mu_pip;
            foo(j,1).world=world_name{world};
            foo(j,1).protocol=protocol_name{prot};
            foo(j,1).simulation_number=k;
            foo(j,1).betaenv=params.beta_base;
            foo(j,1).betaindex=params.beta_index_factor;
            foo(j,1).index_asymp=params.is_asymp;
    
        end
        
         big_NHS=[big_NHS; foo];

        
      end
    end
  end
end



tab=struct2table(big_NHS);
writetable(tab,'highschool_normal.csv')




