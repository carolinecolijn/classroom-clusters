

clear

world=4;
prot=1;
panel=4;

protocol_name{1}='I'; protocol_name{2}='II'; protocol_name{3}='III'; protocol_name{4}='IV';
panel_name{1}='env beta low, index same';
panel_name{2}='env beta low, index higher';
panel_name{3}='env beta high, index same';
panel_name{4}='env beta high, index higher';

world_name{1}='a'; world_name{2}='b'; world_name{3}='c'; world_name{4}='d'; world_name{5}='e';

big_multi_stats=[];

beta_base=0.003;

% one set of parameters, do a bunch of simulations
runstuff.num_sims = 1000;
runstuff.maxDays=50;
runstuff.seed=1234;

params.hour_per_day=6;
params.class_size=25;
params.frac_asymp=0.4;
params.num_real_groups=5;
params.TuesdayFriday=0;


params.mu_pip=2;
params.beta_aerosol_factor=.25; % how much less infectious outside groups
params.asymp_ratio=0.8; % how much less infectious are asymptotic people
   

 params.beta_index_factor=3; % how many more times infectious is index case 
 params.beta_base=beta_base*2;
  
    
    for onsite=0:1
    
        switch onsite % how fast do you learn test results
            case 1
                protoc.days_delay=2/24; % two hours
            case 0
                protoc.days_delay=2; % two days
        end
        
       
        
        for freq=1:3 % how often do you do pooled testing

          % these two lines set to baseline protocol (sympomatics go home)
          protoc.num_control_groups=1; 
          protoc.tests_to_shutdown_group=1000; protoc.tests_to_shutdown_class=1000; 
      
          switch freq
          case 1
            params.testingDays=1000*runstuff.maxDays; % never
          case 2
           params.testingDays=7; % every week
          case 3
            params.testingDays=2; % every three days
          end
         

      for jj=0:1

        params.is_asymp=jj; % is index case asymptomatic
        
        multi_stats=make_multi_run_stats(runstuff,params,protoc); % the actual simulations
        
        % put params into the data along with the results
        for k=1:runstuff.num_sims
            multi_stats(k).onsite=onsite;
            multi_stats(k).days_delay=protoc.days_delay;
            multi_stats(k).freq=freq;
            multi_stats(k).testingFrequency=params.testingDays;
            multi_stats(k).panel=panel;
            multi_stats(k).panelname=panel_name{panel};
            multi_stats(k).asymp_ratio=params.asymp_ratio;
            multi_stats(k).beta_aerosol_factor=params.beta_aerosol_factor;
            multi_stats(k).mu_pip=params.mu_pip;
            multi_stats(k).world=world_name{world};
            multi_stats(k).prot=prot;
            multi_stats(k).protocol=protocol_name{prot};
            multi_stats(k).simulation_number=k;
            multi_stats(k).betaenv=params.beta_base;
            multi_stats(k).betaindex=params.beta_index_factor;
            multi_stats(k).index_asymp=params.is_asymp;
        end

        % throw results into big matrix
        big_multi_stats=[big_multi_stats; multi_stats];

      end
    end
    end

% convert to table and output to a file
tab=struct2table(big_multi_stats);
writetable(tab,'elementary_pooled.csv')




