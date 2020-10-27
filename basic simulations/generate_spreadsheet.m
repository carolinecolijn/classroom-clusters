

clear

% what type of class are you simulating?
% 0 elementary school
% 1 highschool morning 
% 2 highschool afternoon
class_type=0;


% labels of the four protocols
protocol_name{1}='I'; protocol_name{2}='II'; protocol_name{3}='III'; protocol_name{4}='IV';

% the 4 different beta settings
panel_name{1}='env beta low, index same';
panel_name{2}='env beta low, index higher';
panel_name{3}='env beta high, index same';
panel_name{4}='env beta high, index higher';

%  labels of the 5 "worlds" (different assumptions on parameters, see
%  below)
world_name{1}='a'; world_name{2}='b'; world_name{3}='c'; world_name{4}='d'; world_name{5}='e';

big_multi_stats=[];

% basic rate of transimission
beta_base=0.003;

% parameters to do with running the simulations
runstuff.num_sims = 1000;  % number of simulations per choice of params
runstuff.maxDays=50; % how many days each sim
runstuff.seed=1234; % random number seed


params.frac_asymp=0.4; % fraction of students infected who are asymptomatic


% parameters about the class and its structure
% depends on class type
switch class_type
    case 0
        params.hour_per_day=6; % hours of class contact per day
        params.class_size=25; % students in class
        params.num_real_groups=5; % number of contact groups
        params.TuesdayFriday=0; % if this is zero, class meets Monday to Friday (like elementary schools and highshcools...
        % in the morning in BC. If it is 1 then class only meet Tuesdays and
        % Thursdays like one cohort in highschools in BC.)
    case 1
        params.hour_per_day=2.5;
        params.class_size=30;
        params.num_real_groups=6;
        params.TuesdayFriday=0;
    case 2
        params.hour_per_day=2.5;
        params.class_size=15;
        params.num_real_groups=15;
        params.TuesdayFriday=1;
end
        
        
        
        
% for each of the five different worlds
for world=1:5
    
  switch world
    case 1
      params.mu_pip=1; % length of presymptomatic infectious period
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

  % for four different beta settings  
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
    
    
    % for four different protocols: see paper for explanations
    for prot=1:4

        % days_delay - how long from being tested to getting result
        % num_control_groups - how many contact groups people think there
        % are
        % tests_to_shutdown_group- how many positive tests until you shut
        % down a group
        % test_to_shutdown_class - how many group have to be shutdown until
        % you shutdown the class (yes, name is inaccuarate)
        
      switch prot
        case 1  % baseline
            protoc.days_delay=100; protoc.num_control_groups=1;
            protoc.tests_to_shutdown_group=1000; protoc.tests_to_shutdown_class=1000; 
        case 2  % contact model
            protoc.days_delay=2; protoc.num_control_groups=5;
            protoc.tests_to_shutdown_class=1000; protoc.tests_to_shutdown_group=1;
        case 3  % two groups is an outbreak
            protoc.days_delay=2; protoc.num_control_groups=5;
            protoc.tests_to_shutdown_class=2; protoc.tests_to_shutdown_group=1;
        case 4  % whole class
            protoc.days_delay=2; protoc.num_control_groups=1;
            protoc.tests_to_shutdown_class=1; protoc.tests_to_shutdown_group=1;
      end

      % whether or not the index case is asymptomatic
      for jj=0:1
        params.is_asymp=jj;
        
        % here we run a bunch of simulations with all the same parameters
        multi_stats=make_multi_run_stats(runstuff,params,protoc);
       
        % storing the parameters along witih the output of the data 
        % (couldn't find a better way of doing this with matlab)
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

        % append to all the other simulations done with different
        % parameters
        big_multi_stats=[big_multi_stats; multi_stats];

      end
    end
  end
end

% convert to a table 
tab=struct2table(big_multi_stats);

% write to a file name depending on what type of class you're simulating
switch class_type
    case 0
        writetable(tab,'elementary.csv')
    case 1 
        writetable(tab,'highschool_morning.csv')
    case 2
        writetable(tab,'highschool_afternoon.csv')
end




