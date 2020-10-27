% this just runs a bunch of simulations
function multi_run_stats=make_multi_run_stats(runstuff,params,protoc)

num_sims=runstuff.num_sims; % number of simulations
rng(runstuff.seed) % random number seed set

multi_run_stats.days_asymp_lax=zeros(num_sims,1);
multi_run_stats.days_asymp_strict=zeros(num_sims,1);
multi_run_stats.total_infected=zeros(num_sims,1);
multi_run_stats.total_not_detected=zeros(num_sims,1);
multi_run_stats.students_affected=zeros(num_sims,1);
multi_run_stats.students_disrupted=zeros(num_sims,1);
multi_run_stats.shutdowntime=zeros(num_sims,1);
multi_run_stats.groups_shut_down=zeros(num_sims,1);
multi_run_stats.class_shut_down=zeros(num_sims,1);

for kk=1:num_sims
  
   % run simulation
  stats=class_sim_groups(runstuff,params,protoc);
  % store in vector
  multi_run_stats(kk,1)=stats;
     
end


end

      
