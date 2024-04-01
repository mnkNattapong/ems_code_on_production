function sol = ems_opt(PARAM,is_save,save_path) 
    %%% This function is used to solve optimization problem, consisting 3 parts. 
    %%% (I) Define optimization variable 
    %%% (II) Define constraints
    %%% (III) Call the solver and save parameters.
    
    options = optimoptions('intlinprog','MaxTime',40);
    
    if rem(PARAM.Horizon, PARAM.Resolution) % Check if the optimization horizon and resolution are compatible
        error('Incorrect, horizon must be a multiple of resolution')
    else
        length_optimvar = PARAM.Horizon/PARAM.Resolution; % Length of each optimization variable
    end
    
    % Change the unit of Resolution from (minute => hour) to be used in Expense calculation
    minutes_in_hour = 60;
    resolution_in_hour = PARAM.Resolution/minutes_in_hour;
    
    Pnet =      optimvar('Pnet',length_optimvar,'LowerBound',-inf,'UpperBound',inf);
    Pdchg =     optimvar('Pdchg',length_optimvar,PARAM.battery.num_batt,'LowerBound',0,'UpperBound',inf);
    xdchg =     optimvar('xdchg',length_optimvar,PARAM.battery.num_batt,'LowerBound',0,'UpperBound',1,'Type','integer');
    Pchg =      optimvar('Pchg',length_optimvar,PARAM.battery.num_batt,'LowerBound',0,'UpperBound',inf);
    xchg =      optimvar('xchg',length_optimvar,PARAM.battery.num_batt,'LowerBound',0,'UpperBound',1,'Type','integer');
    soc =       optimvar('soc',length_optimvar+1,PARAM.battery.num_batt,'LowerBound',ones(length_optimvar+1,PARAM.battery.num_batt).*PARAM.battery.min,'UpperBound',ones(length_optimvar+1,PARAM.battery.num_batt).*PARAM.battery.max);
   
    % Define optimization variable corresponding to the main objective
    if strcmp(PARAM.main_objective, 'energyfromgrid')
        u = optimvar('u', length_optimvar, 'LowerBound', 0, 'UpperBound', inf);  % u is the upper bound of the energy
        total_obj = PARAM.weight_mainobjective*sum(u); 
        prob =      optimproblem('Objective',total_obj);
        %--constraint for energy procurement
        prob.Constraints.epicons1 = -Pnet - u <= 0;

    elseif strcmp(PARAM.main_objective, 'energycost')
        u = optimvar('u', length_optimvar, 'LowerBound', 0, 'UpperBound', inf);  % u is the upper bound of the expense 
        total_obj = PARAM.weight_mainobjective*sum(u); 
        prob =      optimproblem('Objective',total_obj);
        %--constraint for buying electricity
        prob.Constraints.epicons1 = -resolution_in_hour*PARAM.Buy_rate.*Pnet - u <= 0;

    elseif strcmp(PARAM.main_objective, 'profit')
        u = optimvar('u', length_optimvar, 'LowerBound', -inf, 'UpperBound', inf); % u is the upper bound of the profit
        total_obj = PARAM.weight_mainobjective*sum(u); 
        prob =      optimproblem('Objective',total_obj);
        %--constraint for buying/selling electricity
        prob.Constraints.epicons1 = -resolution_in_hour*PARAM.Buy_rate.*Pnet - u <= 0;
        prob.Constraints.epicons2 = -resolution_in_hour*PARAM.Sell_rate.*Pnet - u <= 0;
    end

    if PARAM.weight_multibatt ~= 0 % Add soc diff objective
        % Define optimvar for SoC diff objective
        % s = Upper bound of |SoC diff| objective
        s =         optimvar('s',length_optimvar,'LowerBound',0,'UpperBound',inf);   

        total_obj = total_obj + PARAM.weight_multibatt*sum(s); % Add soc diff objective
        prob =      optimproblem('Objective', total_obj);
        
        prob.Constraints.epicons1 = -resolution_in_hour*PARAM.Buy_rate.*Pnet - u <= 0;
        % %-- constraint battery should be used equally (this code is only usable for 2 batteries)
        prob.Constraints.battdeviate1 = soc(2:length_optimvar+1,1) - soc(2:length_optimvar+1,2) <= s;
        prob.Constraints.battdeviate2 = -s <= soc(2:length_optimvar+1,1) - soc(2:length_optimvar+1,2);
    end

    if PARAM.weight_chargebatt ~= 0  
        % Add term encourange charging battery
        total_obj = total_obj + PARAM.weight_chargebatt*sum(sum((PARAM.battery.max.*(ones(length_optimvar+1,PARAM.battery.num_batt)) - soc) ...
                                ./(ones(length_optimvar+1,PARAM.battery.num_batt).*(PARAM.battery.max - PARAM.battery.min)),2));
        prob =      optimproblem('Objective', total_obj);
        
        prob.Constraints.epicons1 = -resolution_in_hour*PARAM.Buy_rate.*Pnet - u <= 0;
        % %-- constraint battery should be used equally (this code is only usable for 2 batteries)
        prob.Constraints.battdeviate1 = soc(2:length_optimvar+1,1) - soc(2:length_optimvar+1,2) <= s;
        prob.Constraints.battdeviate2 = -s <= soc(2:length_optimvar+1,1) - soc(2:length_optimvar+1,2);
       
    end

    if PARAM.weight_smoothcharge ~= 0 % Add non fluctuation charge and discharge objective
        % Define optimvars for non fluctuation charge and discharge
        % upper_bound_xchg is Upper bound of |xchg(t)-xchg(t-1)| objective
        % upper_bound_xdchg is Upper bound of |xdchg(t)-xdchg(t-1)| objective
        upper_bound_xchg = optimvar('upper_bound_xchg',length_optimvar-1,PARAM.battery.num_batt,'LowerBound',0,'UpperBound',1,'Type','integer');      
        upper_bound_xdchg = optimvar('upper_bound_xdchg',length_optimvar-1,PARAM.battery.num_batt,'LowerBound',0,'UpperBound',1,'Type','integer');    
        
        % Add non fluctuation charge and discharge objective.
        % Assume that the weight is equal for both xchg and xdchg.
        total_obj = total_obj + PARAM.weight_smoothcharge * (sum(upper_bound_xchg(:)) + sum(upper_bound_xdchg(:)));
        prob =      optimproblem('Objective',total_obj);
        
        prob.Constraints.epicons1 = -resolution_in_hour*PARAM.Buy_rate.*Pnet - u <= 0;
        % %-- constraint battery should be used equally (this code is only usable for 2 batteries)
        prob.Constraints.battdeviate1 = soc(2:length_optimvar+1,1) - soc(2:length_optimvar+1,2) <= s;
        prob.Constraints.battdeviate2 = -s <= soc(2:length_optimvar+1,1) - soc(2:length_optimvar+1,2);
        
        % %-- Constraint non fluctuating charge and discharge
        % abs(xchg(t)-xchg(t-1)) <= upper_bound_xchg
        prob.Constraints.non_fluct_xchg_con1 = xchg(1:end-1,:)-xchg(2:end,:) <= upper_bound_xchg;
        prob.Constraints.non_fluct_xchg_con2 = -upper_bound_xchg <= xchg(1:end-1,:)-xchg(2:end,:);
        % abs(xdchg(t)-xdchg(t-1)) <= upper_bound_xdchg
        prob.Constraints.non_fluct_xdchg_con1 = xdchg(1:end-1,:)-xdchg(2:end,:) <= upper_bound_xdchg;
        prob.Constraints.non_fluct_xdchg_con2 = -upper_bound_xdchg <= xdchg(1:end-1,:)-xdchg(2:end,:);
    end
        
    
    % Constraint part

    %--battery constraint

    prob.Constraints.chargeconsbatt = Pchg <= xchg.*(ones(length_optimvar,PARAM.battery.num_batt).*PARAM.battery.charge_rate);
    
    prob.Constraints.dischargeconsbatt = Pdchg   <= xdchg.*(ones(length_optimvar,PARAM.battery.num_batt).*PARAM.battery.discharge_rate);
    
    prob.Constraints.NosimultDchgAndChgbatt = xchg + xdchg >= 0;
    
    prob.Constraints.NosimultDchgAndChgconsbatt1 = xchg + xdchg <= 1;
    
    %--Pnet constraint
    prob.Constraints.powercons = Pnet == PARAM.PV + sum(Pdchg,2) - PARAM.PL - sum(Pchg,2);
    
    %end of static constraint part
    
    %--soc dynamic constraint 
    soccons = optimconstr(length_optimvar+1,PARAM.battery.num_batt);
    
    soccons(1,1:PARAM.battery.num_batt) = soc(1,1:PARAM.battery.num_batt)  == PARAM.battery.initial ;
    for j = 1:PARAM.battery.num_batt
        soccons(2:length_optimvar+1,j) = soc(2:length_optimvar+1,j)  == soc(1:length_optimvar,j) + ...
                                 (PARAM.battery.charge_effiency(:,j)*100*resolution_in_hour/PARAM.battery.actual_capacity(:,j))*Pchg(1:length_optimvar,j) ...
                                    - (resolution_in_hour*100/(PARAM.battery.discharge_effiency(:,j)*PARAM.battery.actual_capacity(:,j)))*Pdchg(1:length_optimvar,j);
        
    end
    prob.Constraints.soccons = soccons;
    
    %---solve for optimal sol
    [sol, ~, exitflag] = solve(prob,'Options',options);
    sol.exitflag = exitflag;
    sol.PARAM = PARAM;
    if is_save == 1
        % Filename is in the format: '(TOU_CHOICE)_(resolution)_(setting_weight)_(start_date).mat' 
        % e.g. 'THcurrent_15min_Wmainobjective_Wmultibatt_Wchargebatt_Wsmoothcharge_2023-03-16.mat'
        save(strcat(save_path,'/ems/',PARAM.TOU_CHOICE,'_', ...
            num2str(PARAM.Resolution),'min_', ...
            num2str(PARAM.weight_mainobjective), '_', ...
            num2str(PARAM.weight_multibatt), '_', ...
            num2str(PARAM.weight_chargebatt), '_', ...
            num2str(PARAM.weight_smoothcharge), '_',...
            PARAM.start_date,'.mat'),'-struct','sol')
    end
end