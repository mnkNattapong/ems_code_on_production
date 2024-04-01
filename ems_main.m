% ---- user-input desired optimize modules and each module's weight
% Each weight must corresponds to each module.

% main_objective = 'energyfromgrid';
main_objective = 'energycost';
% main_objective = 'profit';

other_modules = {'multibatt','chargebatt','smoothcharge'};
module = [{main_objective}, other_modules];

% User-input weight of each module 
weight = [1 1 1 1];
% Check compatability
if length(module) ~= length(weight) || length(module) > 4
    error('Lengths of module and weight must be the same.');
end

% Set default weight
w_e = 0; w_m = 0; w_c = 0; w_s = 0;

% ---- Check main objective weight ----
if ismember('energyfromgrid', module)
    idx = find(ismember(module, 'energyfromgrid'));
    w_e = weight(idx);
end

if ismember('energycost', module)
    idx = find(ismember(module, 'energycost'));
    w_e = weight(idx);
end

if ismember('profit', module)
    idx = find(ismember(module, 'profit'));
    w_e = weight(idx);
end

% ---- Check other modules weights ----
if ismember('multibatt', module)
    idx = find(ismember(module, 'multibatt'));
    w_m = weight(idx);
end

if ismember('chargebatt', module)
    idx = find(ismember(module, 'chargebatt'));
    w_c = weight(idx);
end

if ismember('smoothcharge', module)
    idx = find(ismember(module, 'smoothcharge'));
    w_s = weight(idx);
end

% ---- user-input parameter ----
start_date   = '2023-03-16 00:00:00';           % Start date (str format: YYYY-MM-DD HH:mm:ss)
                                                % Note that: Default time is 00:00:00                                           
resolution   = 15;                     % Resolution in minutes (int)
time_horizon = 3*24*60;                % Optimization horizon in minutes (int)
                                            % Day-ahead (DA)      : Horizon in xx days (resolution 15 mins)
                                            % Intra-day (HA)      : Horizon in xx hours (resolution 5 mins)
pv_capacity  = 50;                     % Solar panel installation capacity in kWp (int) 

% TOU_CHOICE = 'smart1';             % Choice for TOU
% TOU_CHOICE = 'nosell';
TOU_CHOICE = 'THcurrent';


% ---- get load&pv data and buy&sell rate ----
[PARAM.PL,PARAM.PV] = get_load_and_pv_data(start_date, resolution, time_horizon, pv_capacity);
[PARAM.Buy_rate,PARAM.Sell_rate] = getBuySellrate(start_date,resolution,time_horizon,TOU_CHOICE);

% ---- save parameters ----
PARAM.main_objective = main_objective;
PARAM.start_date  = start_date;
PARAM.Resolution  = resolution;
PARAM.Horizon     = time_horizon; 
PARAM.PV_capacity = pv_capacity;
PARAM.TOU_CHOICE  = TOU_CHOICE;
PARAM.weight_mainobjective = w_e;
PARAM.weight_multibatt = w_m;
PARAM.weight_chargebatt = w_c;
PARAM.weight_smoothcharge  = w_s; 

% Battery parameters
PARAM.battery.charge_effiency = [0.95 0.95]; %bes charge eff
PARAM.battery.discharge_effiency = [0.95*0.93 0.95*0.93]; %  bes discharge eff note inverter eff 0.93-0.96
PARAM.battery.discharge_rate = [30 30]; % kW max discharge rate
PARAM.battery.charge_rate = [30 30]; % kW max charge rate
PARAM.battery.actual_capacity = [125 125]; % kWh soc_capacity 
PARAM.battery.initial = [50 50]; % userdefined int 0-100 %
PARAM.battery.min = [20 20]; %min soc userdefined int 0-100 %
PARAM.battery.max = [80 80]; %max soc userdefined int 0-100 %
%end of 2 batt

PARAM.battery.num_batt = length(PARAM.battery.actual_capacity);

% end of ---- parameters ----
%%
solution_path = 'solution';
sol = ems_opt(PARAM,1,solution_path);

