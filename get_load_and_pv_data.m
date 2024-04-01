function [PL,PV] = get_load_and_pv_data(start_date, resolution, time_horizon, desired_PVcapacity)
    
    start_date = datetime(start_date);                 % Convert type from (str=>datetime)
    end_datetime = start_date + minutes(time_horizon); % Calculate end datetime
    
    % Get load_data and pv_data from (.csv) file from either 5 min or 15 min
    % This data is forecasted from ML model.
    % The format of a DataFrame must be 'datetime' and 'Ptot (kW)'
    load_data = readtable(strcat('load_data_', num2str(resolution), 'minresample_concat.csv'),VariableNamingRule="preserve");
    load_data.Properties.VariableNames{'Ptot (kW)'} = 'Load_kW';
    load_data = load_data(:, {'datetime', 'Load_kW'});
    
    pv_data = readtable(strcat('pv_data_', num2str(resolution), 'minresample_concat.csv'),VariableNamingRule="preserve");
    pv_data.Properties.VariableNames{'Ptot (kW)'} = 'PV_kW';
    pv_data = pv_data(:, {'datetime', 'PV_kW'});

    % Filter the data within the specific range
    load_data = load_data(load_data.datetime >= start_date & load_data.datetime < end_datetime, :);
    pv_data = pv_data(pv_data.datetime >= start_date & pv_data.datetime < end_datetime, :);

    % Join load_data and pv_data (The data must have no Nan)
    data = innerjoin(load_data, pv_data, 'Keys', 'datetime');

    % This solar profile is emulated from EE building which has the installtion capacity
    % of 8 kWp, the pv generation power is scaled up to the desired capacity
    source_capacity = 8;                           % PV installation capacity of source
    PV_scale_factor = desired_PVcapacity/source_capacity; % scale up from source to desired capacity (kW)
    PV = PV_scale_factor*data.PV_kW;
    PL = data.Load_kW;
end
