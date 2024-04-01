function [Buy_rate,Sell_rate] = getBuySellrate(start_date,resolution,time_horizon,TOU_CHOICE)
    
    start_date = datetime(start_date); % Convert type from (str=>datetime)
    t1 = start_date; 
    t2 = t1 + minutes(time_horizon);   % Calculate end datetime
  
    vect = t1:minutes(resolution):t2; 
    vect(end) = []; vect = vect';
    vechour = hour(vect);

    switch TOU_CHOICE
        case 'smart1'
        % buy_rate = [0-10:00)     2THB, 
        %            [10:00-14:00] 3THB,
        %            (14:00-18:00) 5THB,
        %            [18:00-22:00] 7THB,
        %            (22:00-24:00) 2THB
        % sell_rate = [18:00-22:00] 2.5THB and 2THB all other
        % times

        Buy_rate = 2*(vechour >= 0  & vechour <  10)+...
                   3*(vechour >= 10 & vechour <= 14)+...
                   5*(vechour >  14 & vechour <  18)+...
                   7*(vechour >= 18 & vechour <= 22)+...
                   2*(vechour >  22 & vechour <= 23);
        Sell_rate = 2*ones(length(vechour),1); 
        Sell_rate(vechour >= 18 & vechour <= 22) = 2.5 ;

        case 'nosell'
        % buyrate is just like case 'smart1' but customers cannot sell the power
        Buy_rate = 2*(vechour >= 0  & vechour <  10)+...
                   3*(vechour >= 10 & vechour <= 14)+...
                   5*(vechour >  14 & vechour <  18)+...
                   7*(vechour >= 18 & vechour <= 22)+...
                   2*(vechour >  22 & vechour <= 23);
        Sell_rate = zeros(length(vechour),1); 

        case 'THcurrent'
        % Current rate  (not smart), sell_rate = 2 THB flat, 
        % buy_rate = 5.8 THB during [9:00-23:00] and 2.6 THB otherwise
        Buy_rate = 5.8*(vechour >= 9 & vechour <= 23)+...
                   2.6*(vechour >= 0 & vechour < 9);
        Sell_rate = 2*ones(length(vechour),1);
    end
end