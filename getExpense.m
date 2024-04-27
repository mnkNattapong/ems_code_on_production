function [profit,expense,revenue] = getExpense(Pnet,Buy_rate,Sell_rate,Resolution)
    %%% This function is used to calculate profit, expense, and revenue.
    %%% The resolution must be in hour.
 expense = min(0,Pnet).*Buy_rate*Resolution ; %  return the negative sign
 revenue = max(0,Pnet).*Sell_rate*Resolution ; % return the positive sign
 profit = revenue + expense ;  % net profit is revenue + expense
end