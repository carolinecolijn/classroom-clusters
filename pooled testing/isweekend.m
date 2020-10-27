function out=isweekend(currentTime,hours_per_day)

day=currentTime/hours_per_day;
day_of_week=mod(day,7);
out=(day_of_week>5 & day_of_week<7);

end