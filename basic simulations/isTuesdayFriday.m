function out=isTuesdayFriday(currentTime,hours_per_day)

day=currentTime/hours_per_day;
day_of_week=mod(day,7);
out=(day_of_week>1 & day_of_week<2) | (day_of_week>4 & day_of_week<5);

end