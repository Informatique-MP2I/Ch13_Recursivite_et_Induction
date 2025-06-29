let rec hanoi_towers n source spare target =
  if n = 1 then
    Printf.printf "Move disk from %s to %s\n" source target
  else
    begin
      hanoi_towers (n-1) source target spare;
      hanoi_towers 1 source spare target;
      hanoi_towers (n-1) spare source target;
    end
