program verificaDiferenca;
var
  num1, num2: integer;
  diferente: boolean;
begin
  read(num1, num2);
  diferente := not (num1 = num2);
  if diferente then
    write(true)
  else
    write(false);
end.
