program parOuImpar;
var
  num: integer;
  ehPar: boolean;
begin
  read(num);
  ehPar := (num mod 2 = 0);
  if ehPar then
    write(true)
  else
    write(false);
end.
