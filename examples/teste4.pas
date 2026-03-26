program comparaNumeros;
var
  num1, num2: integer;
  resultado: boolean;
begin
  read(num1, num2);
  resultado := num1 > num2;
  if resultado then
    write(true)
  else
    write(false);
end.
