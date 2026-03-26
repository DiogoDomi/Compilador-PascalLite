program encontraMaior;
var
  num1, num2, maior: integer;
begin
  read(num1, num2);
  if num1 > num2 then
    maior := num1
  else
    maior := num2;
  write(maior);
end.
