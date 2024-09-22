program maiorMenor;
var
  num, maior, menor: integer;
  contador: integer;
begin
  maior := -32768; (* menor valor possível para integer *)
  menor := 32767;  (* maior valor possível para integer *)
  contador := 0;
  while contador < 5 do
  begin
    read(num);
    if num > maior then
      maior := num;
    if num < menor then
      menor := num;
    contador := contador + 1;
  end;
  write(maior, menor);
end.
