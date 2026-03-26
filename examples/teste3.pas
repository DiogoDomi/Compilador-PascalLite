program calculaFatorial;
var
  num, fatorial, contador: integer;
begin
  read(num);
  fatorial := 1;
  contador := 1;
  while contador <= num do
  begin
    fatorial := fatorial * contador;
    contador := contador + 1;
  end;
  write(fatorial);
end.
