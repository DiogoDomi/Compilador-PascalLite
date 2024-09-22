program calculaMedia;
var
  num1, num2, num3, media: integer;
begin
  read(num1, num2, num3);
  media := (num1 + num2 + num3) div 3;
  write(media);
end.
