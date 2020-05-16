program demo02_images;

uses
    SysUtils, main;


var Game: TGame;

begin
  Game:= TGame.Create;
  Game.Run;
  Game.Free;
end.
