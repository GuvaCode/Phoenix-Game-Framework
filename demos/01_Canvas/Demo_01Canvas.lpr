program Demo_01Canvas;

uses
    SysUtils, main;


var Game: TGame;

begin
  Game:= TGame.Create;
  Game.Run;
  Game.Free;
end.
