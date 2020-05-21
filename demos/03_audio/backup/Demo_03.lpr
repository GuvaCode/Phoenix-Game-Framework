program Demo_03;

uses
    SysUtils, main, phxAudio_Uos;


var Game: TGame;

begin
  Game:= TGame.Create;
  Game.Run;
  Game.Free;
end.
