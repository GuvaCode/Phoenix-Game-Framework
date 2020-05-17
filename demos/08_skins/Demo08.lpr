program Demo08;

uses
    SysUtils, main;


var Game: TGame;

{$R *.res}

begin
  Game:= TGame.Create;
  Game.Run;
  Game.Free;
end.
