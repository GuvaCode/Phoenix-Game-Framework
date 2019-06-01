program Tutorial03_Animation;

{$APPTYPE GUI}

uses
  Main in 'Main.pas';

var Game: TGame;

{$R *.res}

begin
  Game:= TGame.Create;
  Game.Run;
  Game.Free;
end.
