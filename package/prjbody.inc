//Project body
  NewSource:='program myGame;'+LineEnding
    +LE
    +'uses'+LE
    +'SysUtils // this includes the LCL widgetset'+LineEnding
    +'  { you can add units after this };'+LE
    +LE
    +'var Game: TGame;'+LE
    +'begin'+LE
    +'  Game:= TGame.Create;'+LE
    +'  Game.Run;'+LE
    +'  Game.Free;'+LE
    +'end.'+LE
    +LE;  

