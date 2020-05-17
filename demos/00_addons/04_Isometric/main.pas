unit main;

{$mode delphi}{$H+}

interface

uses SysUtils,

  phxTypes,
  phxMath,
  phxEvents,
  phxDevice,
  phxCanvas,
  phxApplication,
  phxFont,
  phxTexture,
  phxImage,
  phxSprite,
  phxInput,
  phxShape;

type

//------------------------------------------------------------------------------
TGame = class(TPHXApplication)
  private
    Device : TPHXDevice;
    Timer  : TPHXTimer;
    Canvas : TPHXCanvas;
    Images : TPHXImageList;
    Fonts  : TPHXFontList;
    Input  : TPHXInput;


    Player: TVector2f;
    Tile  : Integer;
    Selected: TVector2f;

    ShapePlayer: TPHXCircle;
    ShapeMap   : TPHXBox;

    ShowGrid: Boolean;
    ShowPassable: Boolean;

    function CollisionCheck(const Position: TVector2f): Boolean;

    procedure DrawMap;
    procedure DrawPlayer;
    procedure DrawGrid;
    procedure DrawSelection;
    procedure DrawPassable;
  protected
    procedure KeyPressed(Key: TPHXVirtualKey; Shift: TPHXShiftStates); override;
  public

    procedure Init; override;
    procedure Update; override;
    procedure Render; override;
    procedure Shutdown; override;
  end;


implementation

// Use opengl for rendering and freeimage for texture loading
uses
   phxOpenGL_GLFW3,
  phxGraphics_Vampyre,
  Map;

// http://opengameart.org/content/grassland-tileset

// http://laserbrainstudios.com/2010/08/the-basics-of-isometric-programming/
//------------------------------------------------------------------------------
procedure TGame.Init;
begin
  Device:= TPHXDevice.Create;
  Device.Initialize('Phoenix Demo', 800, 600);;

  Timer:= TPHXTimer.Create;

  Input:= TPHXInput.Create;
  //Input.Bindings.Add(IsButton3,

  Input.Bindings.Add(isButton3,  VK_NUM_ADD);
  Input.Bindings.Add(isButton4,  VK_NUM_SUBTRACT);


  // Create our canvas
  Canvas:= Device.CreateCanvas;

  Images:= TPHXImageList.Create(Device, Canvas);
  Images.LoadImage('content/Isometric/grassland_tiles.png');
  Images.LoadImage('content/Isometric/orc_regular_0.png');


  Images[0].Patterns.Add('Tile0',  0,  0, 64, 32, 32, 0);
  Images[0].Patterns.Add('Tile0',  0, 32, 64, 32, 32, 0);
  Images[0].Patterns.Add('Tile0', 64,  0, 64, 32, 32, 0);
  Images[0].Initialize;

  Images[0].Patterns.AddRectangular(64, 32, 32 ,0);
  Images[0].Patterns.List^[241-1].Height:= 64;
  Images[0].Patterns.List^[242-1].Height:= 64;
  Images[0].Patterns.List^[249-1].Height:= 64;
  Images[0].Patterns.List^[322-1].Height:= 64;
  Images[0].Patterns.List^[325-1].Height:= 64;
  Images[0].Patterns.List^[326-1].Height:= 64;

  Images[0].Patterns.List^[579-1].Width := 128;
  Images[0].Patterns.List^[579-1].Height:= 192;
  Images[0].Initialize;


  Images[1].Patterns.Add('Tile0',  0,  0, 128, 128, 64, 128-32);
  Images[1].Initialize;


  Fonts:= TPHXFontList.Create(Device, Canvas);
  Fonts.LoadFont('content/Calibri12.phxfnt');

  ShapePlayer:= TPHXCircle.Create(0.5);
  ShapeMap   := TPHXBox.Create(1.0);

  ShowGrid:= true;
  ShowPassable:= False;
end;

//------------------------------------------------------------------------------
procedure TGame.KeyPressed(Key: TPHXVirtualKey; Shift: TPHXShiftStates);
begin
  inherited;

  if Key = VK_G then
  begin
    ShowGrid:= not ShowGrid;
  end;
  if Key = VK_P then
  begin
    ShowPassable:= not ShowPassable;
  end;

end;

//------------------------------------------------------------------------------
procedure TGame.Update;
var X,Y: Integer;
var Position: TVector2f;
begin
  Timer.Update;

  Input.Update;

  // Get the tile under the mouse cursor
  Selected:= ScreenToWorld(Input.Mouse.X, Input.Mouse.Y);

  // It could be an idea to move x and y seperatly (with collision test for each),
  // that way we can slide along edges with the button pressed
  if isLeft in Input.States then
  begin
    Position.X:= Player.X - 1 * Timer.FrameTime;
    Position.Y:= Player.Y + 1 * Timer.FrameTime;

    // Move the player to the wanted position if no collision occures at the new position
    if CollisionCheck(Position) then
    begin
      Player.X:= Position.X;
      Player.Y:= Position.Y;
    end;
  end;
  if isRight in Input.States then
  begin
    Position.X:= Player.X + 1 * Timer.FrameTime;
    Position.Y:= Player.Y - 1 * Timer.FrameTime;

    // Move the player to the wanted position if no collision occures at the new position
    if CollisionCheck(Position) then
    begin
      Player.X:= Position.X;
      Player.Y:= Position.Y;
    end;
  end;
  if isUp in Input.States then
  begin
    Position.Y:= Player.Y - 1 * Timer.FrameTime;
    Position.X:= Player.X - 1 * Timer.FrameTime;

    // Move the player to the wanted position if no collision occures at the new position
    if CollisionCheck(Position) then
    begin
      Player.X:= Position.X;
      Player.Y:= Position.Y;
    end;
  end;
  if isDown in Input.States then
  begin
    Position.Y:= Player.Y + 1 * Timer.FrameTime;
    Position.X:= Player.X + 1 * Timer.FrameTime;

    // Move the player to the wanted position if no collision occures at the new position
    if CollisionCheck(Position) then
    begin
      Player.X:= Position.X;
      Player.Y:= Position.Y;
    end;
  end;

  // Select next tile with num +
  if isButton3 in Input.States then
  begin
    Tile:= Tile + 1;

    if Tile >= Images[0].Patterns.Count then
    begin
      Tile:= 0;
    end;

    Input.States:= Input.States - [isButton3];
  end;
  // Select prev tile with num -
  if isButton4 in Input.States then
  begin
    Tile:= Tile - 1;

    if Tile < 0 then
    begin
      Tile:= Images[0].Patterns.Count - 1;
    end;

    Input.States:= Input.States - [isButton4];
  end;
  // Draw on the background layer with mouse 1
  if isButton1 in Input.Mouse.States then
  begin
    X:= Trunc(Selected.X);
    Y:= Trunc(Selected.Y);

    if(X >= 0) and (X < MapWidth) and (Y>=0) and (Y < MapHeight) then
    begin
      MapData[0, Y, X]:= Tile;
    end;
  end;

  Device.Update;
end;

// Collisions are made in normal 2D, rectangular fasion, this function will return
// true if no collisions are detected.

// The player has a radius of 0.15 and each tile is 1x1
//------------------------------------------------------------------------------
function TGame.CollisionCheck(const Position: TVector2f): Boolean;
var WX,WY: Integer;
var Tile: Integer;
begin
  ShapePlayer.Radius := 0.15;
  ShapePlayer.CenterX:= Position.X;
  ShapePlayer.CenterY:= Position.Y;

  ShapeMap.SizeX:= 1;
  ShapeMap.SizeY:= 1;

  // There's really no need to check the whole map, only the squares that the player
  // intersects (Player.X - 0.15 to Player.X + 0.15)
  for WY:=0 to MapHeight-1 do
  begin
    for WX:= 0 to MapWidth-1 do
    begin
      Tile:= MapData[LayerPassable, WY, WX];

      // There is only one kind of shape for the map, here you could use a bunch of
      // different ones to make better collisions (Halftiles, slopes etc)
      if Tile <> 0 then
      begin
        ShapeMap.CenterX:= (WX + 0.5);
        ShapeMap.CenterY:= (WY + 0.5);

      //  if TPHXShape.Collide(ShapePlayer, ShapeMap, Vector2f_Zero) then
        if ShapePlayer.Collide(ShapeMap) then
        begin
          Result:= False;
          Exit;
        end;
      end;
    end;
  end;
  // No collision, okey to move
  Result:= True;
end;

//------------------------------------------------------------------------------
procedure TGame.Render;
begin
  Device.SetClearColor(clrBlack);
  Device.Clear;

  DrawMap;
  DrawGrid;
  DrawPassable;
  DrawSelection;

  // Draw the selected tile
  if Tile <> 0 then
  begin
    Images[0].Draw(100, 100, Tile-1);
  end;
  Fonts[0].TextOut(100-32, 100, IntToStr(Tile-1));


  Fonts[0].TextOut(0, 0 * 16, Format('PlayerX: %.2f', [Player.X]));
  Fonts[0].TextOut(0, 1 * 16, Format('PlayerY: %.2f', [Player.Y]));

  Fonts[0].TextOut(0, 2 * 16, Format('Grid [G]: %s', [BoolToStr(ShowGrid    , true)]));
  Fonts[0].TextOut(0, 3 * 16, Format('Passable [P]: %s', [BoolToStr(ShowPassable, true)]));

  Fonts[0].TextOut(700, 0, Format('%d fps', [Timer.FrameRate]));

  Fonts[0].TextOut(400, 0, Format('X: %.1f'#13'Y: %.1f', [ Selected.X, Selected.Y ]));


  CollisionCheck(Player);
  Canvas.Flush;


  Device.Flip;
end;

//------------------------------------------------------------------------------
procedure TGame.DrawMap;
var WX,WY: Integer;
var P    : TVector2f;
var Tile: Integer;
begin
  for WY :=0 to  MapHeight-1 do
  begin
    for WX := 0 to MapWidth-1 do
    begin
      Tile:= MapData[0, WY, WX];

      if Tile <> 0 then
      begin
        P:= WorldToScreen(WX, WY);

        Images[0].Draw(P.X, P.Y, Tile-1);
      end;
    end;
  end;

  DrawPlayer;

  // Layer #2
  for WY :=0 to  MapHeight-1 do
  begin
    for WX := 0 to MapWidth-1 do
    begin
      Tile:= MapData[1, WY, WX];

      if Tile <> 0 then
      begin
        P:= WorldToScreen(WX, WY);

        Images[0].Draw(P.X, P.Y, Tile-1);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TGame.DrawPlayer;
var P    : TVector2f;
begin
  P:= WorldToScreen(Player.X, Player.Y);

  Images[1].Draw(P.X, P.Y, 0);
end;

//------------------------------------------------------------------------------
procedure TGame.DrawGrid;
var WX,WY: Integer;
var P1,P2: TVector2f;
begin
  if not ShowGrid then Exit;

  Canvas.Texture:= nil;
  Canvas.Color  := clrGray;

  for WX :=0 to  MapWidth-1 do
  begin
    P1:= WorldToScreen(WX, 0);
    P2:= WorldToScreen(WX, MapHeight);

    Canvas.Line(P1, P2);
  end;
  for WY :=0 to  MapHeight-1 do
  begin
    P1:= WorldToScreen(0,       WY);
    P2:= WorldToScreen(MapWidth, WY);

    Canvas.Line(P1, P2);
  end;

  Canvas.Color:= clrWhite;
  Canvas.Flush;
end;

//------------------------------------------------------------------------------
procedure TGame.DrawSelection;
var P1,P2: TVector2f;
var P3,P4: TVector2f;
begin
  Canvas.Texture:= nil;
  Canvas.Color  := clrSilver;

  P1:= WorldToScreen(Trunc(Selected.X)    , Trunc(Selected.Y));
  P2:= WorldToScreen(Trunc(Selected.X) + 1, Trunc(Selected.Y));
  P3:= WorldToScreen(Trunc(Selected.X) + 1, Trunc(Selected.Y) + 1);
  P4:= WorldToScreen(Trunc(Selected.X)    , Trunc(Selected.Y) + 1);

  Canvas.Line(P1, P2);
  Canvas.Line(P2, P3);
  Canvas.Line(P3, P4);
  Canvas.Line(P4, P1);

  Canvas.Color:= clrWhite;
  Canvas.Flush;
end;

//------------------------------------------------------------------------------
procedure TGame.DrawPassable;
var WX,WY: Integer;
var P1,P2: TVector2f;
var P3,P4: TVector2f;
var Tile: Integer;
begin
  if not ShowPassable then Exit;

  Canvas.Texture:= nil;
  Canvas.Color  := clrRed;

  for WY :=0 to  MapHeight-1 do
  begin
    for WX := 0 to MapWidth-1 do
    begin
      Tile:= MapData[LayerPassable, WY, WX];

      if Tile <> 0 then
      begin
        P1:= WorldToScreen(WX    , WY);
        P2:= WorldToScreen(WX + 1, WY);
        P3:= WorldToScreen(WX + 1, WY + 1);
        P4:= WorldToScreen(WX    , WY + 1);

        Canvas.Line(P1, P2);
        Canvas.Line(P2, P3);
        Canvas.Line(P3, P4);
        Canvas.Line(P4, P1);
      end;

    end;
  end;
  Canvas.Color:= clrWhite;
  Canvas.Flush;
end;

//------------------------------------------------------------------------------
procedure TGame.Shutdown;
begin
  Timer.Free;
  Images.Free;
  Fonts.Free;
  Canvas.Free;
  Device.Free;
  Input.Free;
end;




end.
