unit main;

{$mode delphi}{$H+}
uses SysUtils,

  phxTypes,
  phxMath,
  phxDevice,
  phxGraphics,
  phxCanvas,
  phxApplication,
  phxInput,
  phxFont,
  phxImage,
  phxShape;

type

TGame = class(TPHXApplication)
  private
    Device : TPHXDevice;
    Timer  : TPHXTimer;
    Input  : TPHXInput;
    Canvas : TPHXCanvas;
    Fonts  : TPHXFontList;
    Images : TPHXImageList;

    Shapes  : TPHXShapeList;
    Hover   : TPHXShape;
    Selected: TPHXShape;

    function CollisionTest: Integer;

    procedure CreateShapes;

    procedure RenderBackground;
    procedure RenderShapes;
  protected
    procedure MouseMoved(X, Y: Integer; Shift: TPHXShiftStates); override;
    procedure MousePressed(X, Y: Integer; Shift: TPHXShiftStates; Button: TPHXMouseButton); override;
  public
    procedure Init; override;
    procedure Update; override;
    procedure Render; override;
    procedure Shutdown; override;
  end;


implementation

uses
    phxOpenGL_GLFW3,
  phxGraphics_Vampyre;

//------------------------------------------------------------------------------
function GetShapeName(const Shape: TPHXShape): String;
begin
  if Assigned(Shape) then
  begin
    Result:= Shape.Name;
  end else
  begin
    Result:= 'none';
  end;
end;

//------------------------------------------------------------------------------
procedure TGame.Init;
begin
  Device:= TPHXDevice.Create;
  Device.Initialize('Phoenix Demo', 800, 600);;



  Timer:= TPHXTimer.Create;
  Input:= TPHXInput.Create;

  // Create our canvas
  Canvas:= Device.CreateCanvas;

  // Load some images
  Images:= TPHXImageList.Create(Device, Canvas);
  Images.LoadImage('data/background.png', 'Images/Background');

  Fonts:= TPHXFontList.Create(Device, Canvas);
  Fonts.LoadFont('data/calibri12.phxfnt');

  CreateShapes;
end;

//------------------------------------------------------------------------------
procedure TGame.Update;
var Delta: Single;
begin
  Timer.Update;
  Input.Update;

  Delta:= Timer.FrameTime * 100;

  if Assigned(Selected) then
  begin
    // Move the player shape
    if isUp    in Input.States then Selected.Translate( Vector2f(0      , - Delta) );
    if isDown  in Input.States then Selected.Translate( Vector2f(0      ,   Delta) );
    if isLeft  in Input.States then Selected.Translate( Vector2f(- Delta, 0      ) );
    if isRight in Input.States then Selected.Translate( Vector2f(  Delta, 0      ) );
  end;

  Device.Update;
end;

//------------------------------------------------------------------------------
procedure TGame.Render;
begin
  Device.Clear;

  RenderBackground;

  RenderShapes;

  CollisionTest;

  Fonts[0].TextOut(2, 2 + 0 * Fonts[0].Height, Format('%d fps', [Timer.FrameRate]), clrMaroon, clrWhite);

  Fonts[0].TextOut(2, 2 + 1 * Fonts[0].Height, Format('Hover: %s', [GetShapeName(Hover)]), clrMaroon, clrWhite);
  Fonts[0].TextOut(2, 2 + 2 * Fonts[0].Height, Format('Selected: %s', [GetShapeName(Selected)]), clrMaroon, clrWhite);

  Canvas.Flush;

  Device.Flip;
end;

//------------------------------------------------------------------------------
procedure TGame.Shutdown;
begin
  Timer.Free;
  Images.Free;
  Canvas.Free;
  Device.Free;
end;

//------------------------------------------------------------------------------
procedure TGame.RenderBackground;
var Image: TPHXImage;
begin
  Image:= Images.Find('Images/Background');

  Image.TileDraw(0,0, Trunc(Device.Width / Image.Width) + 1, Trunc(Device.Height / Image.Height) + 1 );
end;

//------------------------------------------------------------------------------
procedure TGame.RenderShapes;
var Index    : Integer;
var Shape    : TPHXShape;
begin
  Canvas.Texture:= nil;

  // Render shapes
  for Index := 0 to Shapes.Count - 1 do
  begin
    Shape:= Shapes.List^[Index];

    if Shape = Selected then
    begin
      Canvas.Color:= clrRed;
    end else
    begin
      Canvas.Color:= clrWhite;
    end;

    Shape.Render(Canvas, Vector2f_Zero);
  end;
end;

//------------------------------------------------------------------------------
function TGame.CollisionTest: Integer;
var Index: Integer;
var Shape: TPHXShape;
begin
  Result:= 0;

  if Selected = nil then Exit;

  // Test collisions between the selected shape and the shapes
  for Index := 0 to Shapes.Count - 1 do
  begin
    Shape:=Shapes.List^[Index];

    if Shape = Selected then Continue;

    // Collide the player with the current shape, the Vector2f(0,0) is the
    // relative displacement between the objects, as it is zero we are only
    // performing static intersection test
    if TPHXShape.Collide(Selected, Shape, Vector2f(0,0)) then
    begin
      Fonts[0].TextOut(2, 2 + (Result + 3) * Fonts[0].Height, Format('Collided with %s', [Shape.Name]), clrMaroon, clrWhite);

      Inc(Result);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TGame.MouseMoved(X, Y: Integer; Shift: TPHXShiftStates);
begin
  Hover:= Shapes.ShapeAt(X,Y);
end;

//------------------------------------------------------------------------------
procedure TGame.MousePressed(X, Y: Integer; Shift: TPHXShiftStates; Button: TPHXMouseButton);
begin
  if Button = mbLeft then
  begin
    Selected:= Shapes.ShapeAt(X,Y);
  end;
end;

//------------------------------------------------------------------------------
procedure TGame.CreateShapes;
begin
  Shapes:= TPHXShapeList.Create;

  // Add a bos as the player
  Selected:= Shapes.AddBox('Player');
  with TPHXBox(Selected) do
  begin
    Width   := 50;
    Height  := 50;
    Center  := Vector2f(100, 100);
  end;

  with Shapes.AddBox('Box1') do
  begin
    Width   := 50;
    Height  := 50;
    Center  := Vector2f(200, 100);
  end;

  // Here we ad another box, and then we translate it
  with Shapes.AddBox('Box2') do
  begin
    Width   := 50;
    Height  := 50;
    Center  := Vector2f(200, 100);
    Translate( Vector2f(0, 100) );
  end;


  with Shapes.AddPolygon('Polygon1') do
  begin
    // This is a factory function of the polygon that creates a few points
    CreateCircle(50, 3);
    Translate( Vector2f(120, 200) );
  end;

  with Shapes.AddPolygon('Polygon2') do
  begin
    Add(0  , 0  );
    Add(300, 100);
    Add(300, 150);
    Add(0,   150);
    Translate( Vector2f(20, 300) );
  end;

  with Shapes.AddLine('Line1') do
  begin
    Min:= Vector2f(450, 50);
    Max:= Vector2f(400, 250);
  end;

  // A circle collision is tested using a 12-sided polygon, thus it is not
  // fully accurate, but it should be good enough for game applications
  with Shapes.AddCircle('Circle1') do
  begin
    Center:= Vector2f(500, 250);
    Radius:= 25;
  end;

  with Shapes.AddPoint('Point1') do
  begin
    Position:= Vector2f(300, 100);
  end;
end;


end.

