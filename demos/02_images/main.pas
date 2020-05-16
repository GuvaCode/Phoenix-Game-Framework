unit main;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  phxTypes,
  phxMath,
  phxDevice,
  phxApplication,
  phxGraphics,
  phxTexture,
  phxCanvas,
  phxImage;

type
TGame = class(TPHXApplication)
  private
    Device : TPHXDevice;
    Timer  : TPHXTimer;
    Canvas : TPHXCanvas;
    Images    : TPHXImageList;
  protected
    procedure KeyPressed(Key: TPHXVirtualKey; Shift: TPHXShiftStates); override;
  public
    constructor Create; override;
    procedure Init; override;
    procedure Update; override;
    procedure Render; override;
    procedure Shutdown; override;
  end; implementation

uses
  phxOpenGL_GLFW3,
  phxGraphics_FreeImage;

constructor TGame.Create;
begin
  inherited;

end;

procedure TGame.Init;
var Image: TPHXImage;
begin
  // Create the device using the default provider
   Device:= TPHXDevice.Create;
   // Initialize the device
   Device.Initialize;
   // Create the timer
   Timer:= TPHXTimer.Create;
   // Create the device
   Canvas:= Device.CreateCanvas;

   Images:= TPHXImageList.Create(Device, Canvas);
   // Load a regular texture
   Images.LoadImage('data/background.png');
   // Load and rename a image
   Images.LoadImage('data/background.png', 'Images/Background');
   // Load a phoenix image containing patterns and tags
   Image:= Images.LoadImage('data/Shooter.phximg');
   // Add a pattern to the image, Name, X, Y, Width, Height, PivotX, PivotY
   Image.Patterns.Add('Enemy', 73, 2, 33, 32, 16, 16);
end;

procedure TGame.Update;
begin
  Timer.Update;
  Device.Update;
end;

procedure TGame.Render;
  var Transform: TMatrix4f;
  begin
    Device.Clear;

    // Draw background
    Images[0].TileDraw(0, 0, 5, 5);

    // Draw the image using the image index and pattern index
    Images[1].Draw(50, 50, 0);
    // Draw the image rotated
    Images[1].DrawRotate(100, 50, 45, 0);
    // Draw the image using the image name
    Images.Find('Shooter').Draw(150, 50, 0);
    // Draw the image using the image name and pattern name
    Images.Find('Shooter').Draw(200, 50, 'Player');
    // Draw the pattern we added manually
    Images.Find('Shooter').Draw(250, 50, 'Enemy');
    // Draws a custom area of the image
    Images.Find('Shooter').DrawArea(250, 250, Recti(40, 4, 40+30, 4+31));

    // If we want some othe drawing effect we can use the DrawTransform function
    // that takes a trans matrix as paraeter. Thus we need to create this first.
    //
    // This matrix scales, rotates and translates the player sprite, note that
    // changing the order of the multiplications will give different results.
    Transform:= Matrix_CreateTranslation(200,200, 0) * Matrix_CreateRotationZ(Timer.ElapsedTime * 45) * Matrix_CreateScale(2,2,1);
    // Draw the player pattern from the shooter image using the transformation matrix.
    Images.Find('Shooter').DrawTransform(Transform, 'Player');

    Canvas.Flush;
    Device.Flip;
end;

procedure TGame.Shutdown;
begin
  Images.Free;
  Canvas.Free;
  Device.Free;
  Timer.Free;
end;

procedure TGame.KeyPressed(Key: TPHXVirtualKey; Shift: TPHXShiftStates);
begin
  inherited;
  // Terminate the application with esc
  if Key = VK_ESC then
  begin
    Terminate;
  end;
end;

end.

