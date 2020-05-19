unit main;

{$mode delphi}{$H+}

interface


uses SysUtils,

  phxTypes,
  phxMath,
  phxDevice,
  phxApplication,
  phxGraphics,
  phxTexture,
  phxCanvas,
  phxImage;

type

//------------------------------------------------------------------------------
TGame = class(TPHXApplication)
  private
    Device    : TPHXDevice;
    Timer     : TPHXTimer;
    Canvas    : TPHXCanvas;
    Images    : TPHXImageList;
  public
    procedure Init; override;
    procedure Update; override;
    procedure Render; override;
    procedure Shutdown; override;
  end;


implementation

uses
   phxOpenGL_GLFW3,
   phxOpenGL_SDL,
   phxGraphics_FreeImage;

//------------------------------------------------------------------------------
procedure TGame.Init;
var Image: TPHXImage;
begin
  // Create the device using the default provider
  Device:= TPHXDevice.Create;

  Device.Display.Width:= 800;
  Device.Display.Height:= 600;

  // Initialize the device
  Device.Initialize;
  // This loads a new icon for the window
  Device.Icon:= ContentPath + 'Phoenix.bmp';

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

//------------------------------------------------------------------------------
procedure TGame.Update;
begin
  Timer.Update;

  Device.Update;
end;

//------------------------------------------------------------------------------
procedure TGame.Render;
var Scale: TVector3f;

begin
  // Scale all content to a virtual screen size of 1027 x 768


  Scale.X:= Device.Width  / 1024;
  Scale.Y:= Device.Height / 768;

  Canvas.Transform:= Matrix_CreateScale(Scale.X, Scale.Y, 1);

  // Draw background
  Images[0].TileDraw(0, 0, 20, 20);

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

  Canvas.Flush;


  Device.Flip;
end;

//------------------------------------------------------------------------------
procedure TGame.Shutdown;
begin
  Images.Free;
  Canvas.Free;
  Device.Free;
  Timer.Free;
end;




end.

