////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//    Phoenix Game Framework                                                  //
//                                                                            //
//    http://www.phoenixlib.net                                               //
//                                                                            //
//    The contents of this file are used with permission, subject to          //
//    the Mozilla Public License Version 1.1 (the "License"); you may         //
//    not use this file except in compliance with the License. You may        //
//    obtain a copy of the License at                                         //
//    http://www.mozilla.org/MPL/MPL-1.1.html                                 //
//                                                                            //
//    Software distributed under the License is distributed on an             //
//    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or          //
//    implied. See the License for the specific language governing            //
//    rights and limitations under the License.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
unit Main;

interface

// This includes the phoenix configuration file
{$I phxConfig.inc}

uses SysUtils,
  // Basic phoenix types
  phxTypes,
  // Contains phoenix utility classes
  phxClasses,
  // Math functions
  phxMath,
  // Device
  phxDevice,
  // Contains the application framework
  phxApplication,
  // Used for loading and manipulating graphic images
  phxGraphics,
  // Contains the canvas class for rendering 2D primitives
  phxCanvas,
  // Texture classes
  phxTexture,
  // Image classes
  phxImage,
  // Input framework
  phxInput;

type

TGame = class(TPHXApplication)
  private
    Device: TPHXDevice;
    Timer : TPHXTimer;
    Canvas: TPHXCanvas;
    Images: TPHXImageList;
    Input : TPHXInput;

    PlayerPosition: TVector2f;
  public
    procedure Init; override;
    procedure Update; override;
    procedure Render; override;
    procedure Shutdown; override;
  end;

implementation

uses
  phxOpenGL_SDL,

  phxGraphics_FreeImage;

//------------------------------------------------------------------------------
procedure TGame.Init;
begin
  // Creates the device with the renderer from phxProvider_OpenGL_SDL.pas
  Device:= TPHXDevice.Create;
  // This loads a new icon for the window
  Device.Window.Icon:= ContentPath + 'Phoenix.bmp';
  // Initialize the window with a width of 800 and a height of 600 pixels
  Device.Initialize('Phoenix Demo', 800, 600);

  // Create the timer
  Timer:= TPHXTimer.Create;

  // Create the canvas using the device factory function
  Canvas:= Device.CreateCanvas;

  // Create the image list
  Images:= TPHXImageList.Create(Device, Canvas);
  // Load the image, this can both open normal textures and phoenix images
  Images.LoadImage('Shooter.phximg');

  // Create the input
  Input:= TPHXInput.Create;

  // Remove all bindings from a state
  Input.Bindings.Remove(isButton3);
  // Bind a key to a state
  Input.Bindings.Add(isButton3, VK_F2);

  // Save the bindings to a file
  Input.SaveBindings('input.txt');
  // Load the bindings from a file
  Input.LoadBindings('input.txt');

  // Reset the player position
  PlayerPosition.X:= 200;
  PlayerPosition.Y:= 200;
end;

//------------------------------------------------------------------------------
procedure TGame.Update;
begin
  // Update the device
  Device.Update;
  // Update the timer
  Timer.Update;
  // Update the input
  Input.Update;

  // Move the player to the left with a velocity of 200 pixels per second
  if isLeft in Input.States then
  begin
    PlayerPosition.X:= PlayerPosition.X - 200 * Timer.FrameTime;
  end;

  // Move the player to the left with a velocity of 200 pixels per second
  if isRight in Input.States then
  begin
    PlayerPosition.X:= PlayerPosition.X + 200 * Timer.FrameTime;
  end;

  // Check the keyboard for a key
  if Input.Keyboard[VK_TAB] then
  begin
    PlayerPosition.X:= Input.Mouse.X;
    PlayerPosition.Y:= Input.Mouse.Y;
  end;

  // Do something when the left mouse button is pressed
  if isButton1 in Input.Mouse.States then
  begin
  end;

  // Only perform a action when a button is pressed
  if isButton1 in Input.States then
  begin
    // TODO: Fire bullet

    // Remove the state
    Input.States:= Input.States - [isButton1];
  end;

end;

//------------------------------------------------------------------------------
procedure TGame.Render;
begin
  // Clear the back buffer
  Device.Clear;

  // Draw the image using the image index and pattern index
  Images[0].Draw(PlayerPosition.X, PlayerPosition.Y, 0);

  // Flush the canvas
  Canvas.Flush;

  // Flip the front and back buffers to show the scene
  Device.Flip;
end;

//------------------------------------------------------------------------------
procedure TGame.Shutdown;
begin
  Timer.Free;
  Images.Free;
  Input.Free;
  Canvas.Free;
  Device.Free;
end;






end.
