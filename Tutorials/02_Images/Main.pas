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
  // Image classes
  phxImage;

type

TGame = class(TPHXApplication)
  private
    Device : TPHXDevice;
    Canvas : TPHXCanvas;
    Images : TPHXImageList;
  public
    procedure Init; override;
    procedure Update; override;
    procedure Render; override;
    procedure Shutdown; override;
  end;

implementation

uses
  phxOpenGL_GLFW3,

  phxGraphics_FreeImage;

//------------------------------------------------------------------------------
procedure TGame.Init;
begin
  // Creates the device with the renderer from phxProvider_OpenGL_SDL.pas
  Device:= TPHXDevice.Create;
  // This loads a new icon for the window
  Device.Icon:= ContentPath + 'Phoenix.bmp';
  // Initialize the window with a width of 800 and a height of 600 pixels
  Device.Initialize('Phoenix Demo', 800, 600);;

  // Create the canvas
  Canvas:= Device.CreateCanvas;

  // Create the image list
  Images:= TPHXImageList.Create(Device, Canvas);
  // Load the image, this can both open normal textures and phoenix images
  Images.LoadImage('Shooter.phximg');
end;

//------------------------------------------------------------------------------
procedure TGame.Update;
begin
  Device.Update;
end;

//------------------------------------------------------------------------------
procedure TGame.Render;
begin
  // Clear the back buffer
  Device.Clear;

  // Draw the image using the image index and pattern index
  Images[0].Draw(50, 50, 0);
  // Draw the image rotated
  Images[0].DrawRotate(100, 50, 45, 0);
  // Draw the image using the image name
  Images.Find('Shooter').Draw(150, 50, 0);
  // Draw the image using the image name and pattern name
  Images.Find('Shooter').Draw(200, 50, 'Player');

  // Draw the asteroid
  Images.Find('Shooter').Draw(100, 250, 'Asteroid');

  // Flush the canvas
  Canvas.Flush;

  // Flip the front and back buffers to show the scene
  Device.Flip;
end;

//------------------------------------------------------------------------------
procedure TGame.Shutdown;
begin
  Images.Free;
  Canvas.Free;
  Device.Free;
end;






end.
