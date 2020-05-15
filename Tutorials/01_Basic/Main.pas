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

  phxTypes,// Basic phoenix types

  phxClasses,// Contains phoenix utility classes
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
  phxTexture;

type

TGame = class(TPHXApplication)
  private
    Device : TPHXDevice;
    Canvas : TPHXCanvas;
    Texture: TPHXTexture;
  public
    procedure Init; override;
    procedure Update; override;
    procedure Render; override;
    procedure Shutdown; override;
  end;

implementation

uses

  phxOpenGL_GLFW3,   //libglfw.so.3
  phxGraphics_FreeImage;  //libfreeimage.so.3

//------------------------------------------------------------------------------
procedure TGame.Init;
begin
  // Creates the device with the renderer from phxProvider_OpenGL_SDL.pas
  Device:= TPHXDevice.Create;
  // This loads a new icon for the window
  Device.Icon:= ContentPath + 'Phoenix.bmp';
  // Initialize the window with a width of 800 and a height of 600 pixels
  Device.Initialize('Phoenix Demo', 800, 600);;

  // Create the canvas using the device factory function
  Canvas := Device.CreateCanvas;
  // Create the texture using the device factory function
  Texture:= Device.CreateTexture;
  // Load the texture from a file
  Texture.LoadTexture('mud.png');
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

  // Change the color to white
  Canvas.Color:= clrWhite;
  // Draw a rectangle
  Canvas.Rectangle(100, 100, 200, 200);
  // Change the color to red
  Canvas.Color:= clrRed;
  // Draw a rectangle
  Canvas.Rectangle(300, 100, 400, 200);

  // Change back to white color
  Canvas.Color:= clrWhite;
  // Bind the texture
  Canvas.Texture:= Texture;
  // Draw a filled rectangle
  Canvas.FilledRectangle(100, 300, 200, 400);
  // Remove the texture
  Canvas.Texture:= nil;

  // Flush the canvas
  Canvas.Flush;

  // Flip the front and back buffers to show the scene
  Device.Flip;
end;

//------------------------------------------------------------------------------
procedure TGame.Shutdown;
begin
  Texture.Free;
  Canvas.Free;
  Device.Free;
end;






end.
