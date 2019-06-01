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
  // Font classes
  phxFont;

type

TGame = class(TPHXApplication)
  private
    Device : TPHXDevice;
    Canvas : TPHXCanvas;
    Fonts  : TPHXFontList;
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
  Device.Window.Icon:= ContentPath + 'Phoenix.bmp';
  // Initialize the window with a width of 800 and a height of 600 pixels
  Device.Initialize('Phoenix Demo', 800, 600);;

  // Create the canvas using the device factory function
  Canvas:= Device.CreateCanvas;

  // Create the font list
  Fonts:= TPHXFontList.Create(Device, Canvas);
  // Load a gont
  Fonts.LoadFont('Tahoma.phxfnt');
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

  // Draw a simple font
  Fonts[0].TextOut(4, 4, 'Hello world');
  // Draw gradient text
  Fonts[0].TextOut(4, 24, 'Hello world', clrSilver, clrMaroon);

  // Lines with the correct spacing
  Fonts[0].TextOut(200, 4 + Fonts[0].Height * 0, 'Line 1');
  Fonts[0].TextOut(200, 4 + Fonts[0].Height * 1, 'Line 2');
  Fonts[0].TextOut(200, 4 + Fonts[0].Height * 2, 'Line 3');

  // Draw text that is aligned to the top right corner of the device window
  Fonts[0].TextOut(Device.Bounds, 'Aligned text', taTopRight, clrGreen);
  // Draw text that is aligned to the bottom left corner of the device window
  Fonts[0].TextOut(Device.Bounds, 'Aligned text', taBottomLeft, clrTeal);

  // Draw a selection rectangle for a text
 // Fonts[0].DrawSelection(4, 120,  'The quick brown fox jumps'#13'over the lazy dog.', 5, 11, clrNavy);
  // Draw the text above the selection rectangle
  //Fonts[0].TextOut(4, 120, 'The quick brown fox jumps'#13'over the lazy dog.');

  // Flush the canvas
  Canvas.Flush;

  // Flip the front and back buffers to show the scene
  Device.Flip;
end;

//------------------------------------------------------------------------------
procedure TGame.Shutdown;
begin
  Fonts.Free;
  Canvas.Free;
  Device.Free;
end;








end.
