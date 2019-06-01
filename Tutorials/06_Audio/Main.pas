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
  // Font class
  phxFont,
  // Input framework
  phxInput,
  // Audio classes
  phxAudio,
  // Audio device using the bass provider
  phxAudio_Bass;

type

TGame = class(TPHXApplication)
  private
    Device: TPHXDevice;
    Timer : TPHXTimer;
    Canvas: TPHXCanvas;
    Fonts : TPHXFontList;
    Input : TPHXInput;
    Audio : TPHXAudioEngine;
    Music : TPHXAudioStream;
  protected
    procedure KeyPressed(Key: TPHXVirtualKey; Shift: TPHXShiftStates); override;
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

// http://www.freesound.org/people/Zangrutz/sounds/155235/
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

  Fonts:= TPHXFontList.Create(Device, Canvas);
  Fonts.LoadFont('Calibri12.phxfnt');

  // Create the input
  Input:= TPHXInput.Create;

  // Create the audio engine using the included BASS provider
  Audio:= TPHXAudioEngine.Create;

  // Initialize the audio engine using the default audio device
  Audio.Initialize;
  // Initialize the audio engine using a audio device by  index
  //Audio.Initialize(2);
  // Initialize the audio engine using a audio device by name
  //Audio.Initialize('SPDIF Out (Creative SB X-Fi)');

  // Load the bomb sample
  Audio.LoadSample('Bomb.mp3', 'Bomb');
  // Load the missile sample
  Audio.LoadSample('Missile.wav', 'Missile');

  // Streams are streamed from disk and are used for music
  Music:= Audio.LoadStream('Music.wav', 'Music');
  // We want to loop the music
  Music.Looped:= True;
  // Set the volume of the music from 0.0 (silent) to 1.0 (full)
  Music.Volume:= 0.5;
  // Play the music
  Music.Play;
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
  // Update the audio engine
  Audio.Update;
   // Play the bomb sample with the left mouse button
  if isButton1 in Input.States then
  begin
    Audio.Play('Bomb');

    // Remove the state
    Input.States:= Input.States - [isButton1];
  end;
  // Play the missile sample the right mouse button
  if isButton2 in Input.States then
  begin
    Audio.Play('Missile');

    // Remove the state
    Input.States:= Input.States - [isButton2];
  end;
  // Play the missile sample at 50% volume with the right mouse button
  if isButton3 in Input.States then
  begin
    Audio.Play('Missile', 0.5);

    // Remove the state
    Input.States:= Input.States - [isButton3];
  end;
  // Raise the volume of the music
  if isUp in Input.States then
  begin
    Music.Volume:= Music.Volume + 0.2 * Timer.FrameTime;

    if Music.Volume > 1 then Music.Volume:= 1;
  end;
  // Lower the volume of the music
  if isDown in Input.States then
  begin
    Music.Volume:= Music.Volume - 0.2 * Timer.FrameTime;

    if Music.Volume < 0 then Music.Volume:= 0;
  end;
end;


//------------------------------------------------------------------------------
procedure TGame.Render;
var Index: Integer;
begin
  // Clear the back buffer
  Device.Clear;


  Fonts[0].TextOut(4, 4 + Fonts[0].Height * 0, Format('Music volume: %d %%', [ Round(Music.Volume*100)]));
  Fonts[0].TextOut(4, 4 + Fonts[0].Height * 1, Format('Music position: %.1f s', [Music.Position]));
  Fonts[0].TextOut(4, 4 + Fonts[0].Height * 2, Format('Music duration: %.1f s', [Music.Duration]));

  for Index:=0 to Audio.Devices.Count-1 do
  begin
    Fonts[0].TextOut(400, 4 + Fonts[0].Height * Index, Format('%d: %s', [Index, Audio.Devices[Index].Name]));
  end;

  // Flush the canvas
  Canvas.Flush;

  // Flip the front and back buffers to show the scene
  Device.Flip;
end;

//------------------------------------------------------------------------------
procedure TGame.KeyPressed(Key: TPHXVirtualKey; Shift: TPHXShiftStates);
begin
  inherited;

  // Change the master volume to 50% when pressing 1 on the keyboard
  if Key = VK_1 then
  begin
    Audio.Volume:= 0.5;
  end;
  // Change the master volume to 100% when pressing 2 on the keyboard
  if Key = VK_2 then
  begin
    Audio.Volume:= 1.0;
  end;
  // Change the volume of all playing instances the missile sample
  if Key = VK_3 then
  begin
    Audio.Samples[1].Volume:= 0.1;
  end;
end;

//------------------------------------------------------------------------------
procedure TGame.Shutdown;
begin
  Audio.Free;

  Timer.Free;
  Fonts.Free;
  Input.Free;
  Canvas.Free;
  Device.Free;
end;






end.
