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
  // Contains the canvas class for rendering 2D primitives
  phxCanvas,
  // Image classes
  phxImage,
  // Bitmap font
  phxFont,
  // Input framework
  phxInput,
  // Shapes for collision testing
  phxShape,
  // Sprite engine
  phxSprite;

type

// http://phoenixlib.net/wiki/doku.php?id=tutorial:collision
//------------------------------------------------------------------------------
TGame = class(TPHXApplication)
  private
    Device   : TPHXDevice;
    Canvas   : TPHXCanvas;
    Timer    : TPHXTimer;
    Input    : TPHXInput;
    Images   : TPHXImageList;
    Fonts    : TPHXFontList;
    Shapes   : TPHXShapeList;
    Sprites  : TPHXSpriteEngine;
    Player   : TPHXSprite;

    Cooldown: Single;

    procedure CreateSprites;

    procedure Fire;
  public
    procedure Init; override;
    procedure Update; override;
    procedure Render; override;
    procedure Shutdown; override;
  end;

TBullet = class(TPHXSprite)
  public
    procedure Update(const DeltaTime: Double); override;
    procedure Collided(Sprite: TPHXSprite); override;
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

  // Create the canvas
  Canvas:= Device.CreateCanvas;

  // Create the timer
  Timer:= TPHXTimer.Create;
  // Create the input
  Input:= TPHXInput.Create;

  // Create the image list
  Images:= TPHXImageList.Create(Device, Canvas);
  // Load the image, this can both open normal textures and phoenix images
  Images.LoadImage('Shooter.phximg');

  // Create the font list
  Fonts:= TPHXFontList.Create(Device, Canvas);
  // Load a gont
  Fonts.LoadFont('Tahoma.phxfnt');

  // Create the shape list
  Shapes:= TPHXShapeList.Create;
  // Load the shapes from disk
  Shapes.LoadFromFile('Shapes.shapes');

  // Create the sprites
  CreateSprites;
end;

//------------------------------------------------------------------------------
procedure TGame.CreateSprites;
var Index   : Integer;
var Asteroid: TPHXSprite;
begin
  // Create the sprite engine
  Sprites:= TPHXSpriteEngine.Create(Device);
  Sprites.Images:= Images;
  Sprites.Shapes:= Shapes;

  // Create the asteroid sprites
  for Index := 1 to 10 do
  begin
    Asteroid:= TPHXSprite.Create(Sprites);
    Asteroid.Name    := 'Asteroid ' + IntToStr(Index);
    Asteroid.X       := 20 + Random * (Device.Width  - 40);
    Asteroid.Y       := 20 + Random * (Device.Height - 40);
    Asteroid.Image   := 'Shooter';
    Asteroid.Shape   := 'Asteroid';
    Asteroid.Pattern := 'Asteroid';
    Asteroid.Collider:= True;
    Asteroid.Parent  := Sprites.Root;

    // Mark asteroids as static
    Asteroid.Mode := cmStatic;
    // Make the asteroids belong to group 2
    Asteroid.Group:= cgGroup2;
  end;

  // Create the player sprite
  Player:= TPHXSprite.Create(Sprites);
  Player.Name    := 'Player';
  Player.X       := 100;
  Player.Y       := 100;
  Player.Image   := 'Shooter';
  Player.Shape   := 'Player';
  Player.Pattern := 'Player';
  Player.Collider:= True;
  Player.Parent  := Sprites.Root;

  // Mark the player as dynamic (this is the default value)
  Player.Mode := cmDynamic;
  // Make the player belong to group 1
  Player.Group:= cgGroup1;

  // Initialize the sprite engine
  Sprites.Initialize;
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
   // Update the sprite engine
  Sprites.Update(Timer.FrameTime);

  // Rotate left with 90 degrees per second
  if isLeft in Input.States then
  begin
    Player.RotateLeft(90 * Timer.FrameTime);
  end;
  // Rotate rightwith 90 degrees per second
  if isRight in Input.States then
  begin
    Player.RotateRight(90 * Timer.FrameTime);
  end;
  // Move forward along the rotation with 200 pixels per second
  if isUp in Input.States then
  begin
    Player.MoveForward(200 * Timer.FrameTime);
  end;
  // Move backward along the rotation with 200 pixels per second
  if isDown in Input.States then
  begin
    Player.MoveBackward(200 * Timer.FrameTime);
  end;
  // Making the sprite engine follow the player
  Sprites.Camera.CenterOn(Player);

  //if isButton1 in Input.States then
  //begin
  //  Fire;
  //  Input.States:= Input.States - [isButton1];
  //end;

  Cooldown:= Cooldown - Timer.FrameTime;
  if (isButton1 in Input.States) and (Cooldown < 0) then
  begin
    Fire;
    // We must wait 0.2 seconds before fireing again
    Cooldown:= 0.2;
  end;

end;

//------------------------------------------------------------------------------
procedure TGame.Render;
var Collisions: TPHXSpriteCollisionList;
var Index     : Integer;
begin
  // Clear the back buffer
  Device.Clear;

  // Render the sprites
  Sprites.Render;
  // Render the shapes for the sprites
 // Sprites.RenderShapes(Canvas, clrWhite);
  // Render the bounding boxes for the sprites
  //Sprites.RenderBounds(Canvas, clrGray);

  Collisions:= TPHXSpriteCollisionList.Create;
  try
    Sprites.Collide(Player, Collisions);

    // Only collide the player agianst asteroids
    Sprites.Collide(Player, Collisions, cgGroup2);

    for Index:= 0 to Collisions.Count-1 do
    begin
      Fonts[0].TextOut(4,4 + Fonts[0].Height * Index, Collisions[Index].B.Name);
    end;
  finally
    Collisions.Free;
  end;

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

//------------------------------------------------------------------------------
procedure TGame.Fire;
var Bullet: TPHXSprite;
begin
  // Create the bullet sprite for the first hardpoint
  Bullet:= TBullet.Create(Sprites);
  Bullet.Name    := 'Bullet';
  Bullet.Image   := 'Shooter';
  Bullet.Pattern := 'Bullet';
  Bullet.Shape   := 'Bullet';
  Bullet.Collider:= True;
  Bullet.Parent  := Sprites.Root;

  // Attatch the first bullet to the "Hardpoint1" tag of the image
  Bullet.AttatchTo(Player, 'Hardpoint1');

  // Create the bullet sprite for the second hardpoint
  Bullet:= TBullet.Create(Sprites);
  Bullet.Name    := 'Bullet';
  Bullet.Image   := 'Shooter';
  Bullet.Pattern := 'Bullet';
  Bullet.Shape   := 'Bullet';
  Bullet.Collider:= True;
  Bullet.Parent  := Sprites.Root;

  // Attatch the second bullet to the "Hardpoint2" tag of the image
  Bullet.AttatchTo(Player, 'Hardpoint2');
end;


// Update the bullet
//------------------------------------------------------------------------------
procedure TBullet.Update(const DeltaTime: Double);
begin
  inherited;
  // Move forward with 400 pixels per second
  MoveForward(250 * DeltaTime);

  // Kill the bullet after one second 
  if Time > 1 then Kill;
end;

//------------------------------------------------------------------------------
procedure TBullet.Collided(Sprite: TPHXSprite);
begin
  inherited;

  // We only have asteroids the player and bullets in the sprite list so when we have collided
  // with something other then the player its safe to assume its a asteroid we have collided with.
  // A better way is to make a class for asteroids and test with if Sprite is TAsteroid instead.
  if(Sprite.Name  <> 'Player') then
  begin
    // Kill the bullet
    Kill;
    // Kill the asteroid
    Sprite.Kill;
  end;
end;


end.
