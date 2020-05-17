unit uPlayer;

interface

uses SysUtils,

  phxTypes,
  phxMath,
  phxDevice,

  phxGraphics,
  phxInput,

  phxCanvas,
  phxFont,
  phxTexture,
  phxImage,
  phxSprite;

type

//------------------------------------------------------------------------------
TWeapon = class
  public
    procedure Fire; virtual; abstract;
  end;

//------------------------------------------------------------------------------
TPlasmaWeapon = class(TWeapon)

end;

//------------------------------------------------------------------------------
TPlayer = class(TPHXSprite)
  private
    FVelocity: Single;
    FThrust: Single;
    FAfterburner: Single;
    FTarget: TPHXSprite;

    FReload: Single;
    FReloadSec: Single;

    FHardpoint: Boolean;
  public
    constructor Create(AEngine: TPHXSpriteEngine); override;

    procedure Update(const FrameTime: Double; Input: TPHXInput); reintroduce; overload;
    procedure Update(const FrameTime: Double); overload; override;

    procedure Fire;
    procedure FireMissile;

    property Thrust: Single read FThrust write FThrust;
    property Velocity: Single read FVelocity write FVelocity;

    property Target: TPHXSprite read FTarget write FTarget;
  end;

implementation

uses uBullet, uWeapon;

//------------------------------------------------------------------------------
constructor TPlayer.Create(AEngine: TPHXSpriteEngine);
begin
  inherited;
  Name:= 'Player';

  FReloadSec:= 0;

  FAfterburner:= 1.0;
end;

// TPlayer
//------------------------------------------------------------------------------
procedure TPlayer.Update(const FrameTime: Double; Input: TPHXInput);
begin
  FReload:= FReload - FrameTime;

  FReloadSec:= FReloadSec - FrameTime;

  if isLeft in Input.States then
  begin
    RotateLeft(90 * FrameTime);
  end;
  if isRight in Input.States then
  begin
    RotateRight(90 * FrameTime);
  end;
  if isUp in Input.States then
  begin
    FVelocity:= FVelocity + 100 * FrameTime;
  end;
  if isDown in Input.States then
  begin
    FVelocity:= FVelocity - 100 * FrameTime;

    if FVelocity < -100 then FVelocity:= -100 ;
  end else
  if isButton3 in Input.States then
  begin
    FVelocity:= 500;
  end;
  // Fire a bullet
  if isButton1 in Input.States then
  begin
   // Input.States:= Input.States - [isButton1];

    if FReload < 0 then
    begin
      Fire;

      FReload:= 0.250;
    end;
  end;
  // Fire a missile
  if (isButton2 in Input.States) and (Target <> Self) then
  begin
   // Input.States:= Input.States - [isButton1];

    if FReloadSec < 0 then
    begin
      FireMissile;

      FReloadSec:= 0.500;
    end;
  end;

  if FVelocity > 100 then FVelocity:= FVelocity - 1000 * FrameTime;
end;

//------------------------------------------------------------------------------
procedure TPlayer.Update(const FrameTime: Double);
begin
  inherited;

  MoveForward(FVelocity * FrameTime);
end;

//------------------------------------------------------------------------------
procedure TPlayer.Fire;
var Bullet: TBullet;
begin
  Bullet:= TBullet.Create(Engine);
  Bullet.Name := 'Bullet';
  // Find and sets the image and pattern from the engine image list
  Bullet.Image:='Images';
  Bullet.Pattern:= 'Bullet';
  // The bullet image is larger then the wanted collision box, resize it
  Bullet.Bounds:= Rectf(-4,-4,4,4);

  Bullet.Collider := True;
  Bullet.Parent:= Engine.Root;

  // Attatch the bullet to the Muzzle tag of the turret
  Bullet.AttatchTo(Self, 'Hardpoint 1');


  Bullet:= TBullet.Create(Engine);
  Bullet.Name := 'Bullet';
  // Find and sets the image and pattern from the engine image list
  Bullet.Image:='Images';
  Bullet.Pattern:= 'Bullet';
  // The bullet image is larger then the wanted collision box, resize it
  Bullet.Bounds:= Rectf(-4,-4,4,4);

  Bullet.Collider := True;
  Bullet.Parent:= Engine.Root;

  // Attatch the bullet to the Muzzle tag of the turret
  Bullet.AttatchTo(Self, 'Hardpoint 2');
end;

//------------------------------------------------------------------------------
procedure TPlayer.FireMissile;
var Missile: TMissile;
begin
  Missile:= TMissile.Create(Engine);
  Missile.Name := 'Missile';
  // Find and sets the image and pattern from the engine image list
  Missile.Image:='Images';
  Missile.Pattern:= 'Missile';
  // The bullet image is larger then the wanted collision box, resize it
  Missile.Bounds:= Rectf(-4,-4,4,4);

  Missile.Collider := True;
  Missile.Parent:= Engine.Root;

  Missile.Target:= Target;

  if FHardpoint then
  begin
    // Attatch the bullet to the Muzzle tag of the turret
    Missile.AttatchTo(Self, 'Hardpoint 1');
  end else
  begin
    // Attatch the bullet to the Muzzle tag of the turret
    Missile.AttatchTo(Self, 'Hardpoint 2');
  end;

  FHardpoint:= not FHardpoint;
end;

end.
