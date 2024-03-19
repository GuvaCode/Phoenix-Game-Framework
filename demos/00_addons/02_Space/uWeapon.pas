unit uWeapon;

interface

uses SysUtils,

  phxInput,
  phxSprite;
type

//------------------------------------------------------------------------------
TMissile = class(TPHXSprite)
  private
    FTarget: TPHXSprite;
  public
    procedure Update(const Delta: Double); override;

    procedure Collided(Sprite: TPHXSprite); override;

    property Target: TPHXSprite read FTarget write FTarget;
  end;

implementation

// TBullet
//------------------------------------------------------------------------------
procedure TMissile.Collided(Sprite: TPHXSprite);
begin
  inherited;
  Self.Kill;
        {
  // Destroy barrel and the bullet
  if(Sprite is TBarrel) then
  begin
    Self.Kill;

    Sprite.Kill;
  end;
  // Crates are indestructable, only destroy the bullet
  if(Sprite is TCrate) then
  begin
    Self.Kill;
  end;  }

end;

//------------------------------------------------------------------------------
procedure TMissile.Update(const Delta: Double);
begin
  inherited;

  if Assigned(FTarget) then
  begin
    MoveTowards(Delta, Target.Position, 200, 90);
  end else
  begin
    try
     MoveForward(200 * Delta);
    except
      writeln(200 * Delta);
    end;


  end;


  // Kill the missile after three seconds
  if Time >= 3 then
  begin
    Kill;
  end;
end;

end.
