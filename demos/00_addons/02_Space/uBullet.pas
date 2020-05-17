unit uBullet;

interface

uses SysUtils,

  phxInput,
  phxSprite;

type

//------------------------------------------------------------------------------
TBullet = class(TPHXSprite)
  public
    procedure Update(const Delta: Double); override;

    procedure Collided(Sprite: TPHXSprite); override;
  end;


implementation

// TBullet
//------------------------------------------------------------------------------
procedure TBullet.Collided(Sprite: TPHXSprite);
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
procedure TBullet.Update(const Delta: Double);
begin
  inherited;

  MoveForward(500 * Delta);

  // Kill the bullet after two seconds
  if Time >= 2 then
  begin
    Kill;
  end;
end;



end.
