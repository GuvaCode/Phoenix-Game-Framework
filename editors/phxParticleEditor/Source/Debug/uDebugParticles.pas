unit uDebugParticles;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls,

  phxParticle, Vcl.ExtCtrls;

type
  TFrmDebugParticles = class(TForm)
    lwParticles: TListView;
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
  private
    FSystem: TPHXParticleSystem;
    procedure SetSystem(const Value: TPHXParticleSystem);
  public
    procedure lwParticlesUpdate;

    property System: TPHXParticleSystem read FSystem write SetSystem;
  end;

var
  FrmDebugParticles: TFrmDebugParticles;

implementation

{$R *.dfm}

{ TForm1 }

procedure TFrmDebugParticles.lwParticlesUpdate;
var Count   : Integer;
var Index   : Integer;
var Item    : TListItem;
var Particle: TPHXParticle;
var FS      : TFormatSettings;
begin
  FS:= TFormatSettings.Create;
  FS.DecimalSeparator:= '.';

  lwParticles.Items.BeginUpdate;

 // if EditShowInactive.Checked then
 // begin
    Count:= System.Particles.Capacity;
 // end else
  //begin
 //   Count:= Systems.Count;
 // end;

  if lwParticles.Items.Count <> Count then
  begin
    lwParticles.Items.Clear;

    for Index:= 0 to Count - 1 do
    begin
      Item:= lwParticles.Items.Add;
      Item.SubItems.Add('');
      Item.SubItems.Add('');
      Item.SubItems.Add('');
      Item.SubItems.Add('');
      Item.SubItems.Add('');
    end;
  end;

  for Index:= 0 to Count - 1 do
  begin
    Particle:= System.Particles.List^[Index];

    Item:= lwParticles.Items[Index];
    Item.Caption:= IntToStr(Index);
    Item.SubItems[0]:= Format('%.1f', [Particle.Time           ], FS) ;
    Item.SubItems[1]:= Format('%.1f', [Particle.Life           ], FS) ;
    Item.SubItems[2]:= Format('%.1f', [Particle.Energy        ], FS) ;
    Item.SubItems[3]:= Format('%.1f', [Particle.Size         ], FS) ;
    Item.SubItems[4]:= Format('%.1f', [Particle.Color.Alpha           ], FS) ;
   // Item.SubItems[4]:=  '';
    //Item.SubItems[4]:=  '';
  end;

  lwParticles.Items.EndUpdate;
end;

procedure TFrmDebugParticles.SetSystem(const Value: TPHXParticleSystem);
begin
  if FSystem <> Value then
  begin
    FSystem := Value;

    Timer1.Enabled:= Assigned(FSystem);


  end;
end;

//------------------------------------------------------------------------------
procedure TFrmDebugParticles.Timer1Timer(Sender: TObject);
begin
  if Assigned(FSystem) then
  begin
    lwParticlesUpdate;
  end;
end;

end.
