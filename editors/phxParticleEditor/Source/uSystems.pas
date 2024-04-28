unit uSystems;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls,

  phxParticle, Vcl.Menus;

type
  TFrmSystems = class(TFrame)
    lwSystems: TListView;
    PopupMenu1: TPopupMenu;
    EditShowInactive: TMenuItem;
    Showparticles1: TMenuItem;
    procedure EditShowInactiveClick(Sender: TObject);
    procedure Showparticles1Click(Sender: TObject);
  private
    function GetSystems: TPHXParticleSystems;
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;

    procedure lwSystemsUpdate;

    property Systems: TPHXParticleSystems read GetSystems;
  end;

implementation

{$R *.dfm}

uses uActions,
    uDebugParticles;

//------------------------------------------------------------------------------
constructor TFrmSystems.Create(AOwner: TComponent);
begin
  inherited;
  lwSystems.DoubleBuffered := true;
end;

//------------------------------------------------------------------------------
procedure TFrmSystems.EditShowInactiveClick(Sender: TObject);
begin
  lwSystemsUpdate;
end;

//------------------------------------------------------------------------------
procedure TFrmSystems.Showparticles1Click(Sender: TObject);
var Index : Integer;
var System: TPHXParticleSystem;
begin
  Index:= lwSystems.ItemIndex;

  if Assigned(lwSystems.Selected) and (Index >= 0) and (Index < Systems.Count) then
  begin
    System:= TPHXParticleSystem(lwSystems.Selected.Data);

    FrmDebugParticles.System:= System;
    FrmDebugParticles.Show;
  end else
  begin
    FrmDebugParticles.System:= nil;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmSystems.lwSystemsUpdate;
var Count : Integer;
var Index : Integer;
var Item  : TListItem;
var System: TPHXParticleSystem;
var FS    : TFormatSettings;
begin
  FS:= TFormatSettings.Create;
  FS.DecimalSeparator:= '.';

  lwSystems.Items.BeginUpdate;

  if EditShowInactive.Checked then
  begin
    Count:= Systems.Capacity;
  end else
  begin
    Count:= Systems.Count;
  end;

  if lwSystems.Items.Count <> Count then
  begin
    lwSystems.Items.Clear;

    for Index:= 0 to Count - 1 do
    begin
      Item:= lwSystems.Items.Add;
      Item.SubItems.Add('');
      Item.SubItems.Add('');
      Item.SubItems.Add('');
      Item.SubItems.Add('');
      Item.SubItems.Add('');
    end;
  end;

  for Index:= 0 to Count - 1 do
  begin
    System:= Systems[Index];

    Item:= lwSystems.Items[Index];
    Item.Data   := System;
    Item.Caption:= IntToStr(Index);
    Item.SubItems[0]:= System.Name;
    Item.SubItems[1]:= Format('%d'  , [System.Particles.Count], FS) ;
    Item.SubItems[2]:= Format('%.1f', [System.Time           ], FS) ;
    Item.SubItems[3]:= BoolToStr(System.Alive , True);
    Item.SubItems[4]:= BoolToStr(System.Active, True);
  end;

  lwSystems.Items.EndUpdate;
end;

//------------------------------------------------------------------------------
function TFrmSystems.GetSystems: TPHXParticleSystems;
begin
  Result:= ModActions.Manager.Systems;
end;


end.
